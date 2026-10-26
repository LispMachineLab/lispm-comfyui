;;; lispm-comfyui-settings.el --- LispM-ComfyUI's settings, in a window  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-comfyui
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LispM-ComfyUI's settings, in a window of their own, which , opens
;; from a workflow: the ComfyUI server it works with, asked at once
;; whether it answers, and what it is; the colours and the font
;; workflows are drawn with.  Each line is a setting: n and p go from
;; one to the next, RET changes the one at point, and its key changes it
;; from anywhere in the window.  What is changed is saved with
;; Customize, for next time, and the workflows open are drawn anew.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'lispm-kit-commands)
(require 'lispm-comfyui-client)

(defvar lispm-comfyui-dark)
(defvar lispm-comfyui-font)
(declare-function lispm-comfyui--canvas-buffers "lispm-comfyui")
(declare-function lispm-comfyui-reload "lispm-comfyui")
(declare-function lispm-comfyui--draw "lispm-comfyui")

(defconst lispm-comfyui-settings-buffer-name "*LispM-ComfyUI Settings*"
  "The name of the settings' buffer.")

(defface lispm-comfyui-settings-heading '((t :inherit bold))
  "Face of the settings' headings."
  :group 'lispm-comfyui)

(defface lispm-comfyui-settings-good '((t :inherit success))
  "Face of what goes well, as a server answering."
  :group 'lispm-comfyui)

(defface lispm-comfyui-settings-bad '((t :inherit error))
  "Face of what goes wrong, as a server not answering."
  :group 'lispm-comfyui)

(defface lispm-comfyui-settings-dim '((t :inherit shadow))
  "Face of what is said of a setting."
  :group 'lispm-comfyui)

(defvar lispm-comfyui-settings--server nil
  "What the server said when last asked: a plist.
:state is `asking', `answered' or `failed'; :url, the address asked;
:version, :device and :types, what it is; :error, what went wrong.")

(defvar-local lispm-comfyui-settings--back nil
  "The buffer the settings were opened from, to go back to.")

;;;; Saving

(defun lispm-comfyui-settings--save (variable value)
  "Set VARIABLE to VALUE and save it with Customize, for next time."
  (customize-save-variable variable value))

(defun lispm-comfyui-settings--redraw-workflows (&optional again)
  "Draw the workflows open anew; with AGAIN, read the server's node types too."
  (dolist (buffer (lispm-comfyui--canvas-buffers))
    (with-current-buffer buffer
      (if again (lispm-comfyui-reload) (lispm-comfyui--draw)))))

;;;; The server

(defun lispm-comfyui-settings--device (stats)
  "What STATS, the server's /system_stats, say of its first device, or nil."
  (when-let* ((devices (alist-get 'devices stats))
              ((and (vectorp devices) (> (length devices) 0)))
              (device (aref devices 0)))
    (let ((name (alist-get 'name device))
          (memory (alist-get 'vram_total device)))
      (concat (if (stringp name)
                  ;; "cuda:0 Tesla V100-SXM3-32GB : cudaMallocAsync"
                  (string-trim (replace-regexp-in-string "\\`[a-z]+:[0-9]+ \\| : .*\\'" "" name))
                "a device")
              (if (and (numberp memory) (> memory 0))
                  (format ", %d GB" (round memory 1073741824))
                "")))))

(defun lispm-comfyui-settings-test ()
  "Ask the server whether it answers, and what it is."
  (interactive)
  (let ((url lispm-comfyui-url))
    (setq lispm-comfyui-settings--server (list :state 'asking :url url))
    (lispm-comfyui-settings--refresh)
    (lispm-comfyui-request
     "/system_stats"
     (lambda (stats error)
       (when (equal url lispm-comfyui-url)
         (if (or error (not (consp stats)))
             (setq lispm-comfyui-settings--server
                   (list :state 'failed :url url :error (or error "it answered nothing ComfyUI says")))
           (setq lispm-comfyui-settings--server
                 (list :state 'answered :url url
                       :version (alist-get 'comfyui_version (alist-get 'system stats))
                       :device (lispm-comfyui-settings--device stats)))
           (lispm-comfyui-types-of-server
            (lambda (types _error)
              (when (and types (equal url lispm-comfyui-url))
                (setq lispm-comfyui-settings--server
                      (plist-put lispm-comfyui-settings--server :types (hash-table-count types)))
                (lispm-comfyui-settings--refresh)))
            t))
         (lispm-comfyui-settings--refresh))))))

(defun lispm-comfyui-settings--address-p (address)
  "Non-nil if ADDRESS is a server's address, http or https, host and port."
  (let ((url (url-generic-parse-url address)))
    (and (member (url-type url) '("http" "https"))
         (not (string-empty-p (or (url-host url) "")))
         (member (url-filename url) '("" "/")))))

(defun lispm-comfyui-settings-address (address)
  "Work with the ComfyUI server at ADDRESS, as http://HOST:PORT."
  (interactive (list (read-string "ComfyUI server's address: " lispm-comfyui-url)))
  (let ((address (string-remove-suffix "/" (string-trim address))))
    (unless (string-match-p "\\`[a-z]+://" address)
      (setq address (concat "http://" address)))
    (unless (lispm-comfyui-settings--address-p address)
      (user-error "A server's address is http://HOST:PORT, as http://127.0.0.1:8188"))
    (lispm-comfyui-close-socket)
    (lispm-comfyui-settings--save 'lispm-comfyui-url address)
    (lispm-comfyui-settings-test)
    (lispm-comfyui-settings--redraw-workflows t)))

(defun lispm-comfyui-settings-open-server ()
  "Open the server's own page, ComfyUI's frontend, in a browser."
  (interactive)
  (browse-url lispm-comfyui-url))

;;;; The look

(defun lispm-comfyui-settings-colours ()
  "Draw workflows dark, as ComfyUI does, or in Emacs's colours, the other."
  (interactive)
  (lispm-comfyui-settings--save 'lispm-comfyui-dark (not lispm-comfyui-dark))
  (lispm-comfyui-settings--redraw-workflows)
  (lispm-comfyui-settings--refresh))

(defun lispm-comfyui-settings-font (font)
  "Draw workflows with FONT, fonts as SVG names them, the first found used."
  (interactive (list (read-string "Fonts, the first found used: " lispm-comfyui-font)))
  (when (string-blank-p font) (user-error "A font is needed"))
  (lispm-comfyui-settings--save 'lispm-comfyui-font (string-trim font))
  (lispm-comfyui-settings--redraw-workflows)
  (lispm-comfyui-settings--refresh))

;;;; The window

(defun lispm-comfyui-settings--line (action label value &optional note)
  "Insert a line of the setting ACTION: its LABEL, VALUE and NOTE."
  (let ((start (point)))
    (insert "  " (propertize label 'face 'lispm-comfyui-settings-dim)
            (propertize " " 'display '(space :align-to 14))
            value
            (if note (concat "  " (propertize note 'face 'lispm-comfyui-settings-dim)) "")
            "\n")
    (put-text-property start (point) 'lispm-comfyui-setting action)))

(defun lispm-comfyui-settings--server-text ()
  "What the server said, as a line's value."
  (let ((server lispm-comfyui-settings--server))
    (if (not (equal (plist-get server :url) lispm-comfyui-url))
        (propertize "not asked yet" 'face 'lispm-comfyui-settings-dim)
      (pcase (plist-get server :state)
        ('asking (propertize "Asking…" 'face 'lispm-comfyui-settings-dim))
        ('answered
         (concat (propertize "● " 'face 'lispm-comfyui-settings-good)
                 (format "ComfyUI %s" (or (plist-get server :version) ""))
                 (if (plist-get server :device) (concat ", " (plist-get server :device)) "")
                 (if (plist-get server :types) (format ", %d node types" (plist-get server :types)) "")))
        (_ (concat (propertize "○ " 'face 'lispm-comfyui-settings-bad)
                   (propertize (format "No answer: %s" (plist-get server :error))
                               'face 'lispm-comfyui-settings-bad)))))))

(defun lispm-comfyui-settings--refresh ()
  "Draw the settings anew, if they are shown, point kept on its setting."
  (when-let* ((buffer (get-buffer lispm-comfyui-settings-buffer-name)))
    (with-current-buffer buffer
      (let ((setting (get-text-property (point) 'lispm-comfyui-setting))
            (inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "LispM-ComfyUI Settings" 'face '(lispm-comfyui-settings-heading (:height 1.2)))
                "\n\n"
                (propertize "Server" 'face 'lispm-comfyui-settings-heading) "\n")
        (lispm-comfyui-settings--line 'address "Address" lispm-comfyui-url "a changes it")
        (lispm-comfyui-settings--line 'test "Status" (lispm-comfyui-settings--server-text) "t asks again")
        (lispm-comfyui-settings--line 'open "Its page" "ComfyUI's own frontend" "o opens it")
        (insert "\n" (propertize "Look" 'face 'lispm-comfyui-settings-heading) "\n")
        (lispm-comfyui-settings--line 'colours "Colours"
                                      (if lispm-comfyui-dark "Dark, as ComfyUI's" "Emacs's colours")
                                      "c changes them")
        (lispm-comfyui-settings--line 'font "Font" lispm-comfyui-font "f changes it")
        (insert "\n"
                (propertize "Saved for next time.  RET changes the setting at point, q goes back to the workflow."
                            'face 'lispm-comfyui-settings-dim)
                "\n")
        (goto-char (point-min))
        (if-let* ((found (and setting (text-property-any (point-min) (point-max)
                                                         'lispm-comfyui-setting setting))))
            (goto-char found)
          (lispm-comfyui-settings-next))))))

(defun lispm-comfyui-settings-next ()
  "Go to the next setting."
  (interactive)
  (let ((here (get-text-property (point) 'lispm-comfyui-setting))
        (point (point)))
    (when here
      (setq point (or (next-single-property-change point 'lispm-comfyui-setting) (point-max))))
    (when-let* ((next (text-property-not-all point (point-max) 'lispm-comfyui-setting nil)))
      (goto-char next))))

(defun lispm-comfyui-settings-previous ()
  "Go to the previous setting."
  (interactive)
  (let ((start (or (previous-single-property-change (line-beginning-position) 'lispm-comfyui-setting)
                   (point-min))))
    (goto-char start)
    (unless (get-text-property (point) 'lispm-comfyui-setting)
      (when-let* ((previous (previous-single-property-change (point) 'lispm-comfyui-setting)))
        (goto-char previous)))
    (goto-char (line-beginning-position))))

(defun lispm-comfyui-settings-act ()
  "Change the setting at point."
  (interactive)
  (pcase (get-text-property (point) 'lispm-comfyui-setting)
    ('address (call-interactively #'lispm-comfyui-settings-address))
    ('test (lispm-comfyui-settings-test))
    ('open (lispm-comfyui-settings-open-server))
    ('colours (lispm-comfyui-settings-colours))
    ('font (call-interactively #'lispm-comfyui-settings-font))
    (_ (lispm-comfyui-settings-next))))

(defun lispm-comfyui-settings-back ()
  "Go back to the workflow the settings were opened from."
  (interactive)
  (let ((back lispm-comfyui-settings--back))
    (if (buffer-live-p back)
        (switch-to-buffer back)
      (quit-window))))

(lispm-kit-define-commands lispm-comfyui-settings-mode
  (lispm-comfyui-settings-act :key "RET" :help "Change the setting at point")
  (lispm-comfyui-settings-next :key ("n" "C-n" "<down>" "TAB") :help "Go to the next setting")
  (lispm-comfyui-settings-previous :key ("p" "C-p" "<up>" "<backtab>")
                                   :help "Go to the previous setting")
  :menu "Settings"
  (lispm-comfyui-settings-address :key "a" :help "Work with another ComfyUI server"
                                  :label "Server Address…" :button "Server")
  (lispm-comfyui-settings-test :key ("t" "g") :help "Ask the server whether it answers"
                               :label "Ask the Server" :button "Test")
  (lispm-comfyui-settings-open-server :key "o" :help "Open the server's own page"
                                      :label "Open the Server's Page")
  "---"
  (lispm-comfyui-settings-colours :key "c" :help "Dark, as ComfyUI, or Emacs's colours"
                                  :label "Dark Colours" :style toggle :selected lispm-comfyui-dark)
  (lispm-comfyui-settings-font :key "f" :help "The fonts workflows are drawn with" :label "Font…")
  "---"
  (lispm-comfyui-settings-back :key ("q" "," "C-c C-c") :help "Go back to the workflow"
                               :label "Back to the Workflow" :button "Done"))

(define-derived-mode lispm-comfyui-settings-mode special-mode "LispM-ComfyUI Settings"
  "LispM-ComfyUI's settings: the ComfyUI server, the colours, the font.
\\{lispm-comfyui-settings-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (lispm-comfyui-settings-test))))

;;;###autoload
(defun lispm-comfyui-settings ()
  "Show LispM-ComfyUI's settings, the server asked at once whether it answers."
  (interactive)
  (let ((back (current-buffer))
        (buffer (get-buffer-create lispm-comfyui-settings-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-comfyui-settings-mode)
        (lispm-comfyui-settings-mode))
      (unless (eq back buffer)
        (setq lispm-comfyui-settings--back back)))
    (switch-to-buffer buffer)
    (lispm-comfyui-settings--refresh)
    (lispm-comfyui-settings-test)
    buffer))

(provide 'lispm-comfyui-settings)
;;; lispm-comfyui-settings.el ends here
