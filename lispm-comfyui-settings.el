;;; lispm-comfyui-settings.el --- LispM-ComfyUI's settings, in a form  -*- lexical-binding: t; -*-

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

;; LispM-ComfyUI's settings, which , opens from a workflow, are a form
;; of LispM UI's (lispm-ui-form.el, `lispm-form' by its first name, which
;; an older LispM Agent had), shown in the workflow's window as a dialog
;; is: the ComfyUI server's address in a text field, a Test Connection
;; button asking the server at the address typed whether it answers,
;; and what it is, said under the field, an Open ComfyUI's Page button;
;; the colours workflows are drawn in, radio buttons, and their fonts,
;; a text field.  Save keeps them with Customize, for next time, and
;; draws the workflows open anew; Cancel leaves them as they were.
;;
;; Where LispM UI is not, in an Emacs of its own, the settings are
;; Customize's.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'lispm-comfyui-client)
(require 'lispm-form nil t)

(defvar lispm-comfyui-dark)
(defvar lispm-comfyui-font)
(defvar lispm-form-types)
(declare-function lispm-form "lispm-form" (title fields &rest keys))
(declare-function lispm-form-value "lispm-form" (key))
(declare-function lispm-form-redraw "lispm-form" ())
(declare-function lispm-comfyui--canvas-buffers "lispm-comfyui")
(declare-function lispm-comfyui-reload "lispm-comfyui")
(declare-function lispm-comfyui--draw "lispm-comfyui")

(defconst lispm-comfyui-settings-title "LispM-ComfyUI Settings"
  "The title of the settings' form.")

(defvar lispm-comfyui-settings--server nil
  "What the server said when last asked: a plist.
:state is `asking', `answered' or `failed'; :url, the address asked;
:version, :device and :types, what it is; :error, what went wrong.")

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

(defun lispm-comfyui-settings--address (text)
  "TEXT, typed as a server's address, as http://HOST:PORT; or nil.
A missing http:// is added and a slash at the end dropped."
  (when text
    (let ((address (string-remove-suffix "/" (string-trim text))))
      (unless (string-match-p "\\`[a-z]+://" address)
        (setq address (concat "http://" address)))
      (and (lispm-comfyui-settings--address-p address) address))))

(defun lispm-comfyui-settings--address-p (address)
  "Non-nil if ADDRESS is a server's address, http or https, host and port."
  (let ((url (url-generic-parse-url address)))
    (and (member (url-type url) '("http" "https"))
         (not (string-empty-p (or (url-host url) "")))
         (member (url-filename url) '("" "/")))))

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

(defun lispm-comfyui-settings-test (&optional address then)
  "Ask the server at ADDRESS, by default the one set, whether it answers.
What it says is kept in `lispm-comfyui-settings--server'; THEN, if
given, is called with no argument each time that changes."
  (let ((url (or address lispm-comfyui-url)))
    (cl-flet ((stands (server)
                (when (equal (plist-get lispm-comfyui-settings--server :url) url)
                  (setq lispm-comfyui-settings--server server)
                  (when then (funcall then)))))
      (setq lispm-comfyui-settings--server (list :state 'asking :url url))
      (when then (funcall then))
      (let ((lispm-comfyui-url url))
        (lispm-comfyui-request
         "/system_stats"
         (lambda (stats error)
           (if (or error (not (consp stats)))
               (stands (list :state 'failed :url url
                             :error (or error "it answered nothing ComfyUI says")))
             (stands (list :state 'answered :url url
                           :version (alist-get 'comfyui_version (alist-get 'system stats))
                           :device (lispm-comfyui-settings--device stats)))
             ;; How many node types it has, read apart: the address
             ;; may not be the one whose types the workflows use.
             (let ((lispm-comfyui-url url))
               (lispm-comfyui-request
                "/object_info"
                (lambda (info _error)
                  (when (and (consp info) (eq (plist-get lispm-comfyui-settings--server :state) 'answered))
                    (stands (plist-put (copy-sequence lispm-comfyui-settings--server)
                                       :types (length info))))))))))))))

(defun lispm-comfyui-settings--server-text (address)
  "What the server at ADDRESS said when last asked, in a line."
  (let ((server lispm-comfyui-settings--server))
    (if (not (equal (plist-get server :url) address))
        "Not asked yet: Test Connection asks it."
      (pcase (plist-get server :state)
        ('asking "Asking the server…")
        ('answered
         (concat "● ComfyUI " (or (plist-get server :version) "")
                 (if (plist-get server :device) (concat " · " (plist-get server :device)) "")
                 (if (plist-get server :types) (format " · %d node types" (plist-get server :types)) "")))
        (_ (format "○ No answer: %s." (plist-get server :error)))))))

;;;; The form

(defun lispm-comfyui-settings--typed-address ()
  "The address typed in the form in this buffer, as http://HOST:PORT."
  (or (lispm-comfyui-settings--address (lispm-form-value 'address))
      (user-error "A server's address is http://HOST:PORT, as http://127.0.0.1:8188")))

(defun lispm-comfyui-settings--test-typed ()
  "Ask the server at the address typed whether it answers, and say so."
  (let ((form (current-buffer)))
    (lispm-comfyui-settings-test
     (lispm-comfyui-settings--typed-address)
     (lambda ()
       (when (buffer-live-p form)
         (with-current-buffer form (lispm-form-redraw)))))))

(defun lispm-comfyui-settings--open-typed ()
  "Open the page of the server at the address typed, ComfyUI's frontend."
  (browse-url (lispm-comfyui-settings--typed-address)))

(defun lispm-comfyui-settings--fields ()
  "The fields of the settings' form.
A LispM whose forms have no buttons, older than these settings, shows
them without, and says under the address only how it is written."
  (let ((buttons (memq 'buttons (bound-and-true-p lispm-form-types))))
    `((server :type heading :label "Server")
      (address :type text :label "Address" :required t
               :validate ,(lambda (text)
                            (unless (lispm-comfyui-settings--address text)
                              "it is http://HOST:PORT, as http://127.0.0.1:8188"))
               :help ,(if buttons
                          (lambda (values)
                            (lispm-comfyui-settings--server-text
                             (lispm-comfyui-settings--address (alist-get 'address values))))
                        "As http://HOST:PORT, the ComfyUI server's"))
      ,@(when buttons
          `((actions :type buttons
                     :buttons (("Test Connection" . lispm-comfyui-settings--test-typed)
                               ("Open ComfyUI's Page" . lispm-comfyui-settings--open-typed)))))
      (look :type heading :label "Look")
      (colours :type radio :label "Colours"
               :choices ((dark . "Dark, as ComfyUI draws them") (emacs . "Emacs's own")))
      (font :type text :label "Font" :required t
            :help "Fonts as CSS names them, the first this computer has drawn"))))

(defun lispm-comfyui-settings--apply (values)
  "Keep VALUES, of the settings' form, and draw the workflows open anew."
  (let ((address (lispm-comfyui-settings--address (alist-get 'address values)))
        (dark (eq (alist-get 'colours values) 'dark))
        (font (alist-get 'font values))
        (again nil))
    (unless (equal address lispm-comfyui-url)
      (lispm-comfyui-close-socket)
      (lispm-comfyui-settings--save 'lispm-comfyui-url address)
      (setq again t))
    (unless (eq dark (and lispm-comfyui-dark t))
      (lispm-comfyui-settings--save 'lispm-comfyui-dark dark))
    (unless (equal font lispm-comfyui-font)
      (lispm-comfyui-settings--save 'lispm-comfyui-font font))
    (lispm-comfyui-settings--redraw-workflows again)))

;;;###autoload
(defun lispm-comfyui-settings ()
  "Show LispM-ComfyUI's settings in a form, the server asked at once.
In an Emacs without LispM's forms, show Customize's."
  (interactive)
  (if (not (fboundp 'lispm-form))
      (customize-group 'lispm-comfyui)
    (let ((buffer (or (get-buffer (format "*%s*" lispm-comfyui-settings-title))
                      (lispm-form lispm-comfyui-settings-title (lispm-comfyui-settings--fields)
                                  :values `((address . ,lispm-comfyui-url)
                                            (colours . ,(if lispm-comfyui-dark 'dark 'emacs))
                                            (font . ,lispm-comfyui-font))
                                  :submit "Save"
                                  :on-submit #'lispm-comfyui-settings--apply))))
      (pop-to-buffer-same-window buffer)
      (when (memq 'buttons (bound-and-true-p lispm-form-types))
        (with-current-buffer buffer
          (lispm-comfyui-settings--test-typed)))
      buffer)))

(provide 'lispm-comfyui-settings)
;;; lispm-comfyui-settings.el ends here
