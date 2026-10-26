;;; lispm-comfyui-client.el --- Talking to a ComfyUI server  -*- lexical-binding: t; -*-

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

;; ComfyUI's server, at `lispm-comfyui-url', as its own frontend talks
;; to it: over HTTP, the node types it has (/object_info), the
;; workflows its user library keeps (/api/userdata), prompts queued
;; (/prompt) and stopped (/interrupt), images fetched (/view) and
;; uploaded (/upload/image); over a WebSocket (/ws), what it does while
;; it runs them, which node, how far along, and what each made.
;;
;; Every request is asynchronous: its callback gets what the server
;; answered, read as JSON unless asked for the bytes, and an error
;; message or nil.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'websocket)
(require 'lispm-comfyui-graph)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defgroup lispm-comfyui nil
  "ComfyUI's workflows, drawn, edited and run in Emacs."
  :group 'applications
  :prefix "lispm-comfyui-")

(defcustom lispm-comfyui-url "http://127.0.0.1:8188"
  "Where ComfyUI's server is: http://HOST:PORT, without a path."
  :type 'string)

(defcustom lispm-comfyui-headers nil
  "Headers sent with each request to ComfyUI's server, an alist.
For a server behind a proxy asking for them, as
\((\"Authorization\" . \"Basic ...\"))."
  :type '(alist :key-type string :value-type string))

(defun lispm-comfyui--base ()
  "`lispm-comfyui-url' without a slash at its end."
  (string-remove-suffix "/" lispm-comfyui-url))

;;;; HTTP

(defun lispm-comfyui--body ()
  "The body of the response in this buffer, the bytes after its headers."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers) (markerp url-http-end-of-headers)
           (marker-position url-http-end-of-headers))
      (goto-char url-http-end-of-headers)
    (re-search-forward "\r?\n\r?\n" nil 'move))
  (skip-chars-forward "\r\n" (min (point-max) (+ (point) 4)))
  (buffer-substring-no-properties (point) (point-max)))

(defun lispm-comfyui--json (bytes)
  "BYTES, JSON in UTF-8, read; nil if they are not JSON."
  (ignore-errors
    (json-parse-string (decode-coding-string bytes 'utf-8) :object-type 'alist)))

(cl-defun lispm-comfyui-request (path callback &key (method "GET") data content-type bytes)
  "Ask ComfyUI's server for PATH; call CALLBACK with its answer.
PATH starts with a slash.  DATA, sent with METHOD, is a string of bytes
or an object to send as JSON.  CALLBACK gets the answer, read as JSON
unless BYTES says to give the bytes, and an error message or nil.  An
answer that is an error still comes, read, with its message."
  (let* ((url-request-method method)
         (json (and data (not (stringp data))))
         (url-request-data (cond ((null data) nil)
                                 (json (encode-coding-string (json-serialize data) 'utf-8))
                                 (t data)))
         (url-request-extra-headers
          (append (when data
                    `(("Content-Type" . ,(or content-type
                                             (if json "application/json" "application/octet-stream")))))
                  lispm-comfyui-headers))
         (url-show-status nil)
         (url (concat (lispm-comfyui--base) path)))
    (condition-case err
        (url-retrieve
         url
         (lambda (status)
           (let ((buffer (current-buffer)))
             (unwind-protect
                 (let* ((code (and (boundp 'url-http-response-status) url-http-response-status))
                        (body (lispm-comfyui--body))
                        (value (if bytes body (lispm-comfyui--json body)))
                        (problem (cond
                                ((plist-get status :error)
                                 (if (and code (>= code 400))
                                     (lispm-comfyui--error-message code value body)
                                   (format "%s: %s" url (error-message-string
                                                         (plist-get status :error)))))
                                ((and code (>= code 400))
                                 (lispm-comfyui--error-message code value body)))))
                   (funcall callback value problem))
               (when (buffer-live-p buffer) (kill-buffer buffer)))))
         nil t t)
      (error (funcall callback nil (format "%s: %s" url (error-message-string err)))))))

(defun lispm-comfyui--error-message (code value body)
  "What the server meant by its answer CODE, VALUE read, BODY as sent."
  (let ((error (and (consp value) (alist-get 'error value))))
    (cond ((stringp error) error)
          ((consp error)
           (string-join (delq nil (list (alist-get 'message error)
                                        (let ((details (alist-get 'details error)))
                                          (and (stringp details) (not (string-empty-p details))
                                               details))))
                        ": "))
          (t (format "The server answered %s%s" code
                     (if (and (stringp body) (< (length body) 200) (not (string-blank-p body)))
                         (concat ": " (string-trim body))
                       ""))))))

(defun lispm-comfyui--path (&rest parts)
  "PARTS joined into a path, each but the first encoded for a URL."
  (concat (car parts) (mapconcat #'url-hexify-string (cdr parts) "")))

(defun lispm-comfyui--query (path query)
  "PATH with QUERY, an alist of strings, after a question mark."
  (concat path "?" (url-build-query-string (mapcar (lambda (entry) (list (car entry) (cdr entry)))
                                                   query))))

;;;; Node types

(defvar lispm-comfyui--types (make-hash-table :test #'equal)
  "The node types of each server, by its URL.")

(defun lispm-comfyui-types-of-server (callback &optional again)
  "Call CALLBACK with the node types of the server, and an error or nil.
They are read once, unless AGAIN; see `lispm-comfyui-types'."
  (let ((base (lispm-comfyui--base)))
    (if-let* (((not again)) (types (gethash base lispm-comfyui--types)))
        (funcall callback types nil)
      (lispm-comfyui-request
       "/object_info"
       (lambda (value error)
         (if (or error (not (consp value)))
             (funcall callback nil (or error "The server said nothing of its node types"))
           (let ((types (lispm-comfyui-types value)))
             (puthash base types lispm-comfyui--types)
             (funcall callback types nil))))))))

(defun lispm-comfyui-cached-types ()
  "The node types of the server, if read already, or nil."
  (gethash (lispm-comfyui--base) lispm-comfyui--types))

;;;; The user library

(defun lispm-comfyui-list-workflows (callback)
  "Call CALLBACK with the names of the workflows the server keeps.
Names are paths under its user library's workflows directory."
  (lispm-comfyui-request
   (lispm-comfyui--query "/api/userdata" '(("dir" . "workflows") ("recurse" . "true")))
   (lambda (value error)
     (funcall callback (and (vectorp value) (sort (append value nil) #'string<)) error))))

(defun lispm-comfyui-read-workflow (name callback)
  "Call CALLBACK with the workflow NAME, read as JSON, and an error or nil."
  (lispm-comfyui-request (lispm-comfyui--path "/api/userdata/" (concat "workflows/" name))
                         callback))

(defun lispm-comfyui-write-workflow (name json callback)
  "Save JSON, a string, as the workflow NAME; call CALLBACK when done."
  (lispm-comfyui-request
   (lispm-comfyui--query (lispm-comfyui--path "/api/userdata/" (concat "workflows/" name))
                         '(("overwrite" . "true") ("full_info" . "true")))
   callback :method "POST" :data (encode-coding-string json 'utf-8)
   :content-type "application/json"))

;;;; Running

(defvar lispm-comfyui-client-id
  (apply #'format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
         (cl-loop repeat 8 collect (random 65536)))
  "Who this Emacs is to the server: whose prompts its WebSocket tells of.")

(defun lispm-comfyui-queue (prompt workflow callback)
  "Queue PROMPT, whose WORKFLOW is a graph's JSON; call CALLBACK.
CALLBACK gets the server's answer, its prompt_id and node_errors among
it, and an error or nil.  WORKFLOW goes with the prompt as ComfyUI's
frontend sends it, kept in the images made."
  (lispm-comfyui-request
   "/prompt" callback :method "POST"
   :data `((prompt . ,prompt)
           (client_id . ,lispm-comfyui-client-id)
           (extra_data . ((extra_pnginfo . ((workflow . ,workflow))))))))

(defun lispm-comfyui-interrupt (&optional prompt-id callback)
  "Stop what the server runs, if PROMPT-ID, only that prompt."
  (lispm-comfyui-request "/interrupt" (or callback #'ignore) :method "POST"
                         :data (if prompt-id `((prompt_id . ,prompt-id)) '((all . t)))))

(defun lispm-comfyui-history (prompt-id callback)
  "Call CALLBACK with what the server keeps of the run PROMPT-ID."
  (lispm-comfyui-request (lispm-comfyui--path "/history/" prompt-id)
                         (lambda (value error)
                           (funcall callback (cdr (assq (intern prompt-id) value)) error))))

;;;; Images

(defvar lispm-comfyui-directory
  (expand-file-name (format "lispm-comfyui-%d/" (emacs-pid)) temporary-file-directory)
  "Where images fetched from the server are kept while Emacs runs.")

(defun lispm-comfyui--file-for (image)
  "The file here of IMAGE, the server's (filename, subfolder, type)."
  (let ((name (format "%s-%s-%s" (or (alist-get 'type image) "output")
                      (replace-regexp-in-string "/" "_" (or (alist-get 'subfolder image) ""))
                      (alist-get 'filename image))))
    (expand-file-name (replace-regexp-in-string "[^[:alnum:]._-]" "_" name)
                      lispm-comfyui-directory)))

(defun lispm-comfyui-fetch-image (image callback)
  "Fetch IMAGE, the server's (filename, subfolder, type); call CALLBACK.
CALLBACK gets the file it is kept in here, and an error or nil.  An
image fetched before is not fetched again."
  (let ((file (lispm-comfyui--file-for image)))
    (if (file-exists-p file)
        (funcall callback file nil)
      (lispm-comfyui-request
       (lispm-comfyui--query "/view" `(("filename" . ,(alist-get 'filename image))
                                       ("subfolder" . ,(or (alist-get 'subfolder image) ""))
                                       ("type" . ,(or (alist-get 'type image) "output"))))
       (lambda (bytes error)
         (if error
             (funcall callback nil error)
           (make-directory lispm-comfyui-directory t)
           (let ((coding-system-for-write 'binary))
             (with-temp-file file
               (set-buffer-multibyte nil)
               (insert bytes)))
           (funcall callback file nil)))
       :bytes t))))

(defun lispm-comfyui-upload-image (file callback)
  "Upload FILE as an input image; call CALLBACK with its name there.
CALLBACK gets the server's answer, whose name is the image's, and an
error or nil."
  (let* ((boundary (format "lispm-comfyui-%x%x" (random 4294967295) (random 4294967295)))
         (name (file-name-nondirectory file))
         (bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (let ((coding-system-for-read 'binary))
                    (insert-file-contents-literally file))
                  (buffer-string)))
         (data (concat
                "--" boundary "\r\n"
                "Content-Disposition: form-data; name=\"overwrite\"\r\n\r\ntrue\r\n"
                "--" boundary "\r\n"
                (format "Content-Disposition: form-data; name=\"image\"; filename=\"%s\"\r\n"
                        (encode-coding-string name 'utf-8))
                "Content-Type: application/octet-stream\r\n\r\n"
                bytes "\r\n--" boundary "--\r\n")))
    (lispm-comfyui-request "/upload/image" callback :method "POST" :data data
                           :content-type (concat "multipart/form-data; boundary=" boundary))))

;;;; The WebSocket

(defvar lispm-comfyui--socket nil
  "The WebSocket to the server, or nil.")

(defvar lispm-comfyui--socket-url nil
  "The URL `lispm-comfyui--socket' was opened to.")

(defvar lispm-comfyui-message-functions nil
  "Functions called with each message the server's WebSocket sends.
Each gets its type, a string such as \"executing\", \"progress\" or
\"executed\", and its data, an alist.  A preview of the image a
sampler makes as it goes comes as the type \"preview\", with data
\((file . FILE) (node . NODE)), NODE nil when the server did not say
which node made it.")

(defvar lispm-comfyui--preview-count 0
  "How many previews came, which names the file of the next.")

(defun lispm-comfyui--socket-url ()
  "Where the server's WebSocket is."
  (concat (replace-regexp-in-string "\\`http" "ws" (lispm-comfyui--base))
          "/ws?clientId=" lispm-comfyui-client-id))

(defun lispm-comfyui--dispatch (type data)
  "Call `lispm-comfyui-message-functions' with TYPE and DATA."
  (dolist (function lispm-comfyui-message-functions)
    (with-demoted-errors "ComfyUI: %S"
      (funcall function type data))))

(defun lispm-comfyui--uint32 (bytes start)
  "The big-endian 32-bit number at START of BYTES."
  (logior (ash (aref bytes start) 24) (ash (aref bytes (+ start 1)) 16)
          (ash (aref bytes (+ start 2)) 8) (aref bytes (+ start 3))))

(defun lispm-comfyui--binary (bytes)
  "Take BYTES, a binary message of the server's WebSocket."
  (when (>= (length bytes) 8)
    (pcase (lispm-comfyui--uint32 bytes 0)
      (1                                ; A preview: its format, its bytes.
       (lispm-comfyui--preview (if (= (lispm-comfyui--uint32 bytes 4) 2) "png" "jpg")
                               (substring bytes 8) nil))
      (4                                ; A preview with what it is of.
       (let* ((length (lispm-comfyui--uint32 bytes 4))
              (metadata (lispm-comfyui--json (substring bytes 8 (+ 8 length)))))
         (lispm-comfyui--preview (if (equal (alist-get 'image_type metadata) "image/png")
                                     "png" "jpg")
                                 (substring bytes (+ 8 length))
                                 (or (alist-get 'display_node_id metadata)
                                     (alist-get 'node_id metadata))))))))

(defun lispm-comfyui--preview (extension bytes node)
  "Keep BYTES, a preview of the image NODE makes, as a file of EXTENSION."
  (make-directory lispm-comfyui-directory t)
  (let ((file (expand-file-name (format "preview-%d.%s" (% (cl-incf lispm-comfyui--preview-count) 4)
                                        extension)
                                lispm-comfyui-directory))
        (coding-system-for-write 'binary))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert bytes))
    (lispm-comfyui--dispatch "preview" `((file . ,file) (node . ,node)))))

(defun lispm-comfyui-socket-open-p ()
  "Non-nil if the WebSocket to the server is open."
  (and lispm-comfyui--socket (websocket-openp lispm-comfyui--socket)
       (equal lispm-comfyui--socket-url (lispm-comfyui--socket-url))))

(defun lispm-comfyui-connect-socket ()
  "Open the WebSocket to the server, unless it is open."
  (unless (lispm-comfyui-socket-open-p)
    (lispm-comfyui-close-socket)
    (let ((url (lispm-comfyui--socket-url)))
      (setq lispm-comfyui--socket-url url
            lispm-comfyui--socket
            (websocket-open
             url
             :custom-header-alist lispm-comfyui-headers
             :on-message
             (lambda (_socket frame)
               (pcase (websocket-frame-opcode frame)
                 ('text (when-let* ((message (ignore-errors
                                               (json-parse-string (websocket-frame-text frame)
                                                                  :object-type 'alist))))
                          (lispm-comfyui--dispatch (alist-get 'type message)
                                                   (alist-get 'data message))))
                 ('binary (lispm-comfyui--binary (websocket-frame-payload frame)))))
             :on-close (lambda (socket)
                         (when (eq socket lispm-comfyui--socket)
                           (setq lispm-comfyui--socket nil)
                           (lispm-comfyui--dispatch "closed" nil)))
             :on-error (lambda (_socket _type err)
                         (message "ComfyUI: %s" (error-message-string err))))))))

(defun lispm-comfyui-close-socket ()
  "Close the WebSocket to the server, if open."
  (when lispm-comfyui--socket
    (let ((socket lispm-comfyui--socket))
      (setq lispm-comfyui--socket nil)
      (ignore-errors (websocket-close socket)))))

(provide 'lispm-comfyui-client)
;;; lispm-comfyui-client.el ends here
