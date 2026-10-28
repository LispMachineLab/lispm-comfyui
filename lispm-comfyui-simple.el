;;; lispm-comfyui-simple.el --- A fixed form for ComfyUI workflows  -*- lexical-binding: t; -*-

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

;; One LispM UI form accompanies each workflow buffer, in the same window.
;; Its fixed groups show common parameters first, then nodes in data-flow
;; order, then the last run's pictures.  The workflow buffer remains the
;; owner of the graph, undo history and running requests.  Controls resolve
;; nodes by their numbers and use the canvas's editing functions, so undo
;; and agents' changes cannot leave a control pointing at an old graph.
;; Text edits update their other occurrences in place; a different set of
;; fields, as after relinking or a dynamic choice, rebuilds the form with
;; point kept in the same field.  Scrolling is ordinary buffer scrolling.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-comfyui-graph)
(require 'lispm-ui-form)
(require 'easymenu)

(defvar lispm-comfyui--graph)
(defvar lispm-comfyui--name)
(defvar lispm-comfyui--runs)
(defvar lispm-comfyui--current-view)
(declare-function lispm-comfyui--types "lispm-comfyui")
(declare-function lispm-comfyui--header "lispm-comfyui")
(declare-function lispm-comfyui--set "lispm-comfyui" (node name value))
(declare-function lispm-comfyui--set-link "lispm-comfyui" (node input source output))
(declare-function lispm-comfyui--io-nodes "lispm-comfyui")
(declare-function lispm-comfyui-toggle-view "lispm-comfyui")
(declare-function lispm-comfyui-run "lispm-comfyui" (&optional callback))
(declare-function lispm-comfyui-stop "lispm-comfyui")
(declare-function lispm-comfyui-save "lispm-comfyui" (&optional name))
(declare-function lispm-comfyui-settings "lispm-comfyui-settings")
(declare-function lispm-comfyui-undo "lispm-comfyui")
(declare-function lispm-comfyui-redo "lispm-comfyui")
(declare-function lispm-comfyui-show-workflows "lispm-comfyui")
(declare-function lispm-comfyui--window-changed "lispm-comfyui" (window))

(defvar-local lispm-comfyui--simple-buffer nil
  "The form accompanying this workflow buffer, or nil before it is made.")

(defvar-local lispm-comfyui-simple--workflow nil
  "The workflow buffer whose graph this form edits.")

(defvar-local lispm-comfyui-simple--problems nil
  "Invalid fields in this form, as (KEY . MESSAGE).
Their unfinished text stays editable while the graph keeps its last value.")

(defvar-local lispm-comfyui-simple--shape nil
  "The fields' last structural description, without values or callbacks.")

(defvar lispm-comfyui-simple--editing nil
  "The field currently changing the graph, dynamically bound by its callback.")

(defvar lispm-comfyui-simple--updating nil
  "Non-nil while form values are copied from the graph, suppressing callbacks.")

;;;; Fields from the graph

(defun lispm-comfyui-simple--key (section id &optional name)
  "A stable field key for SECTION, node ID and widget or input NAME."
  (intern (format "%s-%s%s" section id (if name (concat "-" name) ""))))

(defun lispm-comfyui-simple--nodes ()
  "The shown graph's nodes in stable topological order.
Independent nodes retain their saved order.  In a cyclic graph, the
remaining nodes follow in saved order so every node can still be edited."
  (let ((pending (copy-sequence (lispm-comfyui-graph-nodes lispm-comfyui--graph)))
        (links (lispm-comfyui-graph-links lispm-comfyui--graph))
        (ordered nil))
    (while pending
      (let ((ready
             (seq-filter
              (lambda (node)
                (not (seq-some
                      (lambda (link)
                        (and (equal (lispm-comfyui-link-to link) (lispm-comfyui-node-id node))
                             (seq-some (lambda (source)
                                         (equal (lispm-comfyui-node-id source)
                                                (lispm-comfyui-link-from link)))
                                       pending)))
                      links)))
              pending)))
        (unless ready (setq ready pending))
        (setq ordered (append ordered ready)
              pending (seq-difference pending ready #'eq))))
    ordered))

(defun lispm-comfyui-simple--widgets (node types)
  "The widgets of NODE in TYPES, including widgets saved by name."
  (or (lispm-comfyui-node-widgets node types)
      (when-let* ((definition (gethash (lispm-comfyui-node-type node) types)))
        (lispm-comfyui-widgets
         definition
         (lispm-comfyui-values-from-alist
          definition (lispm-comfyui-node-widget-alist node types))))))

(defun lispm-comfyui-simple--number-problem (spec value)
  "Explain why VALUE is not a complete number of SPEC, or return nil.
Reject unfinished numbers and values outside the server's bounds before
calling the graph editor, which otherwise clamps and truncates numbers."
  (let* ((kind (lispm-comfyui-widget-kind spec))
         (text (string-trim (or value "")))
         (pattern (if (eq kind 'int) "\\`[-+]?[0-9]+\\'"
                    "\\`[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"))
         (minimum (lispm-comfyui-spec-option spec 'min))
         (maximum (lispm-comfyui-spec-option spec 'max)))
    (cond ((not (string-match-p pattern text))
           (if (eq kind 'int) "Enter a whole number" "Enter a number"))
          (t (let ((number (string-to-number text)))
               (cond ((or (isnan (float number)) (= (abs (float number)) 1.0e+INF))
                      "Enter a finite number")
                     ((and (numberp minimum) (< number minimum))
                      (format "The minimum is %s" minimum))
                     ((and (numberp maximum) (> number maximum))
                      (format "The maximum is %s" maximum))))))))

(defun lispm-comfyui-simple--widget-field (section node widget types &optional label control)
  "A field in SECTION for NODE's WIDGET of TYPES.
LABEL overrides its name.  CONTROL makes the seed's after-run choice."
  (let* ((id (lispm-comfyui-node-id node))
         (name (concat (plist-get widget :name) (if control "/control" "")))
         (key (lispm-comfyui-simple--key section id name))
         (spec (plist-get widget :spec))
         (kind (if control 'combo (plist-get widget :kind)))
         (value (cdr (assoc name (lispm-comfyui-node-widget-alist node types))))
         (choices (if control lispm-comfyui-controls (lispm-comfyui-widget-choices spec)))
         (type (pcase kind
                 ((or 'combo 'dynamic) 'choice)
                 ('boolean 'check)
                 ('string (if (lispm-comfyui-spec-option spec 'multiline) 'text-area 'text))
                 (_ 'text))))
    (list key :type type :label (or label name) :node-id id :widget-name name
          :value (pcase type
                   ('check (not (memq value '(nil :false :null))))
                   ((or 'text 'text-area) (if (stringp value) value (format "%s" (or value ""))))
                   (_ value))
          ;; LispM UI renders labels as strings; the server's choice values
          ;; may be numbers and must retain their types in the graph.
          :choices (mapcar (lambda (choice) (cons choice (format "%s" choice))) choices)
          :validate (when (memq kind '(int float))
                      (lambda (text) (lispm-comfyui-simple--number-problem spec text)))
          :on-change (lambda (text)
                       (lispm-comfyui-simple--set-widget key id name spec kind text)))))

(defun lispm-comfyui-simple--widget-fields (section node widget types &optional label)
  "Fields for WIDGET of NODE in TYPES, in SECTION, optionally with LABEL.
A widget's after-run control immediately follows the value it controls."
  (cons (lispm-comfyui-simple--widget-field section node widget types label)
        (when (plist-get widget :control)
          (list (lispm-comfyui-simple--widget-field
                 section node widget types
                 (if label (concat label " after run")
                   (concat (plist-get widget :name) " after run")) t)))))

(defun lispm-comfyui-simple--prompts (node input)
  "Text encoders feeding NODE's INPUT, through reroutes or conditioning nodes.
Walk only conditioning links after the named input, and visit each node once."
  (let ((index (lispm-comfyui-input-index node input))
        (seen nil) (found nil))
    (cl-labels
        ((walk (link)
           (when-let* ((source (and link (lispm-comfyui-node lispm-comfyui--graph
                                                           (lispm-comfyui-link-from link))))
                       (id (lispm-comfyui-node-id source))
                       ((not (member id seen))))
             (push id seen)
             (if (string-prefix-p "CLIPTextEncode" (lispm-comfyui-node-type source))
                 (push source found)
               (cl-loop for slot in (lispm-comfyui-node-inputs source)
                        for slot-index from 0
                        when (lispm-comfyui-types-match-p (lispm-comfyui-slot-type slot) "CONDITIONING")
                        do (walk (lispm-comfyui-input-link lispm-comfyui--graph source slot-index)))))))
      (when index (walk (lispm-comfyui-input-link lispm-comfyui--graph node index))))
    (nreverse found)))

(defun lispm-comfyui-simple--common (nodes types)
  "The common fields found among NODES of TYPES, without duplicate widgets."
  (let ((fields nil) (seen nil))
    (cl-labels
        ((add (node name label)
           (let* ((id (lispm-comfyui-node-id node))
                  (identity (cons id name))
                  (widget (seq-find (lambda (item) (equal (plist-get item :name) name))
                                    (lispm-comfyui-simple--widgets node types))))
             (when (and widget (not (member identity seen)))
               (push identity seen)
               (setq fields (append fields
                                    (lispm-comfyui-simple--widget-fields
                                     'common node widget types (format "%s #%s" label id))))))))
      (dolist (node nodes)
        (let ((type (lispm-comfyui-node-type node)))
          (when (or (string-match-p "Sampler" type)
                    (and (lispm-comfyui-input-index node "positive")
                         (lispm-comfyui-input-index node "negative")))
            (dolist (input '("positive" "negative"))
              (dolist (encoder (lispm-comfyui-simple--prompts node input))
                (add encoder "text" (concat (capitalize input) " prompt"))))
            (dolist (name '("seed" "noise_seed" "steps" "cfg" "sampler_name" "scheduler" "denoise"))
              (add node name (replace-regexp-in-string "_" " " (capitalize name)))))
          (when (string-match-p "Empty.*Latent" type)
            (dolist (name '("width" "height" "batch_size"))
              (add node name (replace-regexp-in-string "_" " " (capitalize name)))))
          (when (string-match-p "[Ll][Oo][Rr][Aa]" type)
            (dolist (widget (lispm-comfyui-simple--widgets node types))
              (when (string-match-p "strength" (plist-get widget :name))
                (add node (plist-get widget :name) (concat "LoRA " (plist-get widget :name))))))
          (when (string-prefix-p "LoadImage" type) (add node "image" "Image")))))
    (when fields (cons '(common :type heading :label "Common") fields))))

(defun lispm-comfyui-simple--input-field (node slot index types)
  "A choice linking NODE's input SLOT at INDEX from compatible outputs of TYPES."
  (let* ((id (lispm-comfyui-node-id node))
         (name (lispm-comfyui-slot-name slot))
         (key (lispm-comfyui-simple--key 'input id name))
         (link (lispm-comfyui-input-link lispm-comfyui--graph node index))
         (choices
          (cons '(nil . "Not linked")
                (cl-loop for source in (append (lispm-comfyui--io-nodes)
                                               (lispm-comfyui-graph-nodes lispm-comfyui--graph))
                         unless (equal id (lispm-comfyui-node-id source))
                         append (cl-loop for output in (lispm-comfyui-node-outputs source)
                                         for output-index from 0
                                         when (lispm-comfyui-types-match-p
                                               (lispm-comfyui-slot-type output)
                                               (lispm-comfyui-input-type node types index))
                                         collect (cons (cons (lispm-comfyui-node-id source) output-index)
                                                       (format "%s #%s · %s"
                                                               (lispm-comfyui-node-name source types)
                                                               (lispm-comfyui-node-id source)
                                                               (lispm-comfyui-slot-name output))))))))
    (list key :type 'choice :label (concat name " input") :node-id id :input-name name
          :choices choices
          :value (and link (cons (lispm-comfyui-link-from link) (lispm-comfyui-link-from-slot link)))
          :on-change (lambda (value) (lispm-comfyui-simple--set-input key id name value)))))

(defun lispm-comfyui-simple--output-fields ()
  "Read-only images fetched for this workflow's last run, or no fields yet."
  (let ((files (seq-filter #'file-readable-p (plist-get (car lispm-comfyui--runs) :files))))
    (when files
      (cons '(outputs :type heading :label "Last run")
            (cl-loop for file in files for index from 0
                     collect (list (lispm-comfyui-simple--key 'output index)
                                   :type 'status :label (file-name-nondirectory file)
                                   :value (if (display-images-p)
                                              (propertize " " 'display
                                                          (create-image file nil nil :max-width 512
                                                                        :max-height 512 :ascent 'center))
                                            file)))))))

(defun lispm-comfyui-simple--status-text ()
  "The canvas header as short lines, keeping run updates within the form width.
The workflow name, node count and run state retain the same information,
without widening every control when progress is appended to the header."
  (string-trim (replace-regexp-in-string "   +" "\n" (lispm-comfyui--header))))

(defun lispm-comfyui-simple--unavailable-fields (node types)
  "Read-only saved values of NODE when TYPES has no schema for its controls.
Frontend-only or unavailable custom nodes remain visible without inventing
widget names that the graph editor could not validate or save reliably."
  (when (and (lispm-comfyui-node-values node)
             (not (gethash (lispm-comfyui-node-type node) types)))
    (list (list (lispm-comfyui-simple--key 'saved (lispm-comfyui-node-id node))
                :type 'status :label "Saved values"
                :value (mapconcat (lambda (value) (if (stringp value) value (format "%S" value)))
                                  (lispm-comfyui-node-values node) "\n")
                :help "The server has not supplied this node type; its saved values are read-only."))))

(defun lispm-comfyui-simple--fields ()
  "The form's fields for the workflow in the current buffer.
Field keys stay stable across redraws, undo and switching views."
  (let ((types (lispm-comfyui--types))
        (nodes (lispm-comfyui-simple--nodes)))
    (append
     `((actions :type buttons
                :buttons (("Run" . lispm-comfyui-simple-run)
                          ("Stop" . lispm-comfyui-simple-stop)
                          ("Save" . lispm-comfyui-simple-save)
                          ("Graph View" . lispm-comfyui-simple-toggle-view)
                          ("Settings" . lispm-comfyui-simple-settings)))
       (status :type status :value ,(lispm-comfyui-simple--status-text)))
     (lispm-comfyui-simple--common nodes types)
     (cl-loop for node in nodes
              append (append
                      (list (list (lispm-comfyui-simple--key 'node (lispm-comfyui-node-id node))
                                  :type 'heading :node-id (lispm-comfyui-node-id node)
                                  :label (format "%s #%s" (lispm-comfyui-node-name node types)
                                                 (lispm-comfyui-node-id node))))
                      (cl-loop for widget in (lispm-comfyui-simple--widgets node types)
                               append (lispm-comfyui-simple--widget-fields 'widget node widget types))
                      (lispm-comfyui-simple--unavailable-fields node types)
                      (cl-loop for slot in (lispm-comfyui-node-inputs node) for index from 0
                               collect (lispm-comfyui-simple--input-field node slot index types))))
     (lispm-comfyui-simple--output-fields))))

;;;; Applying changes

(defun lispm-comfyui-simple--owner ()
  "The live workflow owning this form, or signal a user error."
  (or (and (buffer-live-p lispm-comfyui-simple--workflow) lispm-comfyui-simple--workflow)
      (user-error "This workflow has been closed")))

(defun lispm-comfyui-simple--problem (key message)
  "Record MESSAGE for invalid field KEY, or clear its problem for nil."
  (setq lispm-comfyui-simple--problems (assq-delete-all key lispm-comfyui-simple--problems))
  (when message (push (cons key message) lispm-comfyui-simple--problems)))

(defun lispm-comfyui-simple--set-widget (key id name spec kind value)
  "Apply field KEY's VALUE to widget NAME of node ID, using SPEC and KIND.
Invalid numeric drafts remain in the field until complete; successful edits
use the workflow's usual transaction, modified flag and undo history."
  (unless lispm-comfyui-simple--updating
    (let ((owner (lispm-comfyui-simple--owner))
          (lispm-comfyui-simple--editing key)
          (problem (and (memq kind '(int float))
                        (lispm-comfyui-simple--number-problem spec value))))
      (unless problem
        (condition-case err
            (with-current-buffer owner
              (let* ((node (or (lispm-comfyui-node lispm-comfyui--graph id)
                               (user-error "Node #%s no longer exists" id)))
                     (typed (pcase kind
                              ((or 'int 'float) (string-to-number (string-trim value)))
                              ('boolean (if value t :false))
                              ;; Text areas reserve blank lines as their panel's height.
                              ('string (if (lispm-comfyui-spec-option spec 'multiline)
                                           (string-trim-right value) value))
                              (_ value)))
                     (before (cdr (assoc name (lispm-comfyui-node-widget-alist node (lispm-comfyui--types))))))
                (unless (equal typed before) (lispm-comfyui--set node name typed))))
          (error (setq problem (error-message-string err)))))
      (lispm-comfyui-simple--problem key problem)
      (with-current-buffer owner (lispm-comfyui-simple-refresh t)))))

(defun lispm-comfyui-simple--set-input (key id name value)
  "Relink input NAME of node ID from VALUE, a (NODE . OUTPUT), or unlink nil.
KEY identifies the form field for reporting a failed model transaction."
  (unless lispm-comfyui-simple--updating
    (let ((owner (lispm-comfyui-simple--owner))
          (lispm-comfyui-simple--editing key)
          (problem nil))
      (condition-case err
          (with-current-buffer owner
            (let* ((node (or (lispm-comfyui-node lispm-comfyui--graph id)
                             (user-error "Node #%s no longer exists" id)))
                   (source (and value
                                (or (lispm-comfyui-node lispm-comfyui--graph (car value))
                                    (seq-find (lambda (candidate)
                                                (equal (lispm-comfyui-node-id candidate) (car value)))
                                              (lispm-comfyui--io-nodes))
                                    (user-error "Source node no longer exists")))))
              (lispm-comfyui--set-link node name source (cdr value))))
        (error (setq problem (error-message-string err))))
      (lispm-comfyui-simple--problem key problem)
      (with-current-buffer owner (lispm-comfyui-simple-refresh t)))))

;;;; Keeping a form current

(defun lispm-comfyui-simple--field-shape (fields)
  "The structural parts of FIELDS, for deciding whether to rebuild the form."
  (mapcar (lambda (field)
            (cons (car field)
                  (cl-loop for (key value) on (cdr field) by #'cddr
                           unless (memq key '(:value :on-change :validate))
                           append (list key value))))
          fields))

(defun lispm-comfyui-simple--sync-text (key value)
  "Replace the displayed text of KEY with VALUE without rebuilding the form.
The current field and its offset are preserved while its duplicate changes."
  (when-let* ((widget (alist-get key (lispm-ui-form-widgets))))
    (let ((place (lispm-ui-form--place))
          (inhibit-read-only t)
          (inhibit-modification-hooks t)
          (lispm-comfyui-simple--updating t))
      (widget-value-set widget
                        (if (eq (widget-type widget) 'text)
                            (let* ((props (cdr (assq key lispm-ui-form--fields)))
                                   (lines (or (plist-get props :lines) 6))
                                   (shown (string-trim-right value)))
                              (concat shown (make-string (max 0 (- lines 1 (cl-count ?\n shown))) ?\n)))
                          value))
      (widget-setup)
      (lispm-ui-form--shape)
      (lispm-ui-form--go-to place))))

(defun lispm-comfyui-simple-refresh (&optional editing-done)
  "Refresh this workflow's existing form from the graph.
An edit's transaction postpones this until EDITING-DONE, when its callback
has recorded any validation problem.  Text and status changes update in
place; changed controls or structure rebuild once, retaining the field.
Pending invalid drafts are kept until the user repairs them."
  (when (and (buffer-live-p lispm-comfyui--simple-buffer)
             (or (not lispm-comfyui-simple--editing) editing-done))
    (let ((fields (lispm-comfyui-simple--fields))
          (title (format "ComfyUI Simple: %s" (or lispm-comfyui--name "Untitled"))))
      (with-current-buffer lispm-comfyui--simple-buffer
        (unless (equal lispm-ui-form--title title)
          (setq lispm-ui-form--title title)
          (rename-buffer (format "*%s*" title) t))
        (let* ((shape (lispm-comfyui-simple--field-shape fields))
               (rebuild (not (equal shape lispm-comfyui-simple--shape)))
               (texts nil) (statuses nil))
          (setq lispm-comfyui-simple--problems
                (seq-filter (lambda (entry) (assq (car entry) fields)) lispm-comfyui-simple--problems))
          (when lispm-comfyui-simple--problems
            (let ((status (assq 'status fields)))
              (setcdr status
                      (plist-put (cdr status) :value
                                 (concat (plist-get (cdr status) :value) "\n"
                                         (mapconcat #'cdr lispm-comfyui-simple--problems " · "))))))
          (dolist (field fields)
            (let* ((key (car field)) (props (cdr field))
                   (value (plist-get props :value))
                   (type (plist-get props :type)))
              (if (or (eq key lispm-comfyui-simple--editing)
                      (assq key lispm-comfyui-simple--problems))
                  (setcdr field (plist-put props :value (alist-get key lispm-ui-form--values)))
                (unless (equal-including-properties value (alist-get key lispm-ui-form--values))
                  (setf (alist-get key lispm-ui-form--values) value)
                  (pcase type
                    ((or 'text 'text-area) (push (cons key value) texts))
                    ('status (push (cons key value) statuses))
                    ((or 'choice 'check) (setq rebuild t)))))))
          (setq lispm-ui-form--fields fields
                lispm-comfyui-simple--shape shape)
          (if rebuild
              (lispm-ui-form-redraw)
            (dolist (entry texts) (lispm-comfyui-simple--sync-text (car entry) (cdr entry)))
            ;; A status can be replaced during text notification, with no
            ;; form-wide redraw and no widget modification callbacks.
            (dolist (entry statuses)
              (unless (lispm-ui-form--draw-in-place (car entry))
                (lispm-ui-form-redraw)))))))))

(defun lispm-comfyui-simple--closed ()
  "Detach this form when killed, leaving its workflow available in graph view."
  (when (buffer-live-p lispm-comfyui-simple--workflow)
    (with-current-buffer lispm-comfyui-simple--workflow
      (setq lispm-comfyui--simple-buffer nil
            lispm-comfyui--current-view 'graph))))

(defun lispm-comfyui-simple-buffer ()
  "Return this workflow's form, creating or refreshing it without displaying it."
  (unless (buffer-live-p lispm-comfyui--simple-buffer)
    (let* ((owner (current-buffer))
           (fields (lispm-comfyui-simple--fields))
           (title (format "ComfyUI Simple: %s" (or lispm-comfyui--name "Untitled"))))
      ;; Standalone Emacs, unlike LispM's desktop, may not yet have a palette.
      (when (and (display-graphic-p) (null lispm-ui-current-palette))
        (lispm-ui-palette-update))
      (setq lispm-comfyui--simple-buffer
            (lispm-ui-form
             title fields :instant t :mode #'lispm-comfyui-simple-mode
             :setup (lambda ()
                      (setq lispm-comfyui-simple--workflow owner
                            lispm-comfyui-simple--shape (lispm-comfyui-simple--field-shape fields)
                            lispm-ui-form-show-header nil))))))
  (lispm-comfyui-simple-refresh)
  lispm-comfyui--simple-buffer)

(defun lispm-comfyui-simple-show ()
  "Show this workflow's fixed form in the workflow's window; return the form."
  (let ((buffer (lispm-comfyui-simple-buffer)))
    (pop-to-buffer-same-window buffer)
    buffer))

;;;; Commands

(defun lispm-comfyui-simple--command (command &optional validate)
  "Call COMMAND in the owning workflow, checking unfinished fields for VALIDATE."
  (when (and validate lispm-comfyui-simple--problems)
    (user-error "%s" (cdar lispm-comfyui-simple--problems)))
  (with-current-buffer (lispm-comfyui-simple--owner) (call-interactively command)))

(defun lispm-comfyui-simple-run ()
  "Run the owning workflow, once every edited number is valid."
  (interactive) (lispm-comfyui-simple--command #'lispm-comfyui-run t))

(defun lispm-comfyui-simple-stop ()
  "Stop the owning workflow's queued or running request."
  (interactive) (lispm-comfyui-simple--command #'lispm-comfyui-stop))

(defun lispm-comfyui-simple-save ()
  "Save the owning workflow, once every edited number is valid."
  (interactive) (lispm-comfyui-simple--command #'lispm-comfyui-save t))

(defun lispm-comfyui-simple-settings ()
  "Open settings in the workflow's window."
  (interactive) (lispm-comfyui-simple--command #'lispm-comfyui-settings))

(defun lispm-comfyui-simple-undo ()
  "Undo the owning workflow's last graph change and refresh its controls."
  (interactive) (lispm-comfyui-simple--command #'lispm-comfyui-undo))

(defun lispm-comfyui-simple-redo ()
  "Redo the owning workflow's last undone graph change."
  (interactive) (lispm-comfyui-simple--command #'lispm-comfyui-redo))

(defun lispm-comfyui-simple-toggle-view ()
  "Switch from this form to the owning workflow's graph view."
  (interactive) (lispm-comfyui-simple--command #'lispm-comfyui-toggle-view))

(defun lispm-comfyui-simple-show-workflows ()
  "Go to the workflows sidebar beside this form."
  (interactive) (lispm-comfyui-simple--command #'lispm-comfyui-show-workflows))

(defun lispm-comfyui-simple--view-command (command)
  "Use COMMAND outside editable fields, and let a field insert the letter v."
  (if (widget-field-at (point)) #'self-insert-command command))

(defvar-keymap lispm-comfyui-simple-mode-map
  :doc "Commands of the simple view; field widgets retain ordinary typing keys."
  :parent lispm-ui-form-mode-map
  "v" '(menu-item "Graph View" lispm-comfyui-simple-toggle-view
                  :filter lispm-comfyui-simple--view-command)
  "C-c C-v" #'lispm-comfyui-simple-toggle-view
  "C-c C-c" #'lispm-comfyui-simple-run
  "C-c C-k" #'lispm-comfyui-simple-stop
  "C-x C-s" #'lispm-comfyui-simple-save
  "C-/" #'lispm-comfyui-simple-undo
  "C-_" #'lispm-comfyui-simple-undo
  "C-x u" #'lispm-comfyui-simple-undo
  "C-?" #'lispm-comfyui-simple-redo
  "M-_" #'lispm-comfyui-simple-redo
  "C-c C-w" #'lispm-comfyui-simple-show-workflows)

(easy-menu-define lispm-comfyui-simple-mode-menu lispm-comfyui-simple-mode-map
  "The workflow's form commands."
  '("ComfyUI"
    ["Run" lispm-comfyui-simple-run t]
    ["Stop" lispm-comfyui-simple-stop t]
    ["Save" lispm-comfyui-simple-save t]
    ["Settings…" lispm-comfyui-simple-settings t]
    "---"
    ["Undo" lispm-comfyui-simple-undo t]
    ["Redo" lispm-comfyui-simple-redo t]
    ("View" ["Graph View" lispm-comfyui-simple-toggle-view t]
     ["Show Workflows" lispm-comfyui-simple-show-workflows t])))

(define-derived-mode lispm-comfyui-simple-mode lispm-ui-form-mode "ComfyUI Simple"
  "A ComfyUI workflow as fixed graphical controls with vertical scrolling.
TAB and S-TAB move between fields.  Changes edit the workflow immediately;
C-/ undoes a graph change.  C-c C-v switches to the node canvas.
\\{lispm-comfyui-simple-mode-map}"
  (setq-local horizontal-scroll-bar nil
              auto-hscroll-mode nil)
  (add-hook 'window-size-change-functions #'lispm-comfyui--window-changed nil t)
  (add-hook 'window-buffer-change-functions #'lispm-comfyui--window-changed nil t)
  (add-hook 'kill-buffer-hook #'lispm-comfyui-simple--closed nil t))

(provide 'lispm-comfyui-simple)
;;; lispm-comfyui-simple.el ends here
