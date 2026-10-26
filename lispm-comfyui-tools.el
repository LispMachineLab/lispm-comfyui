;;; lispm-comfyui-tools.el --- LispM's agents edit and run ComfyUI workflows  -*- lexical-binding: t; -*-

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

;; Tools of LispM Agent's, for its agents to work on the workflow the
;; user sees: read it, find node types, change it, as the user would,
;; each change one step to undo, and run it, the images it makes
;; coming back to the agent.  lispm-comfyui.el loads this file once
;; LispM's tools are loaded.  The tools are deferred: an agent finds
;; them with search_tools, by "comfyui".

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-tools)
(require 'lispm-comfyui)

(defun lispm-comfyui-tools--buffer (&optional create)
  "The buffer of the workflow last shown; if CREATE, a new one if none is."
  (or (car (lispm-comfyui--canvas-buffers))
      (and create
           (let ((buffer (generate-new-buffer (lispm-comfyui--buffer-name nil))))
             (with-current-buffer buffer (lispm-comfyui-mode))
             (display-buffer buffer)
             buffer))
      (error "No ComfyUI workflow is open; the user opens one with M-x lispm-comfyui")))

(defun lispm-comfyui-tools--value (value)
  "VALUE, a widget's, as the model reads it."
  (cond ((eq value t) "true")
        ((memq value '(:false nil)) "false")
        ((eq value :null) "null")
        ((stringp value) (json-serialize value))
        (t (format "%s" value))))

(defun lispm-comfyui-tools-describe (buffer)
  "The workflow of BUFFER, as text for a model."
  (with-current-buffer buffer
    (let* ((types (lispm-comfyui--types))
           (graph lispm-comfyui--graph)
           (nodes (lispm-comfyui-graph-nodes graph)))
      (with-temp-buffer
        (insert (format "Workflow %s%s, %d nodes, on the ComfyUI server at %s.\n"
                        (or lispm-comfyui--name "Untitled (not saved)")
                        (if lispm-comfyui--modified " (changed since saved)" "")
                        (length nodes) lispm-comfyui-url))
        (when-let* ((run (car lispm-comfyui--runs)))
          (insert (format "Last run: %s%s.\n" (plist-get run :status)
                          (if (plist-get run :message) (concat ", " (plist-get run :message)) ""))))
        (when lispm-comfyui--selected
          (insert (format "The user has node #%s chosen.\n" lispm-comfyui--selected)))
        (dolist (node nodes)
          (let ((name (lispm-comfyui-node-name node types)))
            (insert (format "\n#%s %s%s%s\n" (lispm-comfyui-node-id node) (lispm-comfyui-node-type node)
                            (if (equal name (lispm-comfyui-node-type node)) "" (format " \"%s\"" name))
                            (pcase (lispm-comfyui-node-mode node) (4 " [bypassed]") (2 " [muted]") (_ ""))))
            (cl-loop for slot in (lispm-comfyui-node-inputs node)
                     for index from 0
                     for link = (lispm-comfyui-input-link graph node index)
                     when link
                     do (let ((from (lispm-comfyui-node graph (lispm-comfyui-link-from link))))
                          (insert (format "  %s <- #%s.%s\n" (lispm-comfyui-slot-name slot)
                                          (lispm-comfyui-link-from link)
                                          (if from (lispm-comfyui-slot-name
                                                    (nth (lispm-comfyui-link-from-slot link)
                                                         (lispm-comfyui-node-outputs from)))
                                            (lispm-comfyui-link-from-slot link)))))
                     unless (or link (lispm-comfyui-slot-widget slot))
                     do (insert (format "  %s: %s, not linked\n" (lispm-comfyui-slot-name slot)
                                        (lispm-comfyui-slot-type slot))))
            (when (and (null (gethash (lispm-comfyui-node-type node) types))
                       (lispm-comfyui-node-values node)
                       (not (lispm-comfyui-values-by-name-p (lispm-comfyui-node-values node))))
              ;; A note, or a node of a type the server has not.
              (insert (format "  values = %s\n" (mapconcat #'lispm-comfyui-tools--value
                                                           (lispm-comfyui-node-values node) ", "))))
            (let ((linked (cl-loop for slot in (lispm-comfyui-node-inputs node)
                                   for index from 0
                                   when (lispm-comfyui-input-link graph node index)
                                   collect (lispm-comfyui-slot-name slot))))
              (pcase-dolist (`(,name . ,value) (lispm-comfyui-node-widget-alist node types))
                (unless (or (member name linked) (string-suffix-p "/control" name)
                            (and (consp value) (consp (car value))))
                  (let ((control (cdr (assoc (concat name "/control")
                                             (lispm-comfyui-node-widget-alist node types)))))
                    (insert (format "  %s = %s%s\n" name (lispm-comfyui-tools--value value)
                                    (if (and control (not (equal control "fixed")))
                                        (format " (%s after each run)" control) "")))))))
            (cl-loop for slot in (lispm-comfyui-node-outputs node)
                     for index from 0
                     for links = (lispm-comfyui-output-links graph node index)
                     do (insert (format "  -> %s: %s%s\n" (lispm-comfyui-slot-name slot)
                                        (lispm-comfyui-slot-type slot)
                                        (if links
                                            (concat " to "
                                                    (mapconcat (lambda (link)
                                                                 (let ((to (lispm-comfyui-node graph (lispm-comfyui-link-to link))))
                                                                   (format "#%s.%s" (lispm-comfyui-link-to link)
                                                                           (if to (lispm-comfyui-slot-name
                                                                                   (nth (lispm-comfyui-link-to-slot link)
                                                                                        (lispm-comfyui-node-inputs to)))
                                                                             "?"))))
                                                               links ", "))
                                          ""))))
            (when-let* ((failure (cdr (assoc (format "%s" (lispm-comfyui-node-id node))
                                             lispm-comfyui--errors))))
              (insert (format "  ERROR: %s\n" failure)))))
        (buffer-string)))))

(defun lispm-comfyui-tools--spec-text (name spec)
  "A line telling of the input NAME of SPEC."
  (let* ((kind (lispm-comfyui-widget-kind spec))
         (options (lispm-comfyui-spec-options spec))
         (parts nil))
    (pcase kind
      ((or 'combo 'dynamic)
       (let ((choices (lispm-comfyui-widget-choices spec)))
         (push (format "one of %d: %s%s" (length choices)
                       (mapconcat #'lispm-comfyui-tools--value (seq-take choices 40) ", ")
                       (if (> (length choices) 40) ", …" ""))
               parts)))
      ('nil (push (format "socket of %s" (lispm-comfyui-socket-type spec)) parts))
      (_ (push (format "%s" (lispm-comfyui-spec-type spec)) parts)
         (when-let* ((min (alist-get 'min options))) (push (format "min %s" min) parts))
         (when-let* ((max (alist-get 'max options))) (push (format "max %s" max) parts))
         (when (lispm-comfyui-spec-option spec 'multiline) (push "many lines" parts))))
    (when (and kind (assq 'default options))
      (push (format "default %s" (lispm-comfyui-tools--value (alist-get 'default options))) parts))
    (when (lispm-comfyui-spec-option spec 'control_after_generate)
      (push "changes after each run, as its NAME/control says" parts))
    (when-let* ((tooltip (lispm-comfyui-spec-option spec 'tooltip)))
      (push (car (split-string tooltip "\n")) parts))
    (format "    %s: %s\n" name (string-join (nreverse parts) "; "))))

(defun lispm-comfyui-tools--type-text (name definition)
  "The full specification of the node type NAME, of DEFINITION."
  (concat
   (format "%s, \"%s\", in %s%s\n" name (lispm-comfyui-type-title definition)
           (or (lispm-comfyui--field definition 'category) "no category")
           (if (lispm-comfyui--field definition 'output_node) ", an output node" ""))
   (if-let* ((description (lispm-comfyui--field definition 'description))
             ((not (string-empty-p description))))
       (format "  %s\n" (car (split-string description "\n")))
     "")
   "  Inputs:\n"
   (mapconcat (pcase-lambda (`(,input ,spec ,section))
                (concat (lispm-comfyui-tools--spec-text input spec)
                        (if (eq section 'optional) "      (optional)\n" "")
                        (when (eq (lispm-comfyui-widget-kind spec) 'dynamic)
                          (mapconcat (lambda (choice)
                                       (mapconcat (pcase-lambda (`(,sub ,sub-spec ,_))
                                                    (lispm-comfyui-tools--spec-text
                                                     (format "%s.%s (when %s is %s)" input sub input choice)
                                                     sub-spec))
                                                  (lispm-comfyui--dynamic-inputs spec choice) ""))
                                     (lispm-comfyui-widget-choices spec) ""))))
              (lispm-comfyui-type-inputs definition) "")
   "  Outputs:\n"
   (mapconcat (pcase-lambda (`(,output ,type)) (format "    %s: %s\n" output type))
              (lispm-comfyui-type-outputs definition) "")))

(defun lispm-comfyui-tools-find-types (types query &optional limit)
  "Text telling of the node types of TYPES QUERY finds, at most LIMIT.
QUERY is exact names separated by commas, for their specifications,
or words, for the types whose name, title, category or description
has them all."
  (let* ((names (mapcar #'string-trim (split-string query "," t)))
         (exact (and names (seq-every-p (lambda (name) (gethash name types)) names))))
    (if exact
        (mapconcat (lambda (name) (lispm-comfyui-tools--type-text name (gethash name types)))
                   names "\n")
      (let* ((words (split-string (downcase query) "[ ,]+" t))
             (found nil))
        (maphash
         (lambda (name definition)
           (let* ((title (or (lispm-comfyui-type-title definition) ""))
                  (category (or (lispm-comfyui--field definition 'category) ""))
                  (description (or (lispm-comfyui--field definition 'description) ""))
                  (text (downcase (string-join (list name title category description) " "))))
             (when (seq-every-p (lambda (word) (string-search word text)) words)
               (push (list (+ (if (seq-every-p (lambda (word) (string-search word (downcase name))) words) 0 2)
                              (if (seq-every-p (lambda (word) (string-search word (downcase title))) words) 0 1)
                              (/ (length name) 100.0))
                           name title category description)
                     found))))
         types)
        (setq found (sort found (lambda (a b) (< (car a) (car b)))))
        (if (null found)
            (format "No node type has all of: %s." query)
          (concat
           (format "%d node types found%s; give exact names, separated by commas, for their inputs and outputs:\n"
                   (length found) (if (> (length found) (or limit 25)) (format ", the first %d" (or limit 25)) ""))
           (mapconcat (pcase-lambda (`(,_ ,name ,title ,category ,description))
                        (format "%s%s [%s]%s\n" name
                                (if (equal title name) "" (format " \"%s\"" title))
                                category
                                (if (string-empty-p description) ""
                                  (concat ": " (truncate-string-to-width
                                                (car (split-string description "\n")) 120 nil nil "…")))))
                      (seq-take found (or limit 25)) "")))))))

(defun lispm-comfyui-tools--node-ref (references value)
  "The node number VALUE names: a number, or a name an earlier add gave.
REFERENCES maps those names to numbers."
  (cond ((numberp value) value)
        ((and (stringp value) (gethash value references)))
        ((and (stringp value) (string-match "\\`#?\\([0-9]+\\)\\'" value))
         (string-to-number (match-string 1 value)))
        (t (signal 'lispm-comfyui-error (list (format "No node %S" value))))))

(defun lispm-comfyui-tools-apply (buffer operations)
  "Apply OPERATIONS, a list of alists as the model sends them, to BUFFER's graph.
All or none: one failing leaves the graph as it was.  Returns what was
done, a string."
  (with-current-buffer buffer
    (let* ((types (lispm-comfyui--need-types))
           (references (make-hash-table :test #'equal))
           (graph lispm-comfyui--graph)
           (added nil)
           (done nil)
           (node-of (lambda (operation key)
                      (or (lispm-comfyui-node graph (lispm-comfyui-tools--node-ref
                                                     references (alist-get key operation)))
                          (signal 'lispm-comfyui-error
                                  (list (format "No node %s" (alist-get key operation))))))))
      (lispm-comfyui--change
        (seq-doseq (operation operations)
          (let ((op (alist-get 'op operation)))
            (pcase op
              ("add"
               (let ((node (lispm-comfyui-graph-add-node graph types (alist-get 'type operation))))
                 (when-let* ((title (alist-get 'title operation)))
                   (setf (lispm-comfyui-node-title node) title))
                 (when-let* ((ref (alist-get 'ref operation)))
                   (puthash ref (lispm-comfyui-node-id node) references))
                 (pcase-dolist (`(,name . ,value) (alist-get 'values operation))
                   (lispm-comfyui-set-widget node types (symbol-name name) value))
                 (push node added)
                 (push (format "added #%s %s%s" (lispm-comfyui-node-id node) (lispm-comfyui-node-type node)
                               (if (alist-get 'ref operation) (format " as %s" (alist-get 'ref operation)) ""))
                       done)))
              ("set"
               (let ((node (funcall node-of operation 'node)))
                 (lispm-comfyui-set-widget node types (alist-get 'name operation) (alist-get 'value operation))
                 (push (format "set #%s %s" (lispm-comfyui-node-id node) (alist-get 'name operation)) done)))
              ("connect"
               (let* ((from (funcall node-of operation 'from_node))
                      (to (funcall node-of operation 'to_node))
                      (output (alist-get 'from_output operation)))
                 (lispm-comfyui-graph-connect graph types from (if (stringp output) output (or output 0))
                                        to (alist-get 'to_input operation))
                 (push (format "linked #%s.%s to #%s.%s" (lispm-comfyui-node-id from) (or output 0)
                               (lispm-comfyui-node-id to) (alist-get 'to_input operation))
                       done)))
              ("disconnect"
               (let* ((node (funcall node-of operation 'node))
                      (index (or (lispm-comfyui-input-index node (alist-get 'input operation))
                                 (signal 'lispm-comfyui-error
                                         (list (format "Node #%s has no input %s" (lispm-comfyui-node-id node)
                                                       (alist-get 'input operation)))))))
                 (lispm-comfyui-graph-disconnect graph node index)
                 (push (format "unlinked #%s.%s" (lispm-comfyui-node-id node) (alist-get 'input operation)) done)))
              ("remove"
               (let ((node (funcall node-of operation 'node)))
                 (lispm-comfyui-remove-node graph node)
                 (setq added (delq node added))
                 (push (format "removed #%s" (lispm-comfyui-node-id node)) done)))
              ("mode"
               (let ((node (funcall node-of operation 'node))
                     (mode (alist-get 'mode operation)))
                 (setf (lispm-comfyui-node-mode node)
                       (pcase mode ("bypass" 4) ("mute" 2) ("normal" 0)
                              (_ (signal 'lispm-comfyui-error
                                         (list (format "A mode is normal, bypass or mute, not %s" mode))))))
                 (push (format "made #%s %s" (lispm-comfyui-node-id node) mode) done)))
              ("title"
               (let ((node (funcall node-of operation 'node)))
                 (setf (lispm-comfyui-node-title node) (alist-get 'title operation))
                 (push (format "titled #%s" (lispm-comfyui-node-id node)) done)))
              ("move"
               (let ((node (funcall node-of operation 'node)))
                 (setf (lispm-comfyui-node-x node) (alist-get 'x operation)
                       (lispm-comfyui-node-y node) (alist-get 'y operation))
                 (push (format "moved #%s" (lispm-comfyui-node-id node)) done)))
              (_ (signal 'lispm-comfyui-error (list (format "No operation %S" op)))))))
        ;; New nodes go where their links lead, those placed by a
        ;; move staying where they were put.
        (dolist (node (reverse added))
          (unless (seq-find (lambda (operation)
                              (and (equal (alist-get 'op operation) "move")
                                   (equal (ignore-errors (lispm-comfyui-tools--node-ref
                                                          references (alist-get 'node operation)))
                                          (lispm-comfyui-node-id node))))
                            operations)
            (lispm-comfyui-place graph node (lispm-comfyui--node))
            (lispm-comfyui--clear-place node)))
        (when added (setq lispm-comfyui--selected (lispm-comfyui-node-id (car added))
                          lispm-comfyui--focus 0)))
      (when added (lispm-comfyui--fit) (lispm-comfyui--draw))
      (let ((missing (lispm-comfyui-tools--missing graph types)))
        (concat (string-join (nreverse done) "; ") "."
                (if missing (concat "\nStill missing: " (string-join missing "; ") ".") ""))))))

(defun lispm-comfyui-tools--missing (graph types)
  "The required inputs of GRAPH's nodes, of TYPES, nothing feeds."
  (cl-loop for node in (lispm-comfyui-graph-nodes graph)
           for definition = (gethash (lispm-comfyui-node-type node) types)
           when (and definition (eql (lispm-comfyui-node-mode node) 0))
           append (cl-loop for (name spec section) in (lispm-comfyui-type-inputs definition)
                           for index = (lispm-comfyui-input-index node name)
                           when (and (eq section 'required) (null (lispm-comfyui-widget-kind spec))
                                     (not (and index (lispm-comfyui-input-link graph node index))))
                           collect (format "#%s.%s (%s)" (lispm-comfyui-node-id node) name
                                           (lispm-comfyui-socket-type spec)))))

(lispm-deftool comfyui_workflow ()
  "Read the ComfyUI workflow open in LispM, the one the user sees: its nodes, each `#ID TYPE \"TITLE\"`, the inputs linked into each (`name <- #ID.output`), its widgets' values (`name = value`), its outputs and where they go, and errors of the last run.  Opens a new, empty workflow if none is open."
  :deferred t
  (let ((buffer (lispm-comfyui-tools--buffer t)))
    (with-current-buffer buffer (lispm-comfyui--need-types))
    (lispm-comfyui-tools-describe buffer)))

(lispm-deftool comfyui_node_types ((query string "Words to look for in node types' names, titles, categories and descriptions, as \"upscale model\"; or exact type names separated by commas, as \"KSampler,VAEDecode\", for their full inputs and outputs.")
                                   (limit integer "How many types to list at most, 25 by default." :optional t))
  "Find the node types of the ComfyUI server, and read their inputs, widgets and outputs: what `comfyui_edit' needs to add and link nodes."
  :deferred t
  (let ((buffer (lispm-comfyui-tools--buffer t)))
    (with-current-buffer buffer
      (lispm-comfyui-tools-find-types (lispm-comfyui--need-types) query limit))))

(lispm-deftool comfyui_edit ((operations array "The changes, in order, each an object with `op`: {op: \"add\", type, ref?, title?, values?: {widget: value}} adds a node, which later operations name by its ref; {op: \"set\", node, name, value} sets a widget (name \"seed/control\" sets how a seed changes after each run: fixed, increment, decrement or randomize); {op: \"connect\", from_node, from_output (name or index), to_node, to_input} links an output to an input, or to a widget, which it then feeds; {op: \"disconnect\", node, input}; {op: \"remove\", node}; {op: \"mode\", node, mode: normal|bypass|mute}; {op: \"title\", node, title}; {op: \"move\", node, x, y}.  A node is its number or an add's ref."
                                         :items ((type . "object"))))
  "Change the ComfyUI workflow open in LispM, as the user sees it change: all the operations, or none if one fails, as one step the user can undo.  New nodes are placed beside those they are linked to.  Says what was done and which required inputs are still not linked."
  :permission write
  :deferred t
  (lispm-comfyui-tools-apply (lispm-comfyui-tools--buffer t) operations))

(lispm-deftool comfyui_run ()
  "Run the ComfyUI workflow open in LispM on its server, as the user would with r, and wait until it is done: returns how it went and the images it made, which you see."
  :permission web
  :deferred t
  :async t
  (let ((buffer (lispm-comfyui-tools--buffer)))
    (with-current-buffer buffer
      (lispm-comfyui-run
       (lambda (run)
         (let ((files (plist-get run :files)))
           (funcall done
                    (pcase (plist-get run :status)
                      ('done (format "Done in %.1f s; %d images made%s."
                                     (float-time (time-since (plist-get run :start)))
                                     (length files)
                                     (if files (concat ": " (mapconcat #'file-name-nondirectory files ", ")) "")))
                      ('stopped "The run was stopped.")
                      (_ (format "The run failed: %s\n%s" (plist-get run :message)
                                 (with-current-buffer buffer
                                   (mapconcat (lambda (entry) (format "#%s: %s" (car entry) (cdr entry)))
                                              lispm-comfyui--errors "\n")))))
                    (not (eq (plist-get run :status) 'done))
                    files)))))))

(lispm-deftool comfyui_open ((name string "The workflow's name in the server's library; empty to list the names." :optional t))
  "Open a workflow of the ComfyUI server's library in LispM, for the user to see, or list the library's workflows."
  :deferred t
  (if (or (null name) (string-empty-p name))
      (let ((names nil) (failure nil))
        (lispm-comfyui-list-workflows (lambda (list error) (setq names (or list t) failure error)))
        (with-timeout (30 (error "The server did not answer"))
          (while (not names) (accept-process-output nil 0.05)))
        (if failure (error "%s" failure)
          (concat "Workflows: " (string-join (if (listp names) names nil) ", "))))
    (when-let* ((buffer (car (lispm-comfyui--canvas-buffers)))
                ((buffer-local-value 'lispm-comfyui--modified buffer))
                ((buffer-local-value 'lispm-comfyui--graph buffer)))
      (error "The workflow open has changes not saved; ask the user to save it first"))
    (save-window-excursion (lispm-comfyui-open name))
    (let ((buffer (car (lispm-comfyui--canvas-buffers))))
      (display-buffer buffer)
      (lispm-comfyui-tools-describe buffer))))

(lispm-deftool comfyui_save ((name string "The name to save it under; the one it has by default." :optional t))
  "Save the ComfyUI workflow open in LispM in the server's library."
  :permission write
  :deferred t
  (let ((buffer (lispm-comfyui-tools--buffer)))
    (with-current-buffer buffer
      (unless (or (and name (not (string-empty-p name))) lispm-comfyui--name)
        (error "The workflow has no name yet: give one"))
      (lispm-comfyui-save (and name (not (string-empty-p name)) name))
      (format "Saving as %s." (or name lispm-comfyui--name)))))

(provide 'lispm-comfyui-tools)
;;; lispm-comfyui-tools.el ends here
