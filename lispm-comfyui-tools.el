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
             (display-buffer (with-current-buffer buffer (lispm-comfyui--view-buffer)))
             buffer))
      (error "No ComfyUI workflow is open; the user opens one with M-x lispm-comfyui")))

(defun lispm-comfyui-tools--value (value)
  "VALUE, a widget's, as the model reads it."
  (cond ((eq value t) "true")
        ((memq value '(:false nil)) "false")
        ((eq value :null) "null")
        ((stringp value) (json-serialize value))
        (t (format "%s" value))))

(defun lispm-comfyui-tools--source-text (graph link prefix subgraph)
  "Where LINK, into a node of GRAPH, numbered after PREFIX, comes from.
SUBGRAPH is the definition GRAPH is the inside of, if any."
  (let ((from (lispm-comfyui-link-from link))
        (slot (lispm-comfyui-link-from-slot link)))
    (if (and subgraph (eql from lispm-comfyui-input-node))
        (format "%sin.%s" prefix
                (lispm-comfyui-slot-name (nth slot (lispm-comfyui-subgraph-inputs subgraph))))
      (let ((node (lispm-comfyui-node graph from)))
        (format "#%s%s.%s" prefix from
                (if node (lispm-comfyui-slot-name (nth slot (lispm-comfyui-node-outputs node))) slot))))))

(defun lispm-comfyui-tools--target-text (graph link prefix subgraph)
  "Where LINK, out of a node of GRAPH, numbered after PREFIX, goes.
SUBGRAPH is the definition GRAPH is the inside of, if any."
  (let ((to (lispm-comfyui-link-to link))
        (slot (lispm-comfyui-link-to-slot link)))
    (if (and subgraph (eql to lispm-comfyui-output-node))
        (format "%sout.%s" prefix
                (lispm-comfyui-slot-name (nth slot (lispm-comfyui-subgraph-outputs subgraph))))
      (let ((node (lispm-comfyui-node graph to)))
        (format "#%s%s.%s" prefix to
                (if node (lispm-comfyui-slot-name (nth slot (lispm-comfyui-node-inputs node))) "?"))))))

(defun lispm-comfyui-tools--node-text (graph node types prefix subgraph errors)
  "Lines telling of NODE of GRAPH, of TYPES, numbered after PREFIX.
SUBGRAPH is the definition GRAPH is the inside of, if any; ERRORS what
went wrong at nodes, by their numbers in the prompt."
  (with-temp-buffer
    (let* ((name (lispm-comfyui-node-name node types))
           (definition (gethash (lispm-comfyui-node-type node) types))
           (inner (alist-get 'lispm-subgraph definition))
           (type (if inner "subgraph" (lispm-comfyui-node-type node))))
      (insert (format "\n#%s%s %s%s%s\n" prefix (lispm-comfyui-node-id node) type
                      (if (equal name type) "" (format " \"%s\"" name))
                      (pcase (lispm-comfyui-node-mode node) (4 " [bypassed]") (2 " [muted]") (_ ""))))
      (cl-loop for slot in (lispm-comfyui-node-inputs node)
               for index from 0
               for link = (lispm-comfyui-input-link graph node index)
               when link
               do (insert (format "  %s <- %s\n" (lispm-comfyui-slot-name slot)
                                  (lispm-comfyui-tools--source-text graph link prefix subgraph)))
               unless (or link (lispm-comfyui-slot-widget slot))
               do (insert (format "  %s: %s, not linked\n" (lispm-comfyui-slot-name slot)
                                  (lispm-comfyui-slot-type slot))))
      (when (and (null definition) (consp (lispm-comfyui-node-values node))
                 (not (keywordp (car (lispm-comfyui-node-values node)))))
        ;; A note, or a node of a type the server has not.
        (insert (format "  values = %s\n" (mapconcat #'lispm-comfyui-tools--value
                                                     (lispm-comfyui-node-values node) ", "))))
      (let ((linked (cl-loop for slot in (lispm-comfyui-node-inputs node)
                             for index from 0
                             when (lispm-comfyui-input-link graph node index)
                             collect (lispm-comfyui-slot-name slot)))
            (alist (lispm-comfyui-node-widget-alist node types)))
        (pcase-dolist (`(,name . ,value) alist)
          (unless (or (member name linked) (string-suffix-p "/control" name)
                      (and (consp value) (consp (car value))))
            (let ((control (cdr (assoc (concat name "/control") alist))))
              (insert (format "  %s = %s%s\n" name (lispm-comfyui-tools--value value)
                              (if (and control (not (equal control "fixed")))
                                  (format " (%s after each run)" control) "")))))))
      (cl-loop for slot in (lispm-comfyui-node-outputs node)
               for index from 0
               for links = (lispm-comfyui-output-links graph node index)
               do (insert (format "  -> %s: %s%s\n" (lispm-comfyui-slot-name slot)
                                  (lispm-comfyui-slot-type slot)
                                  (if links
                                      (concat " to " (mapconcat (lambda (link)
                                                                  (lispm-comfyui-tools--target-text
                                                                   graph link prefix subgraph))
                                                                links ", "))
                                    ""))))
      (when-let* ((failure (cdr (assoc (format "%s%s" prefix (lispm-comfyui-node-id node)) errors))))
        (insert (format "  ERROR: %s\n" failure))))
    (buffer-string)))

(defun lispm-comfyui-tools-describe (buffer)
  "The workflow of BUFFER, as text for a model.
Nodes inside subgraphs are numbered with the path to them, as #12:5,
the inside of each subgraph told once."
  (with-current-buffer buffer
    (let ((types (lispm-comfyui--types))
          (root lispm-comfyui--root)
          (errors lispm-comfyui--errors)
          (name lispm-comfyui--name)
          (modified lispm-comfyui--modified)
          (path (mapconcat (lambda (id) (format "%s:" id)) lispm-comfyui--path ""))
          (selected lispm-comfyui--selected)
          (run (car lispm-comfyui--runs))
          (told nil))
      (with-temp-buffer
        (insert (format "Workflow %s%s, %d nodes, on the ComfyUI server at %s.\n"
                        (or name "Untitled (not saved)") (if modified " (changed since saved)" "")
                        (length (lispm-comfyui-graph-nodes root)) lispm-comfyui-url))
        (when run
          (insert (format "Last run: %s%s.\n" (plist-get run :status)
                          (if (plist-get run :message) (concat ", " (plist-get run :message)) ""))))
        (unless (string-empty-p path)
          (insert (format "The user is looking inside the subgraph of #%s.\n" (string-remove-suffix ":" path))))
        (when selected
          (insert (format "The user has node #%s%s chosen.\n" path selected)))
        (cl-labels
            ((describe (graph prefix subgraph)
               (dolist (node (lispm-comfyui-graph-nodes graph))
                 (insert (lispm-comfyui-tools--node-text graph node types prefix subgraph errors))
                 (when-let* ((inner (lispm-comfyui-node-subgraph root node)))
                   (let ((here (format "%s%s:" prefix (lispm-comfyui-node-id node))))
                     (if (assq inner told)
                         (insert (format "  inside: the subgraph \"%s\", told inside #%s\n"
                                         (lispm-comfyui-subgraph-name inner)
                                         (string-remove-suffix ":" (cdr (assq inner told)))))
                       (push (cons inner here) told)
                       (insert (format "\nInside #%s, the subgraph \"%s\", the same in all its nodes: inputs %s; outputs %s.\n"
                                       (string-remove-suffix ":" here) (lispm-comfyui-subgraph-name inner)
                                       (mapconcat (lambda (slot) (format "%sin.%s: %s" here (lispm-comfyui-slot-name slot)
                                                                         (lispm-comfyui-slot-type slot)))
                                                  (lispm-comfyui-subgraph-inputs inner) ", ")
                                       (mapconcat (lambda (slot) (format "%sout.%s: %s" here (lispm-comfyui-slot-name slot)
                                                                         (lispm-comfyui-slot-type slot)))
                                                  (lispm-comfyui-subgraph-outputs inner) ", ")))
                       (describe (lispm-comfyui-subgraph-graph inner) here inner)
                       (insert (format "End of the inside of #%s.\n" (string-remove-suffix ":" here)))))))))
          (describe root "" nil))
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

(defun lispm-comfyui-tools--resolve (root references value)
  "The node VALUE names in ROOT's workflow, (GRAPH . NODE).
VALUE is a number; a name an earlier add gave, in REFERENCES; or a
path, \"12:5\", node 5 inside the subgraph of node 12, with \"12:in\" and
\"12:out\" the nodes of that subgraph's inputs and outputs."
  (let ((fail (lambda () (signal 'lispm-comfyui-error (list (format "No node %s" value))))))
    (cond
     ((numberp value) (cons root (or (lispm-comfyui-node root value) (funcall fail))))
     ((and (stringp value) (gethash value references)))
     ((stringp value)
      (let ((graph root) (subgraph nil) (node nil)
            (parts (split-string (string-remove-prefix "#" (string-trim value)) ":")))
        (while parts
          (let ((part (pop parts)))
            (cond
             ((and (null parts) subgraph (member part '("in" "out")))
              (setq node (lispm-comfyui-subgraph-io-node subgraph (if (equal part "in") 'input 'output))))
             ((string-match-p "\\`[0-9]+\\'" part)
              (setq node (or (lispm-comfyui-node graph (string-to-number part)) (funcall fail)))
              (when parts
                (setq subgraph (or (lispm-comfyui-node-subgraph root node) (funcall fail))
                      graph (lispm-comfyui-subgraph-graph subgraph))))
             (t (funcall fail)))))
        (cons graph node)))
     (t (funcall fail)))))

(defun lispm-comfyui-tools--io-slot (root graph node name type which)
  "The index of the slot NAME of NODE, a subgraph's WHICH, made if new.
NODE stands for the inputs or the outputs of the subgraph GRAPH is the
inside of, in ROOT; a new slot is of TYPE.  Returns (NODE . INDEX),
NODE made anew if a slot was."
  (let* ((slots (if (eq which 'input) (lispm-comfyui-node-outputs node) (lispm-comfyui-node-inputs node)))
         (index (seq-position slots name (lambda (slot name) (equal (lispm-comfyui-slot-name slot) name)))))
    (if index
        (cons node index)
      (let* ((subgraph (seq-find (lambda (subgraph) (eq (lispm-comfyui-subgraph-graph subgraph) graph))
                                 (lispm-comfyui-graph-subgraphs root)))
             (index (lispm-comfyui-subgraph-add-slot root subgraph which name type)))
        (cons (lispm-comfyui-subgraph-io-node subgraph which) index)))))

(defun lispm-comfyui-tools-apply (buffer operations)
  "Apply OPERATIONS, alists as the model sends them, to BUFFER's workflow.
All or none: one failing leaves the workflow as it was.  Returns what
was done, a string."
  (with-current-buffer buffer
    (let* ((types (lispm-comfyui--need-types))
           (server (lispm-comfyui-cached-types))
           (root lispm-comfyui--root)
           (references (make-hash-table :test #'equal))
           (added nil)
           (done nil)
           (resolve (lambda (operation key)
                      (lispm-comfyui-tools--resolve root references (alist-get key operation))))
           (subgraph-type (lambda (name)
                            (or (and (gethash name types) name)
                                (when-let* ((subgraph (seq-find (lambda (subgraph)
                                                                  (equal (lispm-comfyui-subgraph-name subgraph) name))
                                                                (lispm-comfyui-graph-subgraphs root))))
                                  (lispm-comfyui-subgraph-id subgraph))
                                name))))
      (lispm-comfyui--change
        (seq-doseq (operation operations)
          (setq types (lispm-comfyui-graph-types root server))
          (let ((op (alist-get 'op operation)))
            (pcase op
              ("add"
               (let* ((graph (if (alist-get 'in operation)
                                 (let ((place (lispm-comfyui-tools--resolve
                                               root references (format "%s:in" (alist-get 'in operation)))))
                                   (car place))
                               root))
                      (node (lispm-comfyui-graph-add-node graph types
                                                          (funcall subgraph-type (alist-get 'type operation)))))
                 (when-let* ((title (alist-get 'title operation)))
                   (setf (lispm-comfyui-node-title node) title))
                 (when-let* ((ref (alist-get 'ref operation)))
                   (puthash ref (cons graph node) references))
                 (pcase-dolist (`(,name . ,value) (alist-get 'values operation))
                   (lispm-comfyui-set-widget node types (symbol-name name) value))
                 (push (cons graph node) added)
                 (push (format "added #%s%s %s%s"
                               (if (alist-get 'in operation) (format "%s:" (alist-get 'in operation)) "")
                               (lispm-comfyui-node-id node) (alist-get 'type operation)
                               (if (alist-get 'ref operation) (format " as %s" (alist-get 'ref operation)) ""))
                       done)))
              ("set"
               (let ((node (cdr (funcall resolve operation 'node))))
                 (lispm-comfyui-set-widget node types (alist-get 'name operation) (alist-get 'value operation))
                 (push (format "set %s %s" (alist-get 'node operation) (alist-get 'name operation)) done)))
              ("connect"
               (pcase-let* ((`(,graph . ,from) (funcall resolve operation 'from_node))
                            (`(,to-graph . ,to) (funcall resolve operation 'to_node))
                            (output (alist-get 'from_output operation))
                            (input (alist-get 'to_input operation)))
                 (unless (eq graph to-graph)
                   (signal 'lispm-comfyui-error
                           (list "Linked nodes must be in the same graph: link through a subgraph's in or out")))
                 ;; A subgraph's input or output named anew is made.
                 (when (eql (lispm-comfyui-node-id from) lispm-comfyui-input-node)
                   (let ((made (lispm-comfyui-tools--io-slot
                                root graph from (format "%s" (or output input)) 
                                (lispm-comfyui-input-type to types (lispm-comfyui-ensure-input to types input))
                                'input)))
                     (setq from (car made) output (cdr made))))
                 (when (eql (lispm-comfyui-node-id to) lispm-comfyui-output-node)
                   (let* ((index (if (integerp output) output (lispm-comfyui-output-index from output)))
                          (made (lispm-comfyui-tools--io-slot
                                 root graph to (format "%s" input)
                                 (lispm-comfyui-slot-type (nth (or index 0) (lispm-comfyui-node-outputs from)))
                                 'output)))
                     (setq to (car made))))
                 (lispm-comfyui-graph-connect graph types from (if (stringp output) output (or output 0))
                                              to input)
                 (push (format "linked %s.%s to %s.%s" (alist-get 'from_node operation) (or output 0)
                               (alist-get 'to_node operation) input)
                       done)))
              ("disconnect"
               (pcase-let* ((`(,graph . ,node) (funcall resolve operation 'node))
                            (index (or (lispm-comfyui-input-index node (alist-get 'input operation))
                                       (signal 'lispm-comfyui-error
                                               (list (format "Node %s has no input %s" (alist-get 'node operation)
                                                             (alist-get 'input operation)))))))
                 (lispm-comfyui-graph-disconnect graph node index)
                 (push (format "unlinked %s.%s" (alist-get 'node operation) (alist-get 'input operation)) done)))
              ("remove"
               (pcase-let ((`(,graph . ,node) (funcall resolve operation 'node)))
                 (lispm-comfyui-remove-node graph node)
                 (setq added (seq-remove (lambda (entry) (eq (cdr entry) node)) added))
                 (push (format "removed %s" (alist-get 'node operation)) done)))
              ("mode"
               (let ((node (cdr (funcall resolve operation 'node)))
                     (mode (alist-get 'mode operation)))
                 (setf (lispm-comfyui-node-mode node)
                       (pcase mode ("bypass" 4) ("mute" 2) ("normal" 0)
                              (_ (signal 'lispm-comfyui-error
                                         (list (format "A mode is normal, bypass or mute, not %s" mode))))))
                 (push (format "made %s %s" (alist-get 'node operation) mode) done)))
              ("title"
               (let ((node (cdr (funcall resolve operation 'node))))
                 (setf (lispm-comfyui-node-title node) (alist-get 'title operation))
                 (push (format "titled %s" (alist-get 'node operation)) done)))
              ("move"
               (let ((node (cdr (funcall resolve operation 'node))))
                 (setf (lispm-comfyui-node-x node) (alist-get 'x operation)
                       (lispm-comfyui-node-y node) (alist-get 'y operation))
                 (push (format "moved %s" (alist-get 'node operation)) done)))
              ("subgraph"
               (let* ((places (mapcar (lambda (value) (lispm-comfyui-tools--resolve root references value))
                                      (alist-get 'nodes operation)))
                      (graph (car (car places))))
                 (unless (and places (seq-every-p (lambda (place) (eq (car place) graph)) places))
                   (signal 'lispm-comfyui-error (list "A subgraph is made of nodes of one graph")))
                 (let ((node (lispm-comfyui-graph-convert-to-subgraph
                              graph server (mapcar #'cdr places) (alist-get 'name operation))))
                   (when-let* ((ref (alist-get 'ref operation)))
                     (puthash ref (cons graph node) references))
                   (push (format "made the subgraph node #%s" (lispm-comfyui-node-id node)) done))))
              ("unpack"
               (pcase-let ((`(,graph . ,node) (funcall resolve operation 'node)))
                 (let ((nodes (lispm-comfyui-graph-unpack graph server node)))
                   (push (format "unpacked %s into %s" (alist-get 'node operation)
                                 (mapconcat (lambda (node) (format "#%s" (lispm-comfyui-node-id node))) nodes ", "))
                         done))))
              (_ (signal 'lispm-comfyui-error (list (format "No operation %S" op)))))))
        ;; New nodes go where their links lead, those placed by a
        ;; move staying where they were put.
        (pcase-dolist (`(,graph . ,node) (reverse added))
          (unless (seq-find (lambda (operation)
                              (and (equal (alist-get 'op operation) "move")
                                   (eq (cdr (ignore-errors (lispm-comfyui-tools--resolve
                                                            root references (alist-get 'node operation))))
                                       node)))
                            operations)
            (lispm-comfyui-place graph node (and (eq graph lispm-comfyui--graph) (lispm-comfyui--node)))
            (when (eq graph lispm-comfyui--graph)
              (lispm-comfyui--clear-place node))))
        (when-let* ((shown (seq-find (lambda (entry) (eq (car entry) lispm-comfyui--graph)) added)))
          (setq lispm-comfyui--selected (lispm-comfyui-node-id (cdr shown))
                lispm-comfyui--focus 0)))
      (when added
        (when (eq lispm-comfyui--current-view 'graph) (lispm-comfyui--fit))
        (lispm-comfyui--draw))
      (let ((missing (lispm-comfyui-tools--missing root (lispm-comfyui--types))))
        (concat (string-join (nreverse done) "; ") "."
                (if missing (concat "\nStill missing: " (string-join missing "; ") ".") ""))))))

(defun lispm-comfyui-tools--missing (root types)
  "The required inputs of the nodes of ROOT's workflow, of TYPES, nothing feeds.
Those of nodes inside subgraphs numbered by their paths."
  (let ((told nil) (missing nil))
    (cl-labels
        ((walk (graph prefix)
           (dolist (node (lispm-comfyui-graph-nodes graph))
             (let ((definition (gethash (lispm-comfyui-node-type node) types)))
               (when (and definition (eql (lispm-comfyui-node-mode node) 0))
                 (cl-loop for (name spec section) in (lispm-comfyui-type-inputs definition)
                          for index = (lispm-comfyui-input-index node name)
                          when (and (eq section 'required) (null (lispm-comfyui-widget-kind spec))
                                    (not (and index (lispm-comfyui-input-link graph node index))))
                          do (push (format "#%s%s.%s (%s)" prefix (lispm-comfyui-node-id node) name
                                           (lispm-comfyui-socket-type spec))
                                   missing))
                 (when-let* ((inner (alist-get 'lispm-subgraph definition))
                             ((not (memq inner told))))
                   (push inner told)
                   (walk (lispm-comfyui-subgraph-graph inner)
                         (format "%s%s:" prefix (lispm-comfyui-node-id node)))))))))
      (walk root ""))
    (nreverse missing)))

(lispm-deftool comfyui_workflow ()
  "Read the ComfyUI workflow open in LispM, the one the user sees: its nodes, each `#ID TYPE \"TITLE\"`, the inputs linked into each (`name <- #ID.output`), its widgets' values (`name = value`), its outputs and where they go, and errors of the last run.  The inside of each subgraph is told once, its nodes numbered with the path to them, as #12:5 inside the subgraph of node 12, its inputs and outputs as 12:in.NAME and 12:out.NAME.  Opens a new, empty workflow if none is open."
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

(lispm-deftool comfyui_edit ((operations array "The changes, in order, each an object with `op`: {op: \"add\", type, in?, ref?, title?, values?: {widget: value}} adds a node, inside the subgraph of node `in` if given (a path), of a node type or of a subgraph by its name, which later operations name by its ref; {op: \"set\", node, name, value} sets a widget (name \"seed/control\" sets how a seed changes after each run: fixed, increment, decrement or randomize); {op: \"connect\", from_node, from_output (name or index), to_node, to_input} links an output to an input, or to a widget, which it then feeds; from \"12:in\" or to \"12:out\", a subgraph's input or output, made if the name is new; {op: \"disconnect\", node, input}; {op: \"remove\", node}; {op: \"mode\", node, mode: normal|bypass|mute}; {op: \"title\", node, title}; {op: \"move\", node, x, y}; {op: \"subgraph\", nodes, name, ref?} makes a subgraph of nodes of one graph, a node of it in their place; {op: \"unpack\", node} puts the nodes inside a subgraph's node in its place.  A node is its number, a path into subgraphs as \"12:5\", or an add's ref."
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
      (display-buffer (with-current-buffer buffer (lispm-comfyui--view-buffer)))
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
