;;; lispm-comfyui-prompt.el --- The prompt a ComfyUI workflow makes  -*- lexical-binding: t; -*-

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

;; A workflow turned into the prompt ComfyUI's server runs, as its
;; frontend turns it (graphToPrompt, ExecutableNodeDTO): each node that
;; runs with its widgets' values, named, and its inputs' links, followed
;; through reroutes and bypassed nodes; the nodes inside subgraphs in
;; place of the subgraphs' nodes, numbered with the path to them, as
;; 12:5, links followed in and out of subgraphs through their inputs
;; and outputs; texts of dynamic prompts, {a|b}, made one choice of
;; theirs, at random.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-comfyui-graph)

;;;; Dynamic prompts

(defun lispm-comfyui-dynamic-prompt (text)
  "TEXT with each {A|B|...} made one of its choices, at random.
As ComfyUI's frontend does to a widget of dynamic prompts when it
queues it: C's comments, // to the end of a line and /* to */, go;
choices nest; a backslash keeps the brace or bar after it."
  (let* ((input (replace-regexp-in-string "/\\*\\(?:.\\|\n\\)*?\\*/\\|//.*" "" text))
         (length (length input))
         (i 0))
    (cl-labels
        ((escape ()
           (prog1 (if (< i length) (string ?\\ (aref input i)) "\\")
             (cl-incf i)))
         (choice-block ()
           (let ((options nil) (choice "") (depth 0) (done nil))
             (while (and (not done) (< i length))
               (let ((char (aref input i)))
                 (cl-incf i)
                 (cond ((eq char ?\\) (setq choice (concat choice (escape))))
                       ((and (eq char ?}) (= depth 0)) (setq done t))
                       ((and (eq char ?|) (= depth 0))
                        (push choice options)
                        (setq choice ""))
                       (t (cond ((eq char ?{) (cl-incf depth))
                                ((eq char ?}) (cl-decf depth)))
                          (setq choice (concat choice (string char)))))))
             (push choice options)
             (lispm-comfyui-dynamic-prompt (nth (random (length options)) (nreverse options))))))
      (let ((result ""))
        (while (< i length)
          (let ((char (aref input i)))
            (cl-incf i)
            (setq result (concat result (cond ((eq char ?\\) (escape))
                                              ((eq char ?{) (choice-block))
                                              (t (string char)))))))
        (replace-regexp-in-string "\\\\\\([{}|]\\)" "\\1" result)))))

;;;; Following links

(cl-defstruct (lispm-comfyui--place (:constructor lispm-comfyui--place-create) (:copier nil))
  "Where a node is: in GRAPH, inside the subgraphs of the nodes of PATH,
numbers as strings from the workflow in; INSTANCE is the subgraph's
node it is in, PARENT the place of that node."
  graph path instance parent)

(defun lispm-comfyui--execution-id (place node)
  "The number of NODE at PLACE in the prompt: the path to it, with colons."
  (string-join (append (lispm-comfyui--place-path place)
                       (list (format "%s" (lispm-comfyui-node-id node))))
               ":"))

(defun lispm-comfyui--inside (place node subgraph)
  "The place inside SUBGRAPH, of NODE at PLACE."
  (lispm-comfyui--place-create
   :graph (lispm-comfyui-subgraph-graph subgraph)
   :path (append (lispm-comfyui--place-path place) (list (format "%s" (lispm-comfyui-node-id node))))
   :instance node :parent place))

(defun lispm-comfyui--bypass-index (node slot type)
  "The input of NODE, bypassed, its output SLOT passes on, for TYPE.
As ComfyUI's frontend chooses: the input of the same index if it
fits, else the first of TYPE exactly, else the first that fits; -1 if
none does."
  (let* ((inputs (lispm-comfyui-node-inputs node))
         (opposite (nth slot inputs))
         (output (nth slot (lispm-comfyui-node-outputs node)))
         (output-type (if output (lispm-comfyui-slot-type output) "*"))
         (fits (lambda (input)
                 (and (lispm-comfyui-types-match-p output-type (lispm-comfyui-slot-type input))
                      (lispm-comfyui-types-match-p type (lispm-comfyui-slot-type input))))))
    (cond ((member type '("*" "")) (if (> (length inputs) slot) slot 0))
          ((and opposite (funcall fits opposite)) slot)
          ((seq-position inputs type (lambda (input type) (equal (lispm-comfyui-slot-type input) type))))
          ((seq-position inputs nil (lambda (input _) (funcall fits input))))
          (t -1))))

(defun lispm-comfyui--resolve-input (place node index types &optional type seen crossed)
  "What feeds input INDEX of NODE at PLACE, of TYPES: nil for nothing,
\(:value . VALUE) for a value, (:link ID SLOT) for an output of the
node numbered ID in the prompt.  TYPE is that of the input asking, if
it comes from further on; SEEN guards against cycles; CROSSED is
non-nil once the way has come out of a subgraph."
  (let* ((key (list (lispm-comfyui--place-graph place) node :input index))
         (graph (lispm-comfyui--place-graph place))
         (link (lispm-comfyui-input-link graph node index)))
    (when (member key seen)
      (signal 'lispm-comfyui-error
              (list (format "Node #%s feeds itself" (lispm-comfyui--execution-id place node)))))
    (push key seen)
    (cond
     ((null link) nil)
     ;; From the subgraph's inputs: what feeds its node outside.
     ((and (eql (lispm-comfyui-link-from link) lispm-comfyui-input-node)
           (lispm-comfyui--place-instance place))
      (let* ((instance (lispm-comfyui--place-instance place))
             (parent (lispm-comfyui--place-parent place))
             (slot (lispm-comfyui-link-from-slot link))
             (input (nth slot (lispm-comfyui-node-inputs instance))))
        (cond
         ((null input) nil)
         ((lispm-comfyui-input-link (lispm-comfyui--place-graph parent) instance slot)
          (lispm-comfyui--resolve-input parent instance slot types nil seen t))
         ((lispm-comfyui-slot-widget input)
          (cons :value (cdr (assoc (lispm-comfyui-slot-name input)
                                   (lispm-comfyui-node-widget-alist instance types))))))))
     (t
      (when-let* ((origin (lispm-comfyui-node graph (lispm-comfyui-link-from link))))
        (lispm-comfyui--resolve-output
         place origin (lispm-comfyui-link-from-slot link) types
         (or type (lispm-comfyui-slot-type (nth index (lispm-comfyui-node-inputs node))))
         seen crossed))))))

(defun lispm-comfyui--resolve-output (place node slot types type seen &optional crossed)
  "What output SLOT of NODE at PLACE, of TYPES, gives an input of TYPE.
As `lispm-comfyui--resolve-input' says; SEEN guards against cycles,
CROSSED is non-nil once the way has come out of a subgraph."
  (let ((key (list (lispm-comfyui--place-graph place) node :output slot))
        (node-type (lispm-comfyui-node-type node))
        (definition (gethash (lispm-comfyui-node-type node) types)))
    (when (member key seen)
      (signal 'lispm-comfyui-error
              (list (format "Node #%s feeds itself" (lispm-comfyui--execution-id place node)))))
    (push key seen)
    (cond
     ((eql (lispm-comfyui-node-mode node) 2) nil)
     ((eql (lispm-comfyui-node-mode node) 4)
      (let ((index (lispm-comfyui--bypass-index node slot type)))
        (when (>= index 0)
          (lispm-comfyui--resolve-input place node index types nil seen crossed))))
     ((alist-get 'lispm-subgraph definition)
      (let* ((subgraph (alist-get 'lispm-subgraph definition))
             (inside (lispm-comfyui-subgraph-graph subgraph))
             (link (seq-find (lambda (link) (and (eql (lispm-comfyui-link-to link) lispm-comfyui-output-node)
                                                 (eql (lispm-comfyui-link-to-slot link) slot)))
                             (lispm-comfyui-graph-links inside))))
        (cond ((null link) nil)
              ;; An input passed straight out.
              ((eql (lispm-comfyui-link-from link) lispm-comfyui-input-node)
               (lispm-comfyui--resolve-input place node (lispm-comfyui-link-from-slot link) types type seen crossed))
              (t (when-let* ((origin (lispm-comfyui-node inside (lispm-comfyui-link-from link))))
                   (lispm-comfyui--resolve-output (lispm-comfyui--inside place node subgraph)
                                                  origin (lispm-comfyui-link-from-slot link)
                                                  types type seen crossed))))))
     ((equal node-type "Reroute")
      (lispm-comfyui--resolve-input place node 0 types type seen crossed))
     ;; A primitive's value is set by the frontend in the widget it feeds,
     ;; not in one inside a subgraph it feeds.
     ((equal node-type "PrimitiveNode")
      (let ((values (lispm-comfyui-node-values node)))
        (and (not crossed) (consp values) (not (keywordp (car values))) (cons :value (car values)))))
     ((member node-type lispm-comfyui-virtual-types) nil)
     (t (list :link (lispm-comfyui--execution-id place node) slot)))))

;;;; The prompt

(defun lispm-comfyui--wrap (value)
  "VALUE as a prompt holds it: an array in an object, not to be taken for a link."
  (if (vectorp value) `((__value__ . ,value)) value))

(defun lispm-comfyui--prompt-entry (place node types resolved)
  "NODE at PLACE, of TYPES, as an entry of the prompt, (ID . ENTRY).
RESOLVED, if a table, gets the texts of dynamic prompts chosen, of
the workflow's own nodes."
  (let ((inputs nil))
    (pcase-dolist (`(,name . ,value) (lispm-comfyui-node-widget-alist node types))
      ;; Not a control, nor what a node saving its values by name keeps
      ;; that is no input of its, as a preview's state.
      (unless (or (string-suffix-p "/control" name)
                  (and (lispm-comfyui-values-by-name-p (lispm-comfyui-node-values node))
                       (consp value) (consp (car value))
                       (not (assoc name (lispm-comfyui-type-inputs
                                         (gethash (lispm-comfyui-node-type node) types))))))
        (let ((spec (lispm-comfyui--widget-spec node types name)))
          (when (and (stringp value) spec (lispm-comfyui-spec-option spec 'dynamicPrompts))
            (setq value (lispm-comfyui-dynamic-prompt value))
            (when (and (hash-table-p resolved) (null (lispm-comfyui--place-path place)))
              (push (cons name value) (gethash (lispm-comfyui-node-id node) resolved))))
          (push (cons (intern name) (lispm-comfyui--wrap value)) inputs))))
    (cl-loop for slot in (lispm-comfyui-node-inputs node)
             for index from 0
             for name = (intern (lispm-comfyui-slot-name slot))
             for source = (lispm-comfyui--resolve-input place node index types)
             when source
             do (setq inputs (assq-delete-all name inputs))
             (push (cons name (pcase source
                                (`(:value . ,value) (lispm-comfyui--wrap value))
                                (`(:link ,id ,slot) (vector id slot))))
                   inputs))
    (cons (intern (lispm-comfyui--execution-id place node))
          `((inputs . ,(nreverse inputs))
            (class_type . ,(lispm-comfyui-node-type node))
            (_meta . ((title . ,(lispm-comfyui-node-name node types))))))))

(defun lispm-comfyui-prompt (graph types &optional resolved)
  "The prompt of GRAPH, a workflow, as ComfyUI's server takes it at /prompt.
TYPES are the node types the server knows.  Returns an alist from node
numbers, as strings, 12:5 for node 5 inside the subgraph of node 12, to
each node's inputs and type, to `json-serialize'.  RESOLVED, a hash
table if given, gets for each of GRAPH's own nodes the texts its
dynamic prompts became, (NAME . TEXT) by node number, as the frontend
saves them in the images made.  Signals `lispm-comfyui-error' for a
node of a type TYPES has not."
  (let* ((top (lispm-comfyui-graph-top graph))
         (merged (lispm-comfyui-graph-types top types))
         (prompt nil))
    (cl-labels
        ((visit (place)
           (dolist (node (lispm-comfyui-graph-nodes (lispm-comfyui--place-graph place)))
             (let* ((type (lispm-comfyui-node-type node))
                    (definition (gethash type merged)))
               (cond
                ((member type lispm-comfyui-virtual-types))
                ((null definition)
                 (unless (memql (lispm-comfyui-node-mode node) '(2 4))
                   (signal 'lispm-comfyui-error
                           (list (format "Node #%s is of the type %s, which the server does not have"
                                         (lispm-comfyui--execution-id place node) type)))))
                ;; The frontend leaves out what is inside the subgraphs'
                ;; nodes muted or bypassed in the workflow, not inside
                ;; other subgraphs.
                ((alist-get 'lispm-subgraph definition)
                 (unless (and (null (lispm-comfyui--place-path place))
                              (memql (lispm-comfyui-node-mode node) '(2 4)))
                   (visit (lispm-comfyui--inside place node (alist-get 'lispm-subgraph definition)))))
                ((memql (lispm-comfyui-node-mode node) '(2 4)))
                (t (push (lispm-comfyui--prompt-entry place node merged resolved) prompt)))))))
      (visit (lispm-comfyui--place-create :graph top)))
    ;; Links to nodes that do not run go, as the frontend leaves them out.
    (dolist (entry prompt)
      (let ((inputs (assq 'inputs (cdr entry))))
        (setcdr inputs (seq-remove (lambda (input)
                                     (let ((value (cdr input)))
                                       (and (vectorp value) (= (length value) 2) (stringp (aref value 0))
                                            (not (assq (intern (aref value 0)) prompt)))))
                                   (cdr inputs)))))
    (nreverse prompt)))

(provide 'lispm-comfyui-prompt)
;;; lispm-comfyui-prompt.el ends here
