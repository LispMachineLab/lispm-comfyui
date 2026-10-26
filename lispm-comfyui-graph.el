;;; lispm-comfyui-graph.el --- ComfyUI's workflows as graphs  -*- lexical-binding: t; -*-

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

;; A workflow as ComfyUI's own frontend saves it, nodes placed on a
;; canvas and the links between them, read into a graph that is drawn,
;; edited, saved again and turned into the prompt ComfyUI's server
;; runs.  Nothing here speaks to a server: the node types, what the
;; server's /object_info says of each, are given, as a table made by
;; `lispm-comfyui-types'.
;;
;; A node's widgets, its numbers, texts and choices, keep their values
;; as the frontend saves them, in the order the node type declares
;; them and without their names; `lispm-comfyui-widgets' names them.
;; A seed's widget has a second value after its own, which says how it
;; changes after each run.  A choice of a dynamic combo brings widgets
;; of its own, named after it with a dot, whose values follow its own.
;;
;; What of a workflow this file does not use, a node's colour or
;; properties say, is kept as it was read and written back unchanged.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; Node types

(defun lispm-comfyui-types (object-info)
  "A table of node types from OBJECT-INFO, what /object_info says.
OBJECT-INFO is that JSON as `json-parse-string' reads it with
`:object-type' `alist'.  The table maps each type's name, a string,
to its definition."
  (let ((types (make-hash-table :test #'equal :size (length object-info))))
    (pcase-dolist (`(,name . ,definition) object-info)
      (puthash (if (symbolp name) (symbol-name name) name) definition types))
    types))

(defun lispm-comfyui--field (alist key)
  "The value of KEY, a symbol, in ALIST, a JSON object; nil if absent.
JSON's null and false read as nil."
  (let ((value (alist-get key alist)))
    (if (memq value '(:null :false)) nil value)))

(defun lispm-comfyui-type-title (definition)
  "The name a node type of DEFINITION is shown by."
  (or (lispm-comfyui--field definition 'display_name)
      (lispm-comfyui--field definition 'name)))

(defun lispm-comfyui-spec-type (spec)
  "The type of an input of SPEC: a string, or a vector of choices."
  (aref spec 0))

(defun lispm-comfyui-spec-options (spec)
  "The options of an input of SPEC, an alist."
  (and (> (length spec) 1) (listp (aref spec 1)) (aref spec 1)))

(defun lispm-comfyui-spec-option (spec option)
  "The OPTION, a symbol, of an input of SPEC; nil if absent or false."
  (lispm-comfyui--field (lispm-comfyui-spec-options spec) option))

(defun lispm-comfyui--inputs-of (inputs order)
  "The inputs of INPUTS, an alist of sections, as (NAME SPEC SECTION).
ORDER is the type's input_order, which says the order of each section
when it is there."
  (cl-loop for section in '(required optional)
           for specs = (lispm-comfyui--field inputs section)
           for names = (or (and order (append (lispm-comfyui--field order section) nil))
                           (mapcar (lambda (entry) (symbol-name (car entry))) specs))
           append (cl-loop for name in names
                           for spec = (alist-get (intern name) specs)
                           when (vectorp spec) collect (list name spec section))))

(defun lispm-comfyui-type-inputs (definition)
  "The inputs of a node type of DEFINITION, as (NAME SPEC SECTION).
NAME is a string, SPEC the vector /object_info gives, its type and
options, and SECTION `required' or `optional'.  Hidden inputs are not
among them."
  (lispm-comfyui--inputs-of (lispm-comfyui--field definition 'input)
                            (lispm-comfyui--field definition 'input_order)))

(defun lispm-comfyui-type-outputs (definition)
  "The outputs of a node type of DEFINITION, as (NAME TYPE)."
  (let ((types (append (lispm-comfyui--field definition 'output) nil))
        (names (append (lispm-comfyui--field definition 'output_name) nil)))
    (cl-loop for type in types
             for index from 0
             for name = (or (nth index names) type)
             collect (list (if (stringp name) name (format "%s" name))
                           (if (stringp type) type "COMBO")))))

(defun lispm-comfyui-widget-kind (spec)
  "The kind of widget an input of SPEC is, or nil for a socket.
One of `int', `float', `string', `boolean', `combo' and `dynamic', a
dynamic combo, whose choice brings inputs of its own."
  (let ((type (lispm-comfyui-spec-type spec)))
    (cond ((lispm-comfyui-spec-option spec 'forceInput) nil)
          ((vectorp type) 'combo)
          ((equal type "INT") 'int)
          ((equal type "FLOAT") 'float)
          ((equal type "STRING") 'string)
          ((equal type "BOOLEAN") 'boolean)
          ((equal type "COMBO") 'combo)
          ((equal type "COMFY_DYNAMICCOMBO_V3") 'dynamic))))

(defun lispm-comfyui-socket-type (spec)
  "The type of the socket of an input of SPEC, a string.
Types that may be any of several are named together with commas, as
ComfyUI's frontend names them; \"*\" is any type."
  (let ((type (lispm-comfyui-spec-type spec)))
    (cond ((vectorp type) "COMBO")
          ((equal type "COMFY_MATCHTYPE_V3")
           (or (lispm-comfyui--field (lispm-comfyui-spec-option spec 'template) 'allowed_types)
               "*"))
          ((equal type "COMFY_AUTOGROW_V3") "*")
          ((equal type "COMFY_DYNAMICCOMBO_V3") "COMBO")
          (t type))))

(defun lispm-comfyui-types-match-p (output input)
  "Non-nil if an output of the type OUTPUT may feed an input of INPUT.
Either may be several types separated by commas, or \"*\"."
  (or (member output '("*" "")) (member input '("*" ""))
      (let ((outputs (split-string output "," t " "))
            (inputs (split-string input "," t " ")))
        (seq-intersection outputs inputs))))

(defun lispm-comfyui-widget-choices (spec)
  "The choices a combo widget of SPEC offers, a list of strings."
  (let ((type (lispm-comfyui-spec-type spec)))
    (cond ((vectorp type) (append type nil))
          ((equal type "COMFY_DYNAMICCOMBO_V3")
           (mapcar (lambda (option) (lispm-comfyui--field option 'key))
                   (lispm-comfyui-spec-option spec 'options)))
          (t (append (lispm-comfyui-spec-option spec 'options) nil)))))

(defun lispm-comfyui--dynamic-inputs (spec choice)
  "The inputs the CHOICE of a dynamic combo of SPEC brings."
  (when-let* ((option (seq-find (lambda (option)
                                  (equal (lispm-comfyui--field option 'key) choice))
                                (lispm-comfyui-spec-option spec 'options))))
    (lispm-comfyui--inputs-of (lispm-comfyui--field option 'inputs) nil)))

(defun lispm-comfyui-widget-default (spec)
  "The value a new widget of SPEC starts with."
  (let ((default (alist-get 'default (lispm-comfyui-spec-options spec))))
    (pcase (lispm-comfyui-widget-kind spec)
      ('boolean (if (memq default '(nil :null)) :false default))
      ((and kind (or 'int 'float))
       (cond ((numberp default) default)
             ((numberp (lispm-comfyui-spec-option spec 'min))
              (max 0 (lispm-comfyui-spec-option spec 'min)))
             ((eq kind 'int) 0)
             (t 0.0)))
      ('string (if (stringp default) default ""))
      (_ (if (stringp default) default
           (or (car (lispm-comfyui-widget-choices spec)) ""))))))

(defconst lispm-comfyui-controls '("fixed" "increment" "decrement" "randomize")
  "How a widget with a control changes after each run.")

(defun lispm-comfyui-widgets (definition values)
  "The widgets of a node of the type DEFINITION, whose values are VALUES.
VALUES is the list of the node's widgets' values as saved.  Returns a
list of plists, in the order of their values: :name, the input's name;
:spec, its specification; :kind, as `lispm-comfyui-widget-kind' says;
:index, where its value is in VALUES; and :control, for a widget whose
value changes after each run, where the value saying how is.  The
widgets a dynamic combo's choice brings follow it, named after it
with a dot between."
  (let ((index 0) widgets)
    (cl-labels
        ((walk (inputs prefix)
           (pcase-dolist (`(,name ,spec ,_section) inputs)
             (when-let* ((kind (lispm-comfyui-widget-kind spec)))
               (let ((widget (list :name (concat prefix name) :spec spec :kind kind
                                   :index index)))
                 (cl-incf index)
                 (when (lispm-comfyui-spec-option spec 'control_after_generate)
                   (setq widget (plist-put widget :control index))
                   (cl-incf index))
                 (push widget widgets)
                 (when (eq kind 'dynamic)
                   (walk (lispm-comfyui--dynamic-inputs
                          spec (nth (plist-get widget :index) values))
                         (concat prefix name ".")))
                 (dolist (extra (lispm-comfyui--format-inputs
                                 spec (nth (plist-get widget :index) values)))
                   (push (list :name (car extra) :spec (cadr extra)
                               :kind (lispm-comfyui-widget-kind (cadr extra)) :index nil)
                         widgets)))))))
      (walk (lispm-comfyui-type-inputs definition) ""))
    (nreverse widgets)))

(defun lispm-comfyui--format-inputs (spec choice)
  "The widgets the CHOICE of a combo of SPEC brings, as Video Helper Suite's.
A combo whose options have `formats' brings, for each choice, widgets
of its own, which are not among the node's values as saved in a list:
they have no :index, and hold their defaults until the node's values
are saved by name.  Returns a list of (NAME SPEC)."
  (when-let* ((formats (lispm-comfyui-spec-option spec 'formats))
              ((stringp choice))
              (extras (alist-get (intern choice) formats)))
    (cl-loop for extra across extras
             when (and (vectorp extra) (> (length extra) 1))
             collect (let ((type (aref extra 1)))
                       (list (aref extra 0)
                             (cond ((and (vectorp type) (> (length type) 0)
                                         (stringp (aref type 0))
                                         (member (aref type 0) '("INT" "FLOAT" "STRING" "BOOLEAN"))
                                         (= (length type) 2))
                                    type)
                                   ((> (length extra) 2) (vector type (aref extra 2)))
                                   (t (vector type))))))))

(defun lispm-comfyui-widget-value (widget values)
  "The value WIDGET holds among VALUES, or its default if none is there."
  (let ((index (plist-get widget :index)))
    (if (and index (< index (length values)))
        (nth index values)
      (lispm-comfyui-widget-default (plist-get widget :spec)))))

(defun lispm-comfyui-widget-control (widget values)
  "How WIDGET's value changes after each run, among VALUES, or nil."
  (when-let* ((index (plist-get widget :control)))
    (let ((control (nth index values)))
      (if (member control lispm-comfyui-controls) control "randomize"))))

(defun lispm-comfyui-widget-alist (definition values)
  "VALUES of a node of the type DEFINITION as an alist by widget name.
A control, how a widget's value changes after each run, is under the
widget's name followed by \"/control\"."
  (cl-loop for widget in (lispm-comfyui-widgets definition values)
           collect (cons (plist-get widget :name) (lispm-comfyui-widget-value widget values))
           when (plist-get widget :control)
           collect (cons (concat (plist-get widget :name) "/control")
                         (lispm-comfyui-widget-control widget values))))

(defun lispm-comfyui-values-from-alist (definition alist &optional old)
  "The values of a node of the type DEFINITION whose widgets hold ALIST.
ALIST is as `lispm-comfyui-widget-alist' makes it.  A widget it does
not name gets its default.  OLD are the node's values before: those
after the last widget's, which this does not understand, are kept."
  (let ((values nil) (count 0))
    (cl-labels
        ((walk (inputs prefix)
           (pcase-dolist (`(,name ,spec ,_section) inputs)
             (when-let* ((kind (lispm-comfyui-widget-kind spec)))
               (let* ((full (concat prefix name))
                      (cell (assoc full alist))
                      (value (if cell (cdr cell) (lispm-comfyui-widget-default spec))))
                 (push value values)
                 (cl-incf count)
                 (when (lispm-comfyui-spec-option spec 'control_after_generate)
                   (push (or (cdr (assoc (concat full "/control") alist)) "randomize") values)
                   (cl-incf count))
                 (when (eq kind 'dynamic)
                   (walk (lispm-comfyui--dynamic-inputs spec value) (concat full "."))))))))
      (walk (lispm-comfyui-type-inputs definition) ""))
    (append (nreverse values)
            (nthcdr (lispm-comfyui--values-used (lispm-comfyui-widgets definition old)) old))))

(defun lispm-comfyui--values-used (widgets)
  "How many values WIDGETS, as `lispm-comfyui-widgets' makes them, hold."
  (or (cl-loop for widget in widgets
               maximize (1+ (max (or (plist-get widget :index) -1)
                                 (or (plist-get widget :control) -1))))
      0))

(defun lispm-comfyui-values-by-name-p (values)
  "Non-nil if VALUES, a node's widgets' values, are saved by name.
Some custom nodes save them as an object, which is read as
\=(:object . ALIST)."
  (eq (car-safe values) :object))

;;;; Graphs

(cl-defstruct (lispm-comfyui-slot (:constructor lispm-comfyui-slot-create) (:copier nil))
  "An input or an output of a node.
WIDGET is, for an input standing for a widget, the widget's name.
EXTRA is the rest of what was saved of it, an alist."
  name type widget extra)

(cl-defstruct (lispm-comfyui-node (:constructor lispm-comfyui-node-create) (:copier nil))
  "A node of a graph.
ID is its number, TYPE the name of its type, TITLE what it is
called, if not its type's name.  X and Y are where it is on the
canvas, the top left of its body, below its title; WIDTH and HEIGHT
its size as saved.  MODE is 0 for a node that runs, 2 for one muted,
4 for one bypassed.  INPUTS and OUTPUTS are its slots, VALUES its
widgets' values (a list, or (:object . ALIST) for nodes saving them
by name),
and EXTRA the rest of what was saved of it."
  id type title x y width height (mode 0) inputs outputs values extra)

(cl-defstruct (lispm-comfyui-link (:constructor lispm-comfyui-link-create) (:copier nil))
  "A link from output FROM-SLOT of the node FROM to input TO-SLOT of TO."
  id from from-slot to to-slot type)

(cl-defstruct (lispm-comfyui-graph (:constructor lispm-comfyui-graph-create) (:copier nil))
  "A workflow: its NODES, LINKS and GROUPS, the last node and link
numbers given, and EXTRA, the rest of what was saved of it.  GROUPS are
kept as they were read."
  nodes links groups (last-node-id 0) (last-link-id 0) extra)

(defun lispm-comfyui--number (value)
  "VALUE, read as a number; 0 if it is none."
  (cond ((numberp value) value)
        ((stringp value) (string-to-number value))
        (t 0)))

(defun lispm-comfyui--pair (value)
  "VALUE, a pair saved as [A, B] or as {\"0\": A, \"1\": B}, as (A . B)."
  (cond ((vectorp value) (cons (lispm-comfyui--number (aref value 0))
                               (lispm-comfyui--number (aref value 1))))
        ((consp value) (cons (lispm-comfyui--number (alist-get '\0 value))
                             (lispm-comfyui--number (alist-get '\1 value))))
        (t (cons 0 0))))

(defun lispm-comfyui--without (alist keys)
  "ALIST without the entries of KEYS."
  (seq-remove (lambda (entry) (memq (car entry) keys)) alist))

(defun lispm-comfyui--slot-from-json (alist)
  "The slot ALIST, a slot as saved, describes."
  (lispm-comfyui-slot-create
   :name (lispm-comfyui--field alist 'name)
   :type (let ((type (lispm-comfyui--field alist 'type)))
           (if (stringp type) type (format "%s" (or type "*"))))
   :widget (lispm-comfyui--field (lispm-comfyui--field alist 'widget) 'name)
   :extra (lispm-comfyui--without alist '(name type widget link links slot_index))))

(defun lispm-comfyui--node-from-json (alist)
  "The node ALIST, a node as saved, describes."
  (let ((pos (lispm-comfyui--pair (alist-get 'pos alist)))
        (size (lispm-comfyui--pair (alist-get 'size alist)))
        (values (alist-get 'widgets_values alist)))
    (lispm-comfyui-node-create
     :id (lispm-comfyui--field alist 'id)
     :type (lispm-comfyui--field alist 'type)
     :title (lispm-comfyui--field alist 'title)
     :x (car pos) :y (cdr pos) :width (car size) :height (cdr size)
     :mode (or (lispm-comfyui--field alist 'mode) 0)
     :inputs (mapcar #'lispm-comfyui--slot-from-json (lispm-comfyui--field alist 'inputs))
     :outputs (mapcar #'lispm-comfyui--slot-from-json (lispm-comfyui--field alist 'outputs))
     :values (cond ((vectorp values) (append values nil))
                   ((consp values) (cons :object values)))
     :extra (lispm-comfyui--without
             alist '(id type title pos size mode inputs outputs widgets_values)))))

(defun lispm-comfyui--link-from-json (value)
  "The link VALUE, as saved, an array or an object, describes."
  (if (vectorp value)
      (lispm-comfyui-link-create :id (aref value 0) :from (aref value 1)
                                 :from-slot (aref value 2) :to (aref value 3)
                                 :to-slot (aref value 4)
                                 :type (let ((type (and (> (length value) 5) (aref value 5))))
                                         (if (stringp type) type "*")))
    (lispm-comfyui-link-create :id (lispm-comfyui--field value 'id)
                               :from (lispm-comfyui--field value 'origin_id)
                               :from-slot (lispm-comfyui--field value 'origin_slot)
                               :to (lispm-comfyui--field value 'target_id)
                               :to-slot (lispm-comfyui--field value 'target_slot)
                               :type (or (lispm-comfyui--field value 'type) "*"))))

(defun lispm-comfyui-graph-from-json (alist)
  "The graph ALIST, a workflow as ComfyUI's frontend saves it, describes.
ALIST is that JSON as `json-parse-string' reads it with `:object-type'
`alist'."
  (let* ((nodes (mapcar #'lispm-comfyui--node-from-json
                        (lispm-comfyui--field alist 'nodes)))
         (links (mapcar #'lispm-comfyui--link-from-json
                        (lispm-comfyui--field alist 'links))))
    (lispm-comfyui-graph-create
     :nodes nodes :links links
     :groups (append (lispm-comfyui--field alist 'groups) nil)
     :last-node-id (apply #'max (or (lispm-comfyui--field alist 'last_node_id) 0)
                          (seq-filter #'numberp (mapcar #'lispm-comfyui-node-id nodes)))
     :last-link-id (apply #'max (or (lispm-comfyui--field alist 'last_link_id) 0)
                          (seq-filter #'numberp (mapcar #'lispm-comfyui-link-id links)))
     :extra (lispm-comfyui--without
             alist '(nodes links groups last_node_id last_link_id)))))

(defun lispm-comfyui-graph-from-string (string)
  "The graph of STRING, a workflow's JSON."
  (lispm-comfyui-graph-from-json
   (json-parse-string string :object-type 'alist)))

(defun lispm-comfyui--slot-json (slot &rest more)
  "SLOT as saved, with MORE, more fields as alternating keys and values."
  (append `((name . ,(lispm-comfyui-slot-name slot))
            (type . ,(lispm-comfyui-slot-type slot)))
          (when (lispm-comfyui-slot-widget slot)
            `((widget . ((name . ,(lispm-comfyui-slot-widget slot))))))
          (cl-loop for (key value) on more by #'cddr collect (cons key value))
          (lispm-comfyui-slot-extra slot)))

(defun lispm-comfyui--node-json (graph node)
  "NODE of GRAPH as saved."
  (let ((id (lispm-comfyui-node-id node))
        (links (lispm-comfyui-graph-links graph)))
    (append
     `((id . ,id)
       (type . ,(lispm-comfyui-node-type node))
       (pos . [,(lispm-comfyui-node-x node) ,(lispm-comfyui-node-y node)])
       (size . [,(or (lispm-comfyui-node-width node) 200)
                ,(or (lispm-comfyui-node-height node) 100)]))
     (lispm-comfyui-node-extra node)
     `((mode . ,(lispm-comfyui-node-mode node))
       (inputs
        . ,(vconcat
            (cl-loop for slot in (lispm-comfyui-node-inputs node)
                     for index from 0
                     for link = (seq-find (lambda (link)
                                            (and (equal (lispm-comfyui-link-to link) id)
                                                 (equal (lispm-comfyui-link-to-slot link) index)))
                                          links)
                     collect (lispm-comfyui--slot-json
                              slot 'link (if link (lispm-comfyui-link-id link) :null)))))
       (outputs
        . ,(vconcat
            (cl-loop for slot in (lispm-comfyui-node-outputs node)
                     for index from 0
                     for ids = (cl-loop for link in links
                                        when (and (equal (lispm-comfyui-link-from link) id)
                                                  (equal (lispm-comfyui-link-from-slot link) index))
                                        collect (lispm-comfyui-link-id link))
                     collect (lispm-comfyui--slot-json
                              slot 'links (if ids (vconcat ids) :null) 'slot_index index)))))
     (when (lispm-comfyui-node-title node)
       `((title . ,(lispm-comfyui-node-title node))))
     (let ((values (lispm-comfyui-node-values node)))
       (if (lispm-comfyui-values-by-name-p values)
           `((widgets_values . ,(cdr values)))
         `((widgets_values . ,(vconcat values))))))))

(defun lispm-comfyui-graph-json (graph)
  "GRAPH as ComfyUI's frontend saves a workflow, to `json-serialize'."
  (append
   `((last_node_id . ,(lispm-comfyui-graph-last-node-id graph))
     (last_link_id . ,(lispm-comfyui-graph-last-link-id graph))
     (nodes . ,(vconcat (mapcar (lambda (node) (lispm-comfyui--node-json graph node))
                                (lispm-comfyui-graph-nodes graph))))
     (links . ,(vconcat
                (mapcar (lambda (link)
                          (vector (lispm-comfyui-link-id link) (lispm-comfyui-link-from link)
                                  (lispm-comfyui-link-from-slot link) (lispm-comfyui-link-to link)
                                  (lispm-comfyui-link-to-slot link) (lispm-comfyui-link-type link)))
                        (lispm-comfyui-graph-links graph))))
     (groups . ,(vconcat (lispm-comfyui-graph-groups graph))))
   (or (lispm-comfyui-graph-extra graph)
       '((config . nil) (extra . nil) (version . 0.4)))))

(defun lispm-comfyui-graph-string (graph)
  "GRAPH as the JSON of a workflow, a string."
  (json-serialize (lispm-comfyui-graph-json graph)))

(defun lispm-comfyui-graph-copy (graph)
  "A copy of GRAPH sharing nothing with it that an edit changes."
  (let ((copy-slot (lambda (slot)
                     (lispm-comfyui-slot-create
                      :name (lispm-comfyui-slot-name slot) :type (lispm-comfyui-slot-type slot)
                      :widget (lispm-comfyui-slot-widget slot)
                      :extra (copy-tree (lispm-comfyui-slot-extra slot) t)))))
    (lispm-comfyui-graph-create
     :nodes (mapcar (lambda (node)
                      (lispm-comfyui-node-create
                       :id (lispm-comfyui-node-id node) :type (lispm-comfyui-node-type node)
                       :title (lispm-comfyui-node-title node)
                       :x (lispm-comfyui-node-x node) :y (lispm-comfyui-node-y node)
                       :width (lispm-comfyui-node-width node)
                       :height (lispm-comfyui-node-height node)
                       :mode (lispm-comfyui-node-mode node)
                       :inputs (mapcar copy-slot (lispm-comfyui-node-inputs node))
                       :outputs (mapcar copy-slot (lispm-comfyui-node-outputs node))
                       :values (copy-tree (lispm-comfyui-node-values node) t)
                       :extra (copy-tree (lispm-comfyui-node-extra node) t)))
                    (lispm-comfyui-graph-nodes graph))
     :links (mapcar (lambda (link)
                      (lispm-comfyui-link-create
                       :id (lispm-comfyui-link-id link) :from (lispm-comfyui-link-from link)
                       :from-slot (lispm-comfyui-link-from-slot link)
                       :to (lispm-comfyui-link-to link) :to-slot (lispm-comfyui-link-to-slot link)
                       :type (lispm-comfyui-link-type link)))
                    (lispm-comfyui-graph-links graph))
     :groups (copy-tree (lispm-comfyui-graph-groups graph) t)
     :last-node-id (lispm-comfyui-graph-last-node-id graph)
     :last-link-id (lispm-comfyui-graph-last-link-id graph)
     :extra (copy-tree (lispm-comfyui-graph-extra graph) t))))

;;;; Looking at a graph

(defun lispm-comfyui-node (graph id)
  "The node of GRAPH numbered ID, or nil."
  (seq-find (lambda (node) (equal (lispm-comfyui-node-id node) id))
            (lispm-comfyui-graph-nodes graph)))

(defun lispm-comfyui-node-name (node &optional types)
  "What NODE is called: its title, or its type's name in TYPES."
  (or (lispm-comfyui-node-title node)
      (when-let* ((definition (and types (gethash (lispm-comfyui-node-type node) types))))
        (lispm-comfyui-type-title definition))
      (lispm-comfyui-node-type node)))

(defun lispm-comfyui-input-link (graph node index)
  "The link of GRAPH into input INDEX of NODE, or nil."
  (let ((id (lispm-comfyui-node-id node)))
    (seq-find (lambda (link)
                (and (equal (lispm-comfyui-link-to link) id)
                     (equal (lispm-comfyui-link-to-slot link) index)))
              (lispm-comfyui-graph-links graph))))

(defun lispm-comfyui-output-links (graph node index)
  "The links of GRAPH from output INDEX of NODE."
  (let ((id (lispm-comfyui-node-id node)))
    (seq-filter (lambda (link)
                  (and (equal (lispm-comfyui-link-from link) id)
                       (equal (lispm-comfyui-link-from-slot link) index)))
                (lispm-comfyui-graph-links graph))))

(defun lispm-comfyui-input-index (node name)
  "The index of NODE's input named NAME, or nil."
  (seq-position (lispm-comfyui-node-inputs node) name
                (lambda (slot name) (equal (lispm-comfyui-slot-name slot) name))))

(defun lispm-comfyui-output-index (node name)
  "The index of NODE's output named NAME, or nil."
  (seq-position (lispm-comfyui-node-outputs node) name
                (lambda (slot name) (equal (lispm-comfyui-slot-name slot) name))))

(defun lispm-comfyui-node-widgets (node types)
  "The widgets of NODE, whose type is in TYPES, as `lispm-comfyui-widgets'.
Nil for a node of a type TYPES has not, or one saving its values by name."
  (let ((definition (gethash (lispm-comfyui-node-type node) types))
        (values (lispm-comfyui-node-values node)))
    (when (and definition (not (lispm-comfyui-values-by-name-p values)))
      (lispm-comfyui-widgets definition values))))

(defun lispm-comfyui-node-widget-alist (node types)
  "The values of NODE's widgets by name; see `lispm-comfyui-widget-alist'."
  (let ((definition (gethash (lispm-comfyui-node-type node) types))
        (values (lispm-comfyui-node-values node)))
    (cond ((lispm-comfyui-values-by-name-p values)
           (mapcar (lambda (entry) (cons (symbol-name (car entry)) (cdr entry))) (cdr values)))
          (definition (lispm-comfyui-widget-alist definition values)))))

;;;; The prompt a graph makes

(defconst lispm-comfyui-virtual-types '("Note" "MarkdownNote" "Reroute" "PrimitiveNode")
  "Types of nodes that only ComfyUI's frontend knows, which never run.")

(defun lispm-comfyui--source (graph link &optional seen)
  "Where the value LINK of GRAPH carries comes from.
Through reroutes and bypassed nodes: (NODE . SLOT), the output it
comes out of, (:value . VALUE) for a primitive node's value, or nil
when it comes from nowhere, a muted node or none.  SEEN are the links
followed already."
  (let ((node (lispm-comfyui-node graph (lispm-comfyui-link-from link))))
    (cond
     ((or (null node) (memq link seen)) nil)
     ((equal (lispm-comfyui-node-type node) "Reroute")
      (when-let* ((in (lispm-comfyui-input-link graph node 0)))
        (lispm-comfyui--source graph in (cons link seen))))
     ((equal (lispm-comfyui-node-type node) "PrimitiveNode")
      (cons :value (car (lispm-comfyui-node-values node))))
     ((eql (lispm-comfyui-node-mode node) 2) nil)
     ((eql (lispm-comfyui-node-mode node) 4)
      ;; A bypassed node passes on the first input of the link's type.
      (let ((type (lispm-comfyui-link-type link)))
        (cl-loop for slot in (lispm-comfyui-node-inputs node)
                 for index from 0
                 for in = (lispm-comfyui-input-link graph node index)
                 when (and in (lispm-comfyui-types-match-p (lispm-comfyui-slot-type slot) type))
                 return (lispm-comfyui--source graph in (cons link seen)))))
     (t (cons node (lispm-comfyui-link-from-slot link))))))

(define-error 'lispm-comfyui-error "ComfyUI")

(defun lispm-comfyui-prompt (graph types)
  "The prompt of GRAPH, as ComfyUI's server takes it at /prompt.
TYPES are the node types the server knows.  Returns an alist from
node numbers, as strings, to each node's inputs and type, to
`json-serialize'.  Signals `lispm-comfyui-error' for a node of a type
TYPES has not."
  (let (prompt)
    (dolist (node (lispm-comfyui-graph-nodes graph))
      (let* ((type (lispm-comfyui-node-type node))
             (definition (gethash type types)))
        (cond
         ((member type lispm-comfyui-virtual-types))
         ((memql (lispm-comfyui-node-mode node) '(2 4)))
         ((null definition)
          (signal 'lispm-comfyui-error
                  (list (format "Node #%s is of the type %s, which the server does not have"
                                (lispm-comfyui-node-id node) type))))
         (t
          (let ((inputs nil))
            (pcase-dolist (`(,name . ,value) (lispm-comfyui-node-widget-alist node types))
              ;; Not a control, nor what a node saves by name that is
              ;; no input, as a preview's state.
              (unless (or (string-suffix-p "/control" name)
                          (and (consp value) (consp (car value))))
                (push (cons (intern name) value) inputs)))
            (cl-loop for slot in (lispm-comfyui-node-inputs node)
                     for index from 0
                     for link = (lispm-comfyui-input-link graph node index)
                     for name = (intern (lispm-comfyui-slot-name slot))
                     when link
                     do (let ((source (lispm-comfyui--source graph link)))
                          (setq inputs (assq-delete-all name inputs))
                          (pcase source
                            (`(:value . ,value) (push (cons name value) inputs))
                            (`(,from . ,slot)
                             (push (cons name (vector (format "%s" (lispm-comfyui-node-id from))
                                                      slot))
                                   inputs)))))
            (push (cons (intern (format "%s" (lispm-comfyui-node-id node)))
                        `((inputs . ,(nreverse inputs))
                          (class_type . ,type)
                          (_meta . ((title . ,(lispm-comfyui-node-name node types))))))
                  prompt))))))
    (nreverse prompt)))

;;;; Editing a graph

(defun lispm-comfyui-node-make (graph definition type x y)
  "A new node of TYPE, of DEFINITION, at X Y, numbered next in GRAPH.
The node is not yet in GRAPH."
  (let ((id (1+ (lispm-comfyui-graph-last-node-id graph))))
    (setf (lispm-comfyui-graph-last-node-id graph) id)
    (lispm-comfyui-node-create
     :id id :type type :x x :y y :width 240
     :inputs (cl-loop for (name spec) in (lispm-comfyui-type-inputs definition)
                      unless (lispm-comfyui-widget-kind spec)
                      collect (lispm-comfyui-slot-create
                               :name name :type (lispm-comfyui-socket-type spec)))
     :outputs (cl-loop for (name type) in (lispm-comfyui-type-outputs definition)
                       collect (lispm-comfyui-slot-create :name name :type type))
     :values (lispm-comfyui-values-from-alist definition nil)
     :extra `((flags . nil) (order . 0)
              (properties . ((Node\ name\ for\ S&R . ,type)))))))

(defun lispm-comfyui-graph-add-node (graph types type &optional x y)
  "Add a node of TYPE, one of TYPES, to GRAPH, at X Y; return it."
  (let ((definition (or (gethash type types)
                        (signal 'lispm-comfyui-error
                                (list (format "The server has no node type %s" type))))))
    (let ((node (lispm-comfyui-node-make graph definition type (or x 0) (or y 0))))
      (setf (lispm-comfyui-graph-nodes graph)
            (append (lispm-comfyui-graph-nodes graph) (list node)))
      node)))

(defun lispm-comfyui-remove-node (graph node)
  "Remove NODE from GRAPH, with its links."
  (let ((id (lispm-comfyui-node-id node)))
    (setf (lispm-comfyui-graph-links graph)
          (seq-remove (lambda (link) (or (equal (lispm-comfyui-link-from link) id)
                                         (equal (lispm-comfyui-link-to link) id)))
                      (lispm-comfyui-graph-links graph)))
    (setf (lispm-comfyui-graph-nodes graph)
          (delq node (lispm-comfyui-graph-nodes graph)))))

(defun lispm-comfyui--widget-spec (node types name)
  "The specification of NODE's widget NAME, of TYPES, or nil."
  (plist-get (seq-find (lambda (widget) (equal (plist-get widget :name) name))
                       (lispm-comfyui-node-widgets node types))
             :spec))

(defun lispm-comfyui-ensure-input (node types name)
  "The index of NODE's input NAME, of TYPES, made for a widget if need be.
A widget gets an input when a link is to feed it, as in ComfyUI's
frontend.  Signals `lispm-comfyui-error' if NODE has no such input."
  (or (lispm-comfyui-input-index node name)
      (let ((spec (or (lispm-comfyui--widget-spec node types name)
                      (signal 'lispm-comfyui-error
                              (list (format "Node #%s has no input %s"
                                            (lispm-comfyui-node-id node) name))))))
        (setf (lispm-comfyui-node-inputs node)
              (append (lispm-comfyui-node-inputs node)
                      (list (lispm-comfyui-slot-create
                             :name name :type (lispm-comfyui-socket-type spec) :widget name))))
        (1- (length (lispm-comfyui-node-inputs node))))))

(defun lispm-comfyui-input-type (node types index)
  "The type of NODE's input INDEX, its slot's or its widget's, of TYPES."
  (let ((slot (nth index (lispm-comfyui-node-inputs node))))
    (or (and (lispm-comfyui-slot-widget slot)
             (when-let* ((spec (lispm-comfyui--widget-spec
                                node types (lispm-comfyui-slot-widget slot))))
               (lispm-comfyui-socket-type spec)))
        (lispm-comfyui-slot-type slot))))

(defun lispm-comfyui-graph-disconnect (graph node index)
  "Remove GRAPH's link into input INDEX of NODE, if any.
An input that only stands for a widget goes too, as the widget is
shown again."
  (when-let* ((link (lispm-comfyui-input-link graph node index)))
    (setf (lispm-comfyui-graph-links graph) (delq link (lispm-comfyui-graph-links graph))))
  (let ((slot (nth index (lispm-comfyui-node-inputs node))))
    (when (and slot (lispm-comfyui-slot-widget slot)
               (not (lispm-comfyui-slot-extra slot)))
      (lispm-comfyui--remove-input graph node index))))

(defun lispm-comfyui--remove-input (graph node index)
  "Remove NODE's input INDEX, renumbering the links into those after it."
  (let ((id (lispm-comfyui-node-id node)))
    (dolist (link (lispm-comfyui-graph-links graph))
      (when (and (equal (lispm-comfyui-link-to link) id)
                 (> (lispm-comfyui-link-to-slot link) index))
        (cl-decf (lispm-comfyui-link-to-slot link))))
    (setf (lispm-comfyui-node-inputs node)
          (append (seq-take (lispm-comfyui-node-inputs node) index)
                  (nthcdr (1+ index) (lispm-comfyui-node-inputs node))))))

(defun lispm-comfyui-graph-connect (graph types from output to input)
  "Link output OUTPUT of the node FROM to the input named INPUT of TO.
OUTPUT is an index or a name.  GRAPH's nodes are of TYPES.  A link
into that input before is replaced.  Returns the new link; signals
`lispm-comfyui-error' if the types do not match."
  (let* ((from-slot (if (integerp output) output
                      (or (lispm-comfyui-output-index from output)
                          (signal 'lispm-comfyui-error
                                  (list (format "Node #%s has no output %s"
                                                (lispm-comfyui-node-id from) output))))))
         (slot (or (nth from-slot (lispm-comfyui-node-outputs from))
                   (signal 'lispm-comfyui-error
                           (list (format "Node #%s has no output %s"
                                         (lispm-comfyui-node-id from) output)))))
         (index (lispm-comfyui-ensure-input to types input))
         (type (lispm-comfyui-slot-type slot))
         (input-type (lispm-comfyui-input-type to types index)))
    (unless (lispm-comfyui-types-match-p type input-type)
      (signal 'lispm-comfyui-error
              (list (format "Output %s of node #%s is %s; input %s of node #%s takes %s"
                            (lispm-comfyui-slot-name slot) (lispm-comfyui-node-id from) type
                            input (lispm-comfyui-node-id to) input-type))))
    (when (equal (lispm-comfyui-node-id from) (lispm-comfyui-node-id to))
      (signal 'lispm-comfyui-error (list "A node cannot feed itself")))
    (when-let* ((old (lispm-comfyui-input-link graph to index)))
      (setf (lispm-comfyui-graph-links graph) (delq old (lispm-comfyui-graph-links graph))))
    (let ((link (lispm-comfyui-link-create
                 :id (cl-incf (lispm-comfyui-graph-last-link-id graph))
                 :from (lispm-comfyui-node-id from) :from-slot from-slot
                 :to (lispm-comfyui-node-id to) :to-slot index :type type)))
      (setf (lispm-comfyui-graph-links graph)
            (append (lispm-comfyui-graph-links graph) (list link)))
      link)))

(defun lispm-comfyui-check-value (spec value)
  "VALUE made fit for a widget of SPEC; signals `lispm-comfyui-error' if not.
Numbers are kept within the widget's bounds, integers made whole."
  (let ((kind (lispm-comfyui-widget-kind spec))
        (min (lispm-comfyui-spec-option spec 'min))
        (max (lispm-comfyui-spec-option spec 'max)))
    (pcase kind
      ((or 'int 'float)
       (let ((number (cond ((numberp value) value)
                           ((and (stringp value)
                                 (string-match-p "\\`[-+]?[0-9.eE+-]+\\'" value))
                            (string-to-number value))
                           (t (signal 'lispm-comfyui-error
                                      (list (format "%S is not a number" value)))))))
         (when (numberp min) (setq number (max min number)))
         (when (numberp max) (setq number (min max number)))
         (if (eq kind 'int) (truncate number) (float number))))
      ('boolean (if (or (memq value '(nil :false :null 0)) (equal value "false")) :false t))
      ('string (if (stringp value) value (format "%s" value)))
      (_ (let ((choices (lispm-comfyui-widget-choices spec))
               (value (if (stringp value) value (format "%s" value))))
           (if (or (null choices) (member value choices))
               value
             (signal 'lispm-comfyui-error
                     (list (format "%S is not among the choices: %s" value
                                   (string-join (seq-take choices 20) ", "))))))))))

(defun lispm-comfyui-set-widget (node types name value)
  "Set NODE's widget NAME, of TYPES, to VALUE; return the value set.
NAME followed by \"/control\" sets how the widget changes after each
run, one of `lispm-comfyui-controls'.  VALUE is checked against the
widget first, with `lispm-comfyui-check-value'."
  (let* ((definition (gethash (lispm-comfyui-node-type node) types))
         (alist (lispm-comfyui-node-widget-alist node types))
         (cell (or (assoc name alist)
                   (signal 'lispm-comfyui-error
                           (list (format "Node #%s has no widget %s"
                                         (lispm-comfyui-node-id node) name)))))
         (value (if (string-suffix-p "/control" name)
                    (if (member value lispm-comfyui-controls) value
                      (signal 'lispm-comfyui-error
                              (list (format "A control is one of %s"
                                            (string-join lispm-comfyui-controls ", ")))))
                  (lispm-comfyui-check-value
                   (lispm-comfyui--widget-spec node types name) value))))
    (setcdr cell value)
    (let ((values (lispm-comfyui-node-values node)))
      (when (and (not (lispm-comfyui-values-by-name-p values))
                 (seq-find (lambda (widget) (null (plist-get widget :index)))
                           (lispm-comfyui-node-widgets node types)))
        ;; Widgets a choice brings that a list of values has no room
        ;; for: the node's values are saved by name from now on.
        (setq values (cons :object (mapcar (lambda (entry) (cons (intern (car entry)) (cdr entry)))
                                           (seq-remove (lambda (entry)
                                                         (string-suffix-p "/control" (car entry)))
                                                       alist))))
        (setf (lispm-comfyui-node-values node) values))
      (if (lispm-comfyui-values-by-name-p values)
          (setf (alist-get (intern name) (cdr values)) value)
        (setf (lispm-comfyui-node-values node)
              (lispm-comfyui-values-from-alist definition alist values))))
    value))

(defun lispm-comfyui-after-run (node types)
  "Change NODE's widgets, of TYPES, as their controls say after a run.
Returns non-nil if any changed."
  (let ((changed nil))
    (dolist (widget (lispm-comfyui-node-widgets node types))
      (let* ((values (lispm-comfyui-node-values node))
             (control (lispm-comfyui-widget-control widget values))
             (spec (plist-get widget :spec))
             (value (lispm-comfyui-widget-value widget values))
             (min (or (lispm-comfyui-spec-option spec 'min) 0))
             (max (or (lispm-comfyui-spec-option spec 'max) most-positive-fixnum))
             (new
              (pcase (cons control (plist-get widget :kind))
                (`("randomize" . int)
                 (+ min (random (max 1 (min (- max min) 1125899906842624)))))
                (`("increment" . int) (min max (1+ value)))
                (`("decrement" . int) (max min (1- value)))
                (`(,(or "randomize" "increment" "decrement") . combo)
                 (let* ((choices (lispm-comfyui-widget-choices spec))
                        (position (or (seq-position choices value) 0)))
                   (pcase control
                     ("randomize" (seq-random-elt choices))
                     ("increment" (nth (min (1- (length choices)) (1+ position)) choices))
                     (_ (nth (max 0 (1- position)) choices)))))
                (_ value))))
        (unless (equal new value)
          (setq changed t)
          (setf (nth (plist-get widget :index) (lispm-comfyui-node-values node)) new))))
    changed))

;;;; Placing nodes

(defun lispm-comfyui-place (graph node &optional near)
  "Put NODE, of GRAPH, where it overlaps no other node.
It goes right of the nodes feeding it, or right of NEAR, another node,
or at the left of the graph, and down from there until it is clear."
  (let* ((others (remq node (lispm-comfyui-graph-nodes graph)))
         (feeders (delq nil (cl-loop for index from 0 below (length (lispm-comfyui-node-inputs node))
                                     for link = (lispm-comfyui-input-link graph node index)
                                     when link
                                     collect (lispm-comfyui-node graph (lispm-comfyui-link-from link)))))
         (anchors (or feeders (and near (list near))))
         (x (if anchors
                (+ 60 (apply #'max (mapcar (lambda (other) (+ (lispm-comfyui-node-x other)
                                                              (or (lispm-comfyui-node-width other) 200)))
                                           anchors)))
              (if others (apply #'min (mapcar #'lispm-comfyui-node-x others)) 0)))
         (y (if anchors (apply #'min (mapcar #'lispm-comfyui-node-y anchors))
              (if others (+ 60 (apply #'max (mapcar (lambda (other) (+ (lispm-comfyui-node-y other)
                                                                       (or (lispm-comfyui-node-height other) 100)))
                                                    others)))
                0)))
         (width (or (lispm-comfyui-node-width node) 240))
         (height (or (lispm-comfyui-node-height node) 100)))
    (while (seq-find (lambda (other)
                       (and (< x (+ (lispm-comfyui-node-x other) (or (lispm-comfyui-node-width other) 200) 20))
                            (< (lispm-comfyui-node-x other) (+ x width 20))
                            (< y (+ (lispm-comfyui-node-y other) (or (lispm-comfyui-node-height other) 100) 50))
                            (< (lispm-comfyui-node-y other) (+ y height 50))))
                     others)
      (setq y (+ y 40)))
    (setf (lispm-comfyui-node-x node) x
          (lispm-comfyui-node-y node) y)))

(defun lispm-comfyui-graph-from-prompt (prompt types)
  "A graph of PROMPT, as ComfyUI's server takes it, laid out left to right.
PROMPT is an alist from node numbers to their inputs and types, as
`json-parse-string' reads it with `:object-type' `alist'.  Each node
goes in the column after those feeding it."
  (let ((graph (lispm-comfyui-graph-create))
        (made (make-hash-table :test #'equal))
        (depth (make-hash-table :test #'equal)))
    (pcase-dolist (`(,key . ,entry) prompt)
      (let* ((type (lispm-comfyui--field entry 'class_type))
             (definition (or (gethash type types)
                             (signal 'lispm-comfyui-error
                                     (list (format "The server has no node type %s" type)))))
             (node (lispm-comfyui-node-make graph definition type 0 0))
             (id (string-to-number (format "%s" key))))
        (when (> id 0)
          (setf (lispm-comfyui-node-id node) id
                (lispm-comfyui-graph-last-node-id graph)
                (max id (lispm-comfyui-graph-last-node-id graph))))
        (when-let* ((title (lispm-comfyui--field (lispm-comfyui--field entry '_meta) 'title)))
          (unless (equal title (lispm-comfyui-type-title definition))
            (setf (lispm-comfyui-node-title node) title)))
        (puthash (format "%s" key) node made)
        (setf (lispm-comfyui-graph-nodes graph)
              (append (lispm-comfyui-graph-nodes graph) (list node)))))
    (pcase-dolist (`(,key . ,entry) prompt)
      (let ((node (gethash (format "%s" key) made)))
        (pcase-dolist (`(,name . ,value) (lispm-comfyui--field entry 'inputs))
          (let ((name (symbol-name name)))
            (if (and (vectorp value) (= (length value) 2) (stringp (aref value 0)))
                (when-let* ((from (gethash (aref value 0) made)))
                  (lispm-comfyui-graph-connect graph types from (aref value 1) node name))
              (ignore-errors (lispm-comfyui-set-widget node types name value)))))))
    (cl-labels ((depth-of (node seen)
                  (or (gethash (lispm-comfyui-node-id node) depth)
                      (puthash (lispm-comfyui-node-id node)
                               (if (memq node seen) 0
                                 (1+ (apply #'max -1
                                            (cl-loop for link in (lispm-comfyui-graph-links graph)
                                                     when (equal (lispm-comfyui-link-to link)
                                                                 (lispm-comfyui-node-id node))
                                                     collect (depth-of (lispm-comfyui-node
                                                                        graph (lispm-comfyui-link-from link))
                                                                       (cons node seen))))))
                               depth))))
      (let ((columns (make-hash-table)))
        (dolist (node (lispm-comfyui-graph-nodes graph))
          (let* ((column (depth-of node nil))
                 (y (gethash column columns 0)))
            (setf (lispm-comfyui-node-x node) (* column 320)
                  (lispm-comfyui-node-y node) y)
            (puthash column (+ y 60 (lispm-comfyui-node-estimated-height node types)) columns)))))
    graph))

(defun lispm-comfyui-node-estimated-height (node types)
  "About how tall NODE, of TYPES, is drawn, below its title."
  (+ 12 (* 22 (+ (max (length (lispm-comfyui-node-inputs node))
                      (length (lispm-comfyui-node-outputs node)))
                 (length (lispm-comfyui-node-widgets node types))))))

(provide 'lispm-comfyui-graph)
;;; lispm-comfyui-graph.el ends here
