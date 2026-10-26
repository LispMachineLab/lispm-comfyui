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

(defvar lispm-comfyui-widget-types
  '("COLOR" "COLORS" "BOUNDING_BOX" "BOUNDING_BOXES" "CURVE" "RANGE" "TEXTAREA" "PAINTER"
    "COMPOSITOR" "GALLERIA" "CHART" "VIDEO_EDIT" "LOAD_3D" "LOAD_3D_ADVANCED" "PREVIEW_3D"
    "CAMERA_INFO_STATE" "AUDIO_UI" "AUDIOUPLOAD" "AUDIO_RECORD" "WEBCAM"
    "VHSPATH" "VHSFLOAT" "VHSINT" "VHSTIMESTAMP" "ADEWARN" "ADEAUTOSIZE")
  "Types of inputs ComfyUI's frontend shows as widgets holding a value.
Besides numbers, texts, toggles and choices: those of the frontend and
of the custom nodes whose JavaScript makes widgets.")

(defvar lispm-comfyui-display-widget-types '("RESOLUTION_PREVIEW" "MARKDOWN")
  "Types of inputs the frontend shows as widgets that hold no value.")

(defvar lispm-comfyui-computed-widget-types '(("IMAGECOMPARE" . ["" ""]))
  "Types of inputs whose widgets the frontend saves nothing of, with the
value they give the prompt.")

(defun lispm-comfyui-widget-kind (spec)
  "The kind of widget an input of SPEC is, or nil for a socket.
One of `int', `float', `string', `boolean', `combo'; `dynamic', a
dynamic combo, whose choice brings inputs of its own; `other', one of
`lispm-comfyui-widget-types'; `display', one holding no value; and
`computed', one whose value the frontend makes.  An input of several
types is the widget its widgetType option names, if it does."
  (let* ((widget-type (lispm-comfyui-spec-option spec 'widgetType))
         (type (if (stringp widget-type) widget-type (lispm-comfyui-spec-type spec))))
    (cond ((lispm-comfyui-spec-option spec 'forceInput) nil)
          ((vectorp type) 'combo)
          ((equal type "INT") 'int)
          ((equal type "FLOAT") 'float)
          ((equal type "STRING") 'string)
          ((equal type "BOOLEAN") 'boolean)
          ((equal type "COMBO") 'combo)
          ((equal type "COMFY_DYNAMICCOMBO_V3") 'dynamic)
          ((member type lispm-comfyui-widget-types) 'other)
          ((member type lispm-comfyui-display-widget-types) 'display)
          ((assoc type lispm-comfyui-computed-widget-types) 'computed))))

(defun lispm-comfyui--saved-kind-p (kind)
  "Non-nil if a widget of KIND has a value saved among its node's."
  (memq kind '(int float string boolean combo dynamic other)))

(defun lispm-comfyui--control-p (name spec kind)
  "Non-nil if the widget NAME, of SPEC and KIND, changes after each run.
As its type says; without its saying, an integer named seed or
noise_seed does, as ComfyUI's frontend has it."
  (let ((option (assq 'control_after_generate (lispm-comfyui-spec-options spec))))
    (if option
        (not (memq (cdr option) '(nil :false :null)))
      (and (eq kind 'int) (member name '("seed" "noise_seed"))))))

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
As LiteGraph says: either is any type, \"*\" or empty, or they are the
same, or, separated by commas, share one, case aside."
  (or (member output '("*" "" nil)) (member input '("*" "" nil))
      (equal output input)
      (let ((outputs (split-string (downcase output) "," t " "))
            (inputs (split-string (downcase input) "," t " ")))
        (seq-some (lambda (a) (seq-some (lambda (b) (or (equal a b) (equal a "*") (equal b "*")))
                                        inputs))
                  outputs))))

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
      ('other (if (memq default '(nil)) :null default))
      ('computed (cdr (assoc (lispm-comfyui-spec-type spec) lispm-comfyui-computed-widget-types)))
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
             (when-let* ((kind (lispm-comfyui-widget-kind spec))
                         ((not (eq kind 'display))))
               (let ((widget (list :name (concat prefix name) :spec spec :kind kind
                                   :index (and (lispm-comfyui--saved-kind-p kind) index))))
                 (when (lispm-comfyui--saved-kind-p kind)
                   (cl-incf index))
                 (when (lispm-comfyui--control-p (concat prefix name) spec kind)
                   (setq widget (plist-put widget :control index))
                   (cl-incf index))
                 (push widget widgets)
                 (when (eq kind 'dynamic)
                   (walk (lispm-comfyui--dynamic-inputs
                          spec (lispm-comfyui-widget-value widget values))
                         (concat prefix name ".")))
                 (dolist (extra (lispm-comfyui--format-inputs
                                 spec (lispm-comfyui-widget-value widget values)))
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
             (when-let* ((kind (lispm-comfyui-widget-kind spec))
                         ((lispm-comfyui--saved-kind-p kind)))
               (let* ((full (concat prefix name))
                      (cell (assoc full alist))
                      (value (if cell (cdr cell) (lispm-comfyui-widget-default spec))))
                 (push value values)
                 (cl-incf count)
                 (when (lispm-comfyui--control-p full spec kind)
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
widgets' values: a list; (:object . ALIST) for a node saving them by
name; for a subgraph's node, (:promoted . ALIST), the values of its
inputs standing for widgets inside, by name, or (:legacy . LIST),
values as an older frontend saved them, for its proxyWidgets.  EXTRA
is the rest of what was saved of it."
  id type title x y width height (mode 0) inputs outputs values extra)

(cl-defstruct (lispm-comfyui-link (:constructor lispm-comfyui-link-create) (:copier nil))
  "A link from output FROM-SLOT of the node FROM to input TO-SLOT of TO."
  id from from-slot to to-slot type)

(cl-defstruct (lispm-comfyui-graph (:constructor lispm-comfyui-graph-create) (:copier nil))
  "A workflow, or the inside of a subgraph: its NODES, LINKS and GROUPS,
the last node and link numbers given, and EXTRA, the rest of what was
saved of it.  GROUPS are kept as they were read.  A workflow's
SUBGRAPHS are the definitions of its subgraphs, those inside others
too; the inside of a subgraph has ROOT, the workflow, which numbers
its nodes and links, numbers being the workflow's own throughout."
  nodes links groups (last-node-id 0) (last-link-id 0) extra subgraphs root)

(cl-defstruct (lispm-comfyui-subgraph (:constructor lispm-comfyui-subgraph-create) (:copier nil))
  "The definition of a subgraph: its ID, a UUID, which is the type of its
nodes; its NAME; its INPUTS and OUTPUTS, slots each; GRAPH, what is
inside, where links from the node numbered -10 come from its inputs
and links to -20 go to its outputs; INPUT-NODE and OUTPUT-NODE, those
two nodes as saved; and EXTRA, the rest of what was saved of it."
  id name inputs outputs graph input-node output-node extra)

(defconst lispm-comfyui-input-node -10
  "The number of the node a subgraph's inputs come out of, inside it.")

(defconst lispm-comfyui-output-node -20
  "The number of the node a subgraph's outputs go into, inside it.")

(defun lispm-comfyui-graph-top (graph)
  "The workflow GRAPH is of: GRAPH, or the root of the subgraph it is."
  (or (lispm-comfyui-graph-root graph) graph))

(defun lispm-comfyui--new-node-id (graph)
  "A number for a new node of GRAPH, unused in its whole workflow."
  (cl-incf (lispm-comfyui-graph-last-node-id (lispm-comfyui-graph-top graph))))

(defun lispm-comfyui--new-link-id (graph)
  "A number for a new link of GRAPH, unused in its whole workflow."
  (cl-incf (lispm-comfyui-graph-last-link-id (lispm-comfyui-graph-top graph))))

(defun lispm-comfyui-subgraph (graph id)
  "The definition of the subgraph ID in GRAPH's workflow, or nil."
  (seq-find (lambda (subgraph) (equal (lispm-comfyui-subgraph-id subgraph) id))
            (lispm-comfyui-graph-subgraphs (lispm-comfyui-graph-top graph))))

(defun lispm-comfyui-node-subgraph (graph node)
  "The definition of the subgraph NODE, of GRAPH, is a node of, or nil."
  (lispm-comfyui-subgraph graph (lispm-comfyui-node-type node)))

(defun lispm-comfyui-uuid ()
  "A new random UUID, as ComfyUI's frontend names subgraphs."
  (apply #'format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
         (append (list (random 65536) (random 65536) (random 65536) (random 4096)
                       (logior #x8000 (random #x4000)))
                 (list (random 65536) (random 65536) (random 65536)))))

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

;; Read.

(defun lispm-comfyui--slot-from-json (alist)
  "The slot ALIST, a slot as saved, describes."
  (lispm-comfyui-slot-create
   :name (lispm-comfyui--field alist 'name)
   :type (let ((type (lispm-comfyui--field alist 'type)))
           (if (stringp type) type (format "%s" (or type "*"))))
   :widget (lispm-comfyui--field (lispm-comfyui--field alist 'widget) 'name)
   :extra (lispm-comfyui--without alist '(name type widget link links slot_index linkIds))))

(defun lispm-comfyui--node-from-json (alist subgraph-ids)
  "The node ALIST, a node as saved, describes.
SUBGRAPH-IDS are the types of the workflow's subgraphs' nodes."
  (let* ((pos (lispm-comfyui--pair (alist-get 'pos alist)))
         (size (lispm-comfyui--pair (alist-get 'size alist)))
         (values (alist-get 'widgets_values alist))
         (inputs (mapcar #'lispm-comfyui--slot-from-json (lispm-comfyui--field alist 'inputs)))
         (instance (member (lispm-comfyui--field alist 'type) subgraph-ids)))
    (lispm-comfyui-node-create
     :id (lispm-comfyui--field alist 'id)
     :type (lispm-comfyui--field alist 'type)
     :title (lispm-comfyui--field alist 'title)
     :x (car pos) :y (cdr pos) :width (car size) :height (cdr size)
     :mode (or (lispm-comfyui--field alist 'mode) 0)
     :inputs inputs
     :outputs (mapcar #'lispm-comfyui--slot-from-json (lispm-comfyui--field alist 'outputs))
     ;; A subgraph's node's values are read once its subgraph is.
     :values (cond (instance (cons :saved (and (vectorp values) (append values nil))))
                   ((vectorp values) (append values nil))
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
                               :type (let ((type (lispm-comfyui--field value 'type)))
                                       (if (stringp type) type "*")))))

(defun lispm-comfyui--graph-of-json (alist root subgraph-ids)
  "The nodes, links and groups of ALIST, a workflow or a subgraph, as a graph.
ROOT is the workflow, for a subgraph.  A link into an input that names
another link is let go, as the frontend lets it go."
  (let* ((nodes (mapcar (lambda (node) (lispm-comfyui--node-from-json node subgraph-ids))
                        (lispm-comfyui--field alist 'nodes)))
         (named (make-hash-table :test #'equal))
         (links (progn
                  (seq-doseq (node (lispm-comfyui--field alist 'nodes))
                    (cl-loop for input across (or (lispm-comfyui--field node 'inputs) [])
                             for index from 0
                             do (puthash (cons (format "%s" (lispm-comfyui--field node 'id)) index)
                                         (lispm-comfyui--field input 'link)
                                         named)))
                  (seq-filter
                   (lambda (link)
                     (let ((key (cons (format "%s" (lispm-comfyui-link-to link))
                                      (lispm-comfyui-link-to-slot link))))
                       (or (not (numberp (lispm-comfyui-link-to link)))
                           (< (lispm-comfyui-link-to link) 0)
                           (not (gethash key named))
                           (equal (gethash key named) (lispm-comfyui-link-id link)))))
                   (delq nil (mapcar (lambda (link) (and (not (memq link '(nil :null)))
                                                         (lispm-comfyui--link-from-json link)))
                                     (lispm-comfyui--field alist 'links)))))))
    (lispm-comfyui-graph-create
     :nodes nodes :links links :root root
     :groups (append (lispm-comfyui--field alist 'groups) nil)
     :last-node-id (apply #'max (or (lispm-comfyui--field alist 'last_node_id) 0)
                          (seq-filter #'numberp (mapcar #'lispm-comfyui-node-id nodes)))
     :last-link-id (apply #'max (or (lispm-comfyui--field alist 'last_link_id) 0)
                          (seq-filter #'numberp (mapcar #'lispm-comfyui-link-id links))))))

(defun lispm-comfyui--subgraph-from-json (alist root subgraph-ids)
  "The definition of a subgraph ALIST, as saved, describes, in ROOT."
  (lispm-comfyui-subgraph-create
   :id (lispm-comfyui--field alist 'id)
   :name (or (lispm-comfyui--field alist 'name) "Subgraph")
   :inputs (mapcar #'lispm-comfyui--slot-from-json (lispm-comfyui--field alist 'inputs))
   :outputs (mapcar #'lispm-comfyui--slot-from-json (lispm-comfyui--field alist 'outputs))
   :graph (lispm-comfyui--graph-of-json alist root subgraph-ids)
   :input-node (lispm-comfyui--field alist 'inputNode)
   :output-node (lispm-comfyui--field alist 'outputNode)
   :extra (lispm-comfyui--without
           alist '(id name inputs outputs nodes links groups inputNode outputNode))))

(defun lispm-comfyui-graph-from-json (alist &optional types)
  "The graph ALIST, a workflow as ComfyUI's frontend saves it, describes.
ALIST is that JSON as `json-parse-string' reads it with `:object-type'
`alist'.  Its subgraphs' nodes are made to have their subgraphs'
inputs and outputs, as the frontend makes them.  Given TYPES, the node
types of the server, the widgets an older frontend showed on them,
their proxyWidgets, become inputs, as the frontend makes them."
  (let* ((definitions (append (lispm-comfyui--field (lispm-comfyui--field alist 'definitions)
                                                    'subgraphs)
                              nil))
         (ids (mapcar (lambda (definition) (lispm-comfyui--field definition 'id)) definitions))
         (graph (lispm-comfyui--graph-of-json alist nil ids)))
    (setf (lispm-comfyui-graph-subgraphs graph)
          (mapcar (lambda (definition) (lispm-comfyui--subgraph-from-json definition graph ids))
                  definitions)
          (lispm-comfyui-graph-extra graph)
          (lispm-comfyui--without alist '(nodes links groups last_node_id last_link_id definitions)))
    (lispm-comfyui--renumber-subgraph-nodes graph (or (lispm-comfyui--field alist 'last_node_id) 0))
    ;; The numbers of the nodes and links inside subgraphs are the
    ;; workflow's own: new ones follow all of them.
    (dolist (subgraph (lispm-comfyui-graph-subgraphs graph))
      (let ((inside (lispm-comfyui-subgraph-graph subgraph))
            (state (lispm-comfyui--field (lispm-comfyui-subgraph-extra subgraph) 'state)))
        (setf (lispm-comfyui-graph-last-node-id graph)
              (max (lispm-comfyui-graph-last-node-id graph)
                   (lispm-comfyui-graph-last-node-id inside)
                   (or (lispm-comfyui--field state 'lastNodeId) 0))
              (lispm-comfyui-graph-last-link-id graph)
              (max (lispm-comfyui-graph-last-link-id graph)
                   (lispm-comfyui-graph-last-link-id inside)
                   (or (lispm-comfyui--field state 'lastLinkId) 0)))))
    (dolist (inside (lispm-comfyui--graphs graph))
      (dolist (node (lispm-comfyui-graph-nodes inside))
        (when-let* ((subgraph (lispm-comfyui-node-subgraph graph node))
                    ((eq (car-safe (lispm-comfyui-node-values node)) :saved)))
          (lispm-comfyui--read-instance-values node subgraph))))
    (lispm-comfyui-align-instances graph types)
    (when types (lispm-comfyui-migrate-proxy-widgets graph types))
    graph))

(defun lispm-comfyui--renumber-subgraph-nodes (graph last)
  "Give the nodes inside GRAPH's subgraphs numbers no other node has.
As ComfyUI's frontend does when it reads a workflow: a node whose
number a node of the workflow or of a subgraph before has gets the
next after LAST, the workflow's last, and the links and proxyWidgets
naming it follow."
  (let ((used (make-hash-table :test #'equal))
        (renumbered nil))
    (dolist (node (lispm-comfyui-graph-nodes graph))
      (puthash (format "%s" (lispm-comfyui-node-id node)) t used))
    (dolist (subgraph (lispm-comfyui-graph-subgraphs graph))
      (let ((inside (lispm-comfyui-subgraph-graph subgraph))
            (map nil))
        (dolist (node (lispm-comfyui-graph-nodes inside))
          (let ((id (lispm-comfyui-node-id node)))
            (if (gethash (format "%s" id) used)
                (let ((new (cl-loop do (cl-incf last)
                                    unless (gethash (format "%s" last) used) return last)))
                  (push (cons id new) map)
                  (setf (lispm-comfyui-node-id node) new)
                  (puthash (format "%s" new) t used))
              (puthash (format "%s" id) t used)
              (when (numberp id) (setq last (max last id))))))
        (when map
          (dolist (link (lispm-comfyui-graph-links inside))
            (when-let* ((cell (assoc (lispm-comfyui-link-from link) map)))
              (setf (lispm-comfyui-link-from link) (cdr cell)))
            (when-let* ((cell (assoc (lispm-comfyui-link-to link) map)))
              (setf (lispm-comfyui-link-to link) (cdr cell))))
          (push (cons (lispm-comfyui-subgraph-id subgraph) map) renumbered))))
    (dolist (inside (lispm-comfyui--graphs graph))
      (dolist (node (lispm-comfyui-graph-nodes inside))
        (when-let* ((map (cdr (assoc (lispm-comfyui-node-type node) renumbered)))
                    (entries (lispm-comfyui--field (alist-get 'properties (lispm-comfyui-node-extra node))
                                                   'proxyWidgets)))
          (seq-doseq (entry entries)
            (when-let* (((vectorp entry))
                        (cell (seq-find (lambda (cell) (equal (format "%s" (car cell))
                                                              (format "%s" (aref entry 0))))
                                        map)))
              (aset entry 0 (format "%s" (cdr cell))))))))
    (setf (lispm-comfyui-graph-last-node-id graph)
          (max last (lispm-comfyui-graph-last-node-id graph)))))

(defun lispm-comfyui--promoted-p (subgraph index)
  "Non-nil if SUBGRAPH's input INDEX stands for a widget inside.
As ComfyUI's frontend makes it: linked to an input that is a widget's."
  (let ((inside (lispm-comfyui-subgraph-graph subgraph)))
    (seq-find (lambda (link)
                (and (eql (lispm-comfyui-link-from link) lispm-comfyui-input-node)
                     (eql (lispm-comfyui-link-from-slot link) index)
                     (when-let* ((node (lispm-comfyui-node inside (lispm-comfyui-link-to link)))
                                 (slot (nth (lispm-comfyui-link-to-slot link) (lispm-comfyui-node-inputs node))))
                       (lispm-comfyui-slot-widget slot))))
              (lispm-comfyui-graph-links inside))))

(defun lispm-comfyui--hole-p (value)
  "Non-nil if VALUE, as saved for a widget, holds nothing."
  (or (memq value '(nil :null)) (and (consp value) (consp (car value)))))

(defun lispm-comfyui--read-instance-values (node subgraph)
  "Read NODE's values as saved, one for each of SUBGRAPH's inputs standing
for a widget, in their order, as the frontend reads them, a value its
quarantine of proxyWidgets keeps winning.  A node with proxyWidgets
keeps the values as saved too, for `lispm-comfyui-migrate-proxy-widgets'."
  (let* ((values (cdr (lispm-comfyui-node-values node)))
         (properties (alist-get 'properties (lispm-comfyui-node-extra node)))
         (quarantine (cl-loop for entry across (or (lispm-comfyui--field properties 'proxyWidgetErrorQuarantine) [])
                              for original = (lispm-comfyui--field entry 'originalEntry)
                              when (and (vectorp original) (equal (format "%s" (aref original 0)) "-1")
                                        (assq 'hostValue entry))
                              collect (cons (aref original 1) (alist-get 'hostValue entry))))
         (rest values)
         (promoted nil))
    (cl-loop for slot in (lispm-comfyui-subgraph-inputs subgraph)
             for index from 0
             when (lispm-comfyui--promoted-p subgraph index)
             do (let* ((name (lispm-comfyui-slot-name slot))
                       (cell (assoc name quarantine))
                       (value (if cell (cdr cell) (car rest))))
                  (setq rest (cdr rest))
                  (unless (lispm-comfyui--hole-p value)
                    (push (cons name value) promoted))))
    (setf (lispm-comfyui-node-values node)
          (if (lispm-comfyui--field properties 'proxyWidgets)
              (cons :legacy (cons (nreverse promoted) values))
            (cons :promoted (nreverse promoted))))))

(defun lispm-comfyui-graph-from-string (string &optional types)
  "The graph of STRING, a workflow's JSON; see `lispm-comfyui-graph-from-json'."
  (lispm-comfyui-graph-from-json (json-parse-string string :object-type 'alist) types))

;; Write.

(defun lispm-comfyui--slot-json (slot &rest more)
  "SLOT as saved, with MORE, more fields as alternating keys and values."
  (append `((name . ,(lispm-comfyui-slot-name slot))
            (type . ,(lispm-comfyui-slot-type slot)))
          (when (lispm-comfyui-slot-widget slot)
            `((widget . ((name . ,(lispm-comfyui-slot-widget slot))))))
          (cl-loop for (key value) on more by #'cddr collect (cons key value))
          (lispm-comfyui-slot-extra slot)))

(defun lispm-comfyui--values-json (node)
  "NODE's widgets' values as saved."
  (let ((values (lispm-comfyui-node-values node)))
    (pcase (car-safe values)
      (:object (cdr values))
      (:legacy (vconcat (cddr values)))
      (:promoted
       (vconcat (cl-loop for slot in (lispm-comfyui-node-inputs node)
                         when (lispm-comfyui-slot-widget slot)
                         collect (let ((cell (assoc (lispm-comfyui-slot-name slot) (cdr values))))
                                   (if cell (cdr cell) :null)))))
      (_ (vconcat values)))))

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
     `((widgets_values . ,(lispm-comfyui--values-json node))))))

(defun lispm-comfyui--link-json (link &optional object)
  "LINK as saved: an array, or an OBJECT, as a subgraph's are."
  (if object
      `((id . ,(lispm-comfyui-link-id link)) (origin_id . ,(lispm-comfyui-link-from link))
        (origin_slot . ,(lispm-comfyui-link-from-slot link)) (target_id . ,(lispm-comfyui-link-to link))
        (target_slot . ,(lispm-comfyui-link-to-slot link)) (type . ,(lispm-comfyui-link-type link)))
    (vector (lispm-comfyui-link-id link) (lispm-comfyui-link-from link)
            (lispm-comfyui-link-from-slot link) (lispm-comfyui-link-to link)
            (lispm-comfyui-link-to-slot link) (lispm-comfyui-link-type link))))

(defun lispm-comfyui--subgraph-json (subgraph root)
  "The definition SUBGRAPH, of the workflow ROOT, as saved."
  (let* ((inside (lispm-comfyui-subgraph-graph subgraph))
         (links (lispm-comfyui-graph-links inside))
         (extra (lispm-comfyui-subgraph-extra subgraph))
         (state (lispm-comfyui--field extra 'state)))
    (append
     `((id . ,(lispm-comfyui-subgraph-id subgraph))
       (name . ,(lispm-comfyui-subgraph-name subgraph))
       (state . ((lastGroupId . ,(or (lispm-comfyui--field state 'lastGroupId) 0))
                 (lastNodeId . ,(lispm-comfyui-graph-last-node-id root))
                 (lastLinkId . ,(lispm-comfyui-graph-last-link-id root))
                 (lastRerouteId . ,(or (lispm-comfyui--field state 'lastRerouteId) 0))))
       (inputNode . ,(or (lispm-comfyui-subgraph-input-node subgraph)
                         `((id . ,lispm-comfyui-input-node) (bounding . [0 0 120 60]))))
       (outputNode . ,(or (lispm-comfyui-subgraph-output-node subgraph)
                          `((id . ,lispm-comfyui-output-node) (bounding . [600 0 120 60]))))
       (inputs . ,(vconcat
                   (cl-loop for slot in (lispm-comfyui-subgraph-inputs subgraph)
                            for index from 0
                            collect (lispm-comfyui--slot-json
                                     slot 'linkIds
                                     (vconcat (cl-loop for link in links
                                                       when (and (eql (lispm-comfyui-link-from link)
                                                                      lispm-comfyui-input-node)
                                                                 (eql (lispm-comfyui-link-from-slot link) index))
                                                       collect (lispm-comfyui-link-id link)))))))
       (outputs . ,(vconcat
                    (cl-loop for slot in (lispm-comfyui-subgraph-outputs subgraph)
                             for index from 0
                             collect (lispm-comfyui--slot-json
                                      slot 'linkIds
                                      (vconcat (cl-loop for link in links
                                                        when (and (eql (lispm-comfyui-link-to link)
                                                                       lispm-comfyui-output-node)
                                                                  (eql (lispm-comfyui-link-to-slot link) index))
                                                        collect (lispm-comfyui-link-id link)))))))
       (nodes . ,(vconcat (mapcar (lambda (node) (lispm-comfyui--node-json inside node))
                                  (lispm-comfyui-graph-nodes inside))))
       (groups . ,(vconcat (lispm-comfyui-graph-groups inside)))
       (links . ,(vconcat (mapcar (lambda (link) (lispm-comfyui--link-json link t)) links))))
     (lispm-comfyui--without extra '(state))
     (unless (assq 'version extra)
       '((version . 1) (revision . 0) (config . nil) (widgets . []) (extra . nil))))))

(defun lispm-comfyui-used-subgraphs (graph)
  "The definitions of the subgraphs GRAPH's workflow has nodes of.
Those of nodes inside its subgraphs too."
  (let ((used nil) (seen nil))
    (cl-labels ((visit (inside)
                  (dolist (node (lispm-comfyui-graph-nodes inside))
                    (when-let* ((subgraph (lispm-comfyui-node-subgraph graph node))
                                ((not (memq subgraph seen))))
                      (push subgraph seen)
                      (visit (lispm-comfyui-subgraph-graph subgraph))))))
      (visit graph))
    (dolist (subgraph (lispm-comfyui-graph-subgraphs graph))
      (when (memq subgraph seen) (push subgraph used)))
    (nreverse used)))

(defun lispm-comfyui-graph-json (graph)
  "GRAPH, a workflow, as ComfyUI's frontend saves it, to `json-serialize'.
Only the subgraphs it has nodes of are saved with it."
  (let ((subgraphs (lispm-comfyui-used-subgraphs graph)))
    (append
     `((last_node_id . ,(lispm-comfyui-graph-last-node-id graph))
       (last_link_id . ,(lispm-comfyui-graph-last-link-id graph))
       (nodes . ,(vconcat (mapcar (lambda (node) (lispm-comfyui--node-json graph node))
                                  (lispm-comfyui-graph-nodes graph))))
       (links . ,(vconcat (mapcar #'lispm-comfyui--link-json (lispm-comfyui-graph-links graph))))
       (groups . ,(vconcat (lispm-comfyui-graph-groups graph))))
     (when subgraphs
       `((definitions . ((subgraphs . ,(vconcat (mapcar (lambda (subgraph)
                                                          (lispm-comfyui--subgraph-json subgraph graph))
                                                        subgraphs)))))))
     (or (lispm-comfyui-graph-extra graph)
         '((config . nil) (extra . nil) (version . 0.4))))))

(defun lispm-comfyui-graph-string (graph)
  "GRAPH as the JSON of a workflow, a string."
  (json-serialize (lispm-comfyui-graph-json graph)))

;; Copy.

(defun lispm-comfyui--slot-copy (slot)
  "A copy of SLOT."
  (lispm-comfyui-slot-create
   :name (lispm-comfyui-slot-name slot) :type (lispm-comfyui-slot-type slot)
   :widget (lispm-comfyui-slot-widget slot)
   :extra (copy-tree (lispm-comfyui-slot-extra slot) t)))

(defun lispm-comfyui-node-copy (node)
  "A copy of NODE sharing nothing with it that an edit changes."
  (lispm-comfyui-node-create
   :id (lispm-comfyui-node-id node) :type (lispm-comfyui-node-type node)
   :title (lispm-comfyui-node-title node)
   :x (lispm-comfyui-node-x node) :y (lispm-comfyui-node-y node)
   :width (lispm-comfyui-node-width node)
   :height (lispm-comfyui-node-height node)
   :mode (lispm-comfyui-node-mode node)
   :inputs (mapcar #'lispm-comfyui--slot-copy (lispm-comfyui-node-inputs node))
   :outputs (mapcar #'lispm-comfyui--slot-copy (lispm-comfyui-node-outputs node))
   :values (copy-tree (lispm-comfyui-node-values node) t)
   :extra (copy-tree (lispm-comfyui-node-extra node) t)))

(defun lispm-comfyui--link-copy (link)
  "A copy of LINK."
  (lispm-comfyui-link-create
   :id (lispm-comfyui-link-id link) :from (lispm-comfyui-link-from link)
   :from-slot (lispm-comfyui-link-from-slot link)
   :to (lispm-comfyui-link-to link) :to-slot (lispm-comfyui-link-to-slot link)
   :type (lispm-comfyui-link-type link)))

(defun lispm-comfyui--graph-copy (graph root)
  "A copy of GRAPH's nodes, links and groups, ROOT its workflow's copy."
  (lispm-comfyui-graph-create
   :nodes (mapcar #'lispm-comfyui-node-copy (lispm-comfyui-graph-nodes graph))
   :links (mapcar #'lispm-comfyui--link-copy (lispm-comfyui-graph-links graph))
   :groups (copy-tree (lispm-comfyui-graph-groups graph) t)
   :last-node-id (lispm-comfyui-graph-last-node-id graph)
   :last-link-id (lispm-comfyui-graph-last-link-id graph)
   :extra (copy-tree (lispm-comfyui-graph-extra graph) t)
   :root root))

(defun lispm-comfyui-graph-copy (graph)
  "A copy of GRAPH, a workflow, sharing nothing with it that an edit changes.
Its subgraphs are copied with it."
  (let ((copy (lispm-comfyui--graph-copy graph nil)))
    (setf (lispm-comfyui-graph-subgraphs copy)
          (mapcar (lambda (subgraph)
                    (lispm-comfyui-subgraph-create
                     :id (lispm-comfyui-subgraph-id subgraph)
                     :name (lispm-comfyui-subgraph-name subgraph)
                     :inputs (mapcar #'lispm-comfyui--slot-copy (lispm-comfyui-subgraph-inputs subgraph))
                     :outputs (mapcar #'lispm-comfyui--slot-copy (lispm-comfyui-subgraph-outputs subgraph))
                     :graph (lispm-comfyui--graph-copy (lispm-comfyui-subgraph-graph subgraph) copy)
                     :input-node (copy-tree (lispm-comfyui-subgraph-input-node subgraph) t)
                     :output-node (copy-tree (lispm-comfyui-subgraph-output-node subgraph) t)
                     :extra (copy-tree (lispm-comfyui-subgraph-extra subgraph) t)))
                  (lispm-comfyui-graph-subgraphs graph)))
    copy))

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

(defun lispm-comfyui-node-value-list (node types)
  "NODE's widgets' values as a list, in the order of its type in TYPES.
A subgraph's node's are its inputs standing for widgets inside, each
holding the value of the widget inside where it holds none of its own."
  (let ((values (lispm-comfyui-node-values node)))
    (pcase (car-safe values)
      (:promoted
       (when-let* ((definition (gethash (lispm-comfyui-node-type node) types)))
         (cl-loop for (name spec) in (lispm-comfyui-type-inputs definition)
                  when (lispm-comfyui-widget-kind spec)
                  collect (let ((cell (assoc name (cdr values))))
                            (if cell (cdr cell) (lispm-comfyui-widget-default spec))))))
      (:legacy nil)
      (_ values))))

(defun lispm-comfyui-node-widgets (node types)
  "The widgets of NODE, whose type is in TYPES, as `lispm-comfyui-widgets'.
Nil for a node of a type TYPES has not, or one saving its values by name.
TYPES has subgraphs' types too, see `lispm-comfyui-graph-types'."
  (let ((definition (gethash (lispm-comfyui-node-type node) types))
        (values (lispm-comfyui-node-values node)))
    (when (and definition (not (lispm-comfyui-values-by-name-p values)))
      (lispm-comfyui-widgets definition (lispm-comfyui-node-value-list node types)))))

(defun lispm-comfyui-node-widget-alist (node types)
  "The values of NODE's widgets by name; see `lispm-comfyui-widget-alist'."
  (let ((definition (gethash (lispm-comfyui-node-type node) types))
        (values (lispm-comfyui-node-values node)))
    (cond ((lispm-comfyui-values-by-name-p values)
           (mapcar (lambda (entry) (cons (symbol-name (car entry)) (cdr entry))) (cdr values)))
          (definition (lispm-comfyui-widget-alist
                       definition (lispm-comfyui-node-value-list node types))))))

;;;; Nodes only the frontend runs

(defconst lispm-comfyui-virtual-types '("Note" "MarkdownNote" "Reroute" "PrimitiveNode")
  "Types of nodes that only ComfyUI's frontend knows, which never run.")

(define-error 'lispm-comfyui-error "ComfyUI")

;;;; Editing a graph

(defun lispm-comfyui-node-make (graph definition type x y)
  "A new node of TYPE, of DEFINITION, at X Y, numbered next in GRAPH.
The node is not yet in GRAPH."
  (let ((id (lispm-comfyui--new-node-id graph))
        (subgraph (alist-get 'lispm-subgraph definition)))
    (lispm-comfyui-node-create
     :id id :type type :x x :y y :width 240
     ;; A subgraph's node has all its subgraph's inputs, those standing
     ;; for widgets inside as widgets.
     :inputs (cl-loop for (name spec) in (lispm-comfyui-type-inputs definition)
                      when (or subgraph (not (lispm-comfyui-widget-kind spec)))
                      collect (lispm-comfyui-slot-create
                               :name name :type (lispm-comfyui-socket-type spec)
                               :widget (and (lispm-comfyui-widget-kind spec) name)))
     :outputs (cl-loop for (name type) in (lispm-comfyui-type-outputs definition)
                       collect (lispm-comfyui-slot-create :name name :type type))
     :values (if subgraph (list :promoted)
               (lispm-comfyui-values-from-alist definition nil))
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
                 :id (lispm-comfyui--new-link-id graph)
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

(cl-defun lispm-comfyui-set-widget (node types name value)
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
      (when (eq (car-safe values) :promoted)
        (setf (alist-get name (cdr values) nil nil #'equal) value)
        (cl-return-from lispm-comfyui-set-widget value))
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
    (dolist (widget (and (listp (lispm-comfyui-node-values node))
                         (not (keywordp (car (lispm-comfyui-node-values node))))
                         (lispm-comfyui-node-widgets node types)))
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

;;;; Subgraphs

(defun lispm-comfyui-subgraph-io-node (subgraph which)
  "A node standing for SUBGRAPH's inputs, WHICH being `input', or outputs.
Inside SUBGRAPH, links from it carry the subgraph's inputs, links to
it its outputs: its inputs' slots are its outputs, and the other way
round.  It is no node of the graph, only drawn and linked to."
  (let* ((input (eq which 'input))
         (saved (if input (lispm-comfyui-subgraph-input-node subgraph)
                  (lispm-comfyui-subgraph-output-node subgraph)))
         (bounding (lispm-comfyui--field saved 'bounding))
         (slots (if input (lispm-comfyui-subgraph-inputs subgraph)
                  (lispm-comfyui-subgraph-outputs subgraph))))
    (lispm-comfyui-node-create
     :id (if input lispm-comfyui-input-node lispm-comfyui-output-node)
     :type (if input "Subgraph Inputs" "Subgraph Outputs")
     :title (if input "Inputs" "Outputs")
     :x (if (vectorp bounding) (aref bounding 0) 0)
     :y (+ 30 (if (vectorp bounding) (aref bounding 1) 0))
     :width 160
     :inputs (unless input slots)
     :outputs (when input slots)
     :values nil)))

(defun lispm-comfyui--unique-name (name names)
  "NAME, or NAME followed by _1, _2 and so on, the first not among NAMES."
  (if (not (member name names)) name
    (cl-loop for n from 1
             for candidate = (format "%s_%d" name n)
             unless (member candidate names) return candidate)))

(defun lispm-comfyui--inner-widget (subgraph index types)
  "The widget inside SUBGRAPH its input INDEX stands for, or nil.
Returns (NODE . WIDGET), WIDGET as `lispm-comfyui-widgets' makes them:
that of the first input it is linked to that is a widget's, as
ComfyUI's frontend shows it on the subgraph's nodes.  TYPES has the
subgraphs' types, see `lispm-comfyui-graph-types'."
  (let ((inside (lispm-comfyui-subgraph-graph subgraph)))
    (cl-loop for link in (lispm-comfyui-graph-links inside)
             when (and (eql (lispm-comfyui-link-from link) lispm-comfyui-input-node)
                       (eql (lispm-comfyui-link-from-slot link) index))
             thereis
             (let* ((node (lispm-comfyui-node inside (lispm-comfyui-link-to link)))
                    (slot (and node (nth (lispm-comfyui-link-to-slot link)
                                         (lispm-comfyui-node-inputs node))))
                    (name (and slot (lispm-comfyui-slot-widget slot))))
               (when-let* ((widget (and name (seq-find (lambda (widget)
                                                         (equal (plist-get widget :name) name))
                                                       (lispm-comfyui-node-widgets node types)))))
                 (cons node widget))))))

(defun lispm-comfyui--promoted-spec (node widget types)
  "The specification of an input standing for WIDGET of NODE, of TYPES.
As WIDGET's, its value then the default, with no control: a subgraph's
node holds a single value for it."
  (let* ((spec (plist-get widget :spec))
         (type (if (eq (plist-get widget :kind) 'dynamic)
                   (vconcat (lispm-comfyui-widget-choices spec))
                 (lispm-comfyui-spec-type spec)))
         (value (lispm-comfyui-widget-value widget (lispm-comfyui-node-value-list node types))))
    (vector type (append (list (cons 'default value) (cons 'control_after_generate :false))
                         (seq-remove (lambda (entry)
                                       (memq (car entry) '(default control_after_generate forceInput)))
                                     (lispm-comfyui-spec-options spec))))))

(defun lispm-comfyui--subgraph-definition (subgraph types)
  "A node type's definition, as /object_info has them, for SUBGRAPH's nodes.
Its inputs are SUBGRAPH's, those standing for widgets inside widgets
of theirs; its outputs SUBGRAPH's.  `lispm-subgraph' holds SUBGRAPH."
  (let ((inputs (lispm-comfyui-subgraph-inputs subgraph))
        (outputs (lispm-comfyui-subgraph-outputs subgraph)))
    `((input . ((required
                 . ,(cl-loop for slot in inputs
                             for index from 0
                             collect (cons (intern (lispm-comfyui-slot-name slot))
                                           (if-let* ((found (lispm-comfyui--inner-widget
                                                             subgraph index types)))
                                               (lispm-comfyui--promoted-spec (car found) (cdr found) types)
                                             (vector (lispm-comfyui-slot-type slot))))))))
      (input_order . ((required . ,(vconcat (mapcar #'lispm-comfyui-slot-name inputs)))))
      (output . ,(vconcat (mapcar #'lispm-comfyui-slot-type outputs)))
      (output_name . ,(vconcat (mapcar #'lispm-comfyui-slot-name outputs)))
      (output_is_list . [])
      (name . ,(lispm-comfyui-subgraph-id subgraph))
      (display_name . ,(lispm-comfyui-subgraph-name subgraph))
      (category . "subgraph")
      (description . "")
      (lispm-subgraph . ,subgraph))))

(defun lispm-comfyui-graph-types (graph types)
  "TYPES, the server's node types, with the types of GRAPH's subgraphs' nodes.
The table is TYPES itself when GRAPH's workflow has no subgraphs."
  (let ((subgraphs (lispm-comfyui-graph-subgraphs (lispm-comfyui-graph-top graph))))
    (if (null subgraphs)
        types
      (let ((merged (copy-hash-table types)))
        (cl-labels ((define (subgraph seen)
                      (unless (or (gethash (lispm-comfyui-subgraph-id subgraph) merged)
                                  (memq subgraph seen))
                        ;; Those inside first, whose widgets it may show.
                        (dolist (node (lispm-comfyui-graph-nodes (lispm-comfyui-subgraph-graph subgraph)))
                          (when-let* ((inner (lispm-comfyui-node-subgraph graph node)))
                            (define inner (cons subgraph seen))))
                        (puthash (lispm-comfyui-subgraph-id subgraph)
                                 (lispm-comfyui--subgraph-definition subgraph merged)
                                 merged))))
          (dolist (subgraph subgraphs) (define subgraph nil)))
        merged))))

(defun lispm-comfyui--graphs (graph)
  "GRAPH's workflow and the insides of its subgraphs."
  (let ((top (lispm-comfyui-graph-top graph)))
    (cons top (mapcar #'lispm-comfyui-subgraph-graph (lispm-comfyui-graph-subgraphs top)))))

(defun lispm-comfyui--align-instance (graph node subgraph types)
  "Make NODE, of GRAPH, have SUBGRAPH's inputs and outputs, in their order.
Links and values follow inputs by name; an input or output SUBGRAPH no
longer has goes, with its links.  Given TYPES, with the subgraphs'
types, the inputs standing for widgets inside are marked so, and those
holding no value hold the widget's."
  (let ((id (lispm-comfyui-node-id node))
        (old-inputs (lispm-comfyui-node-inputs node))
        (old-outputs (lispm-comfyui-node-outputs node))
        (named (lambda (slots name)
                 (seq-find (lambda (slot) (equal (lispm-comfyui-slot-name slot) name)) slots))))
    (let ((inputs (mapcar (lambda (slot)
                            (let ((old (funcall named old-inputs (lispm-comfyui-slot-name slot))))
                              (lispm-comfyui-slot-create
                               :name (lispm-comfyui-slot-name slot) :type (lispm-comfyui-slot-type slot)
                               :widget (and old (lispm-comfyui-slot-widget old))
                               :extra (and old (lispm-comfyui-slot-extra old)))))
                          (lispm-comfyui-subgraph-inputs subgraph)))
          (outputs (mapcar (lambda (slot)
                             (let ((old (funcall named old-outputs (lispm-comfyui-slot-name slot))))
                               (lispm-comfyui-slot-create
                                :name (lispm-comfyui-slot-name slot) :type (lispm-comfyui-slot-type slot)
                                :extra (and old (lispm-comfyui-slot-extra old)))))
                           (lispm-comfyui-subgraph-outputs subgraph))))
      (dolist (link (copy-sequence (lispm-comfyui-graph-links graph)))
        (let ((index
               (cond ((equal (lispm-comfyui-link-to link) id)
                      (let ((old (nth (lispm-comfyui-link-to-slot link) old-inputs)))
                        (or (and old (seq-position inputs (lispm-comfyui-slot-name old)
                                                   (lambda (slot name)
                                                     (equal (lispm-comfyui-slot-name slot) name))))
                            :gone)))
                     ((equal (lispm-comfyui-link-from link) id)
                      (let ((old (nth (lispm-comfyui-link-from-slot link) old-outputs)))
                        (or (and old (seq-position outputs (lispm-comfyui-slot-name old)
                                                   (lambda (slot name)
                                                     (equal (lispm-comfyui-slot-name slot) name))))
                            :gone))))))
          (cond ((eq index :gone)
                 (setf (lispm-comfyui-graph-links graph) (delq link (lispm-comfyui-graph-links graph))))
                ((null index))
                ((equal (lispm-comfyui-link-to link) id) (setf (lispm-comfyui-link-to-slot link) index))
                (t (setf (lispm-comfyui-link-from-slot link) index)))))
      (when types
        (cl-loop for slot in inputs
                 for index from 0
                 for found = (lispm-comfyui--inner-widget subgraph index types)
                 do (setf (lispm-comfyui-slot-widget slot)
                          (and (lispm-comfyui--promoted-p subgraph index)
                               (lispm-comfyui-slot-name slot)))
                 when (and found (eq (car-safe (lispm-comfyui-node-values node)) :promoted)
                           (not (assoc (lispm-comfyui-slot-name slot)
                                       (cdr (lispm-comfyui-node-values node)))))
                 do (nconc (lispm-comfyui-node-values node)
                           (list (cons (lispm-comfyui-slot-name slot)
                                       (lispm-comfyui-widget-value
                                        (cdr found)
                                        (lispm-comfyui-node-value-list (car found) types)))))))
      (setf (lispm-comfyui-node-inputs node) inputs
            (lispm-comfyui-node-outputs node) outputs))))

(defun lispm-comfyui-align-instances (graph &optional types)
  "Make each subgraph's node of GRAPH's workflow have its subgraph's slots.
See `lispm-comfyui--align-instance'; TYPES are the server's node types."
  (let* ((top (lispm-comfyui-graph-top graph))
         (merged (and types (lispm-comfyui-graph-types top types))))
    (dolist (inside (lispm-comfyui--graphs top))
      (dolist (node (lispm-comfyui-graph-nodes inside))
        (when-let* ((subgraph (lispm-comfyui-node-subgraph top node)))
          (lispm-comfyui--align-instance inside node subgraph merged))))))

(defun lispm-comfyui--add-link (graph from from-slot to to-slot type)
  "Link output FROM-SLOT of the node numbered FROM to input TO-SLOT of TO.
In GRAPH, unchecked, replacing the link into that input; return it."
  (setf (lispm-comfyui-graph-links graph)
        (seq-remove (lambda (link) (and (equal (lispm-comfyui-link-to link) to)
                                        (equal (lispm-comfyui-link-to-slot link) to-slot)))
                    (lispm-comfyui-graph-links graph)))
  (let ((link (lispm-comfyui-link-create :id (lispm-comfyui--new-link-id graph)
                                         :from from :from-slot from-slot
                                         :to to :to-slot to-slot :type type)))
    (setf (lispm-comfyui-graph-links graph) (append (lispm-comfyui-graph-links graph) (list link)))
    link))

(defun lispm-comfyui-subgraph-add-slot (graph subgraph which name type)
  "Give SUBGRAPH, of GRAPH's workflow, a new input, WHICH `input', or output.
Named NAME, or NAME_1 and so on if taken, of TYPE; return its index.
Its nodes get it too."
  (let* ((input (eq which 'input))
         (slots (if input (lispm-comfyui-subgraph-inputs subgraph)
                  (lispm-comfyui-subgraph-outputs subgraph)))
         (slot (lispm-comfyui-slot-create
                :name (lispm-comfyui--unique-name name (mapcar #'lispm-comfyui-slot-name slots))
                :type type :extra `((id . ,(lispm-comfyui-uuid))))))
    (if input
        (setf (lispm-comfyui-subgraph-inputs subgraph) (append slots (list slot)))
      (setf (lispm-comfyui-subgraph-outputs subgraph) (append slots (list slot))))
    (lispm-comfyui-align-instances graph)
    (length slots)))

(defun lispm-comfyui-subgraph-remove-slot (graph subgraph which index)
  "Take input INDEX, WHICH `input', or output INDEX from SUBGRAPH.
With its links inside, and those of SUBGRAPH's nodes, in GRAPH's workflow."
  (let* ((input (eq which 'input))
         (inside (lispm-comfyui-subgraph-graph subgraph))
         (io (if input lispm-comfyui-input-node lispm-comfyui-output-node)))
    (setf (lispm-comfyui-graph-links inside)
          (seq-remove (lambda (link)
                        (if input
                            (and (eql (lispm-comfyui-link-from link) io)
                                 (eql (lispm-comfyui-link-from-slot link) index))
                          (and (eql (lispm-comfyui-link-to link) io)
                               (eql (lispm-comfyui-link-to-slot link) index))))
                      (lispm-comfyui-graph-links inside)))
    (dolist (link (lispm-comfyui-graph-links inside))
      (if input
          (when (and (eql (lispm-comfyui-link-from link) io)
                     (> (lispm-comfyui-link-from-slot link) index))
            (cl-decf (lispm-comfyui-link-from-slot link)))
        (when (and (eql (lispm-comfyui-link-to link) io)
                   (> (lispm-comfyui-link-to-slot link) index))
          (cl-decf (lispm-comfyui-link-to-slot link)))))
    (if input
        (setf (lispm-comfyui-subgraph-inputs subgraph)
              (append (seq-take (lispm-comfyui-subgraph-inputs subgraph) index)
                      (nthcdr (1+ index) (lispm-comfyui-subgraph-inputs subgraph))))
      (setf (lispm-comfyui-subgraph-outputs subgraph)
            (append (seq-take (lispm-comfyui-subgraph-outputs subgraph) index)
                    (nthcdr (1+ index) (lispm-comfyui-subgraph-outputs subgraph)))))
    (lispm-comfyui-align-instances graph)))

(defun lispm-comfyui-subgraph-rename-slot (graph subgraph which index name)
  "Name SUBGRAPH's input INDEX, WHICH `input', or output INDEX, NAME.
Its nodes in GRAPH's workflow keep their links and values for it."
  (let* ((input (eq which 'input))
         (slot (nth index (if input (lispm-comfyui-subgraph-inputs subgraph)
                            (lispm-comfyui-subgraph-outputs subgraph))))
         (old (lispm-comfyui-slot-name slot))
         (top (lispm-comfyui-graph-top graph))
         (name (lispm-comfyui--unique-name
                name (remove old (mapcar #'lispm-comfyui-slot-name
                                         (if input (lispm-comfyui-subgraph-inputs subgraph)
                                           (lispm-comfyui-subgraph-outputs subgraph)))))))
    (setf (lispm-comfyui-slot-name slot) name)
    (dolist (inside (lispm-comfyui--graphs top))
      (dolist (node (lispm-comfyui-graph-nodes inside))
        (when (eq (lispm-comfyui-node-subgraph top node) subgraph)
          (dolist (each (if input (lispm-comfyui-node-inputs node) (lispm-comfyui-node-outputs node)))
            (when (equal (lispm-comfyui-slot-name each) old)
              (setf (lispm-comfyui-slot-name each) name)
              (when (lispm-comfyui-slot-widget each)
                (setf (lispm-comfyui-slot-widget each) name))))
          (when-let* ((cell (and input (eq (car-safe (lispm-comfyui-node-values node)) :promoted)
                                 (assoc old (cdr (lispm-comfyui-node-values node))))))
            (setcar cell name)))))
    name))

(defun lispm-comfyui-migrate-proxy-widgets (graph types)
  "Make the widgets older frontends showed on subgraphs' nodes inputs.
Each entry of a node's proxyWidgets, a widget inside it, takes the
node's value at its place: through the subgraph's input linked to the
widget, or one made for it, as ComfyUI's frontend does.  Entries of
nested subgraphs and previews are let go.  TYPES are the server's."
  (let ((top (lispm-comfyui-graph-top graph)))
    (dolist (inside (lispm-comfyui--graphs top))
      (dolist (node (lispm-comfyui-graph-nodes inside))
        (when (eq (car-safe (lispm-comfyui-node-values node)) :legacy)
          (lispm-comfyui--migrate-node top node types))))
    (lispm-comfyui-align-instances top types)))

(defun lispm-comfyui--migrate-node (top node types)
  "Make NODE's proxyWidgets inputs of its subgraph, in TOP's workflow."
  (let* ((subgraph (lispm-comfyui-node-subgraph top node))
         (properties (alist-get 'properties (lispm-comfyui-node-extra node)))
         (entries (append (lispm-comfyui--field properties 'proxyWidgets) nil))
         (values (cddr (lispm-comfyui-node-values node)))
         ;; The frontend first gives the inputs standing for widgets the
         ;; values in turn, then each entry its own.
         (promoted (copy-sequence (cadr (lispm-comfyui-node-values node)))))
    (cl-loop
     for entry in entries
     for value in (append values (make-list (max 0 (- (length entries) (length values))) :null))
     for source = (and (vectorp entry) (> (length entry) 1) (format "%s" (aref entry 0)))
     for name = (and source (aref entry 1))
     for hole = (lispm-comfyui--hole-p value)
     when (and subgraph (stringp name) (not (string-prefix-p "$$" name))
               (not (string-match-p "\\`[[:space:]]*[0-9]+[[:space:]]*:" name)))
     do
     (let* ((inside (lispm-comfyui-subgraph-graph subgraph))
            (merged (lispm-comfyui-graph-types top types))
            (inner (seq-find (lambda (inner) (equal (format "%s" (lispm-comfyui-node-id inner)) source))
                             (lispm-comfyui-graph-nodes inside)))
            (index (and inner (lispm-comfyui-input-index inner name)))
            (link (and index (lispm-comfyui-input-link inside inner index))))
       (cond
        ((null inner))
        ((and link (eql (lispm-comfyui-link-from link) lispm-comfyui-input-node))
         (unless hole
           (setf (alist-get (lispm-comfyui-slot-name
                             (nth (lispm-comfyui-link-from-slot link) (lispm-comfyui-subgraph-inputs subgraph)))
                            promoted nil nil #'equal)
                 value)))
        (link)
        ((seq-find (lambda (widget) (equal (plist-get widget :name) name))
                   (lispm-comfyui-node-widgets inner merged))
         (let* ((spec (lispm-comfyui--widget-spec inner merged name))
                (slot (lispm-comfyui-subgraph-add-slot top subgraph 'input name
                                                       (lispm-comfyui-socket-type spec)))
                (input-name (lispm-comfyui-slot-name (nth slot (lispm-comfyui-subgraph-inputs subgraph)))))
           (lispm-comfyui--add-link inside lispm-comfyui-input-node slot
                                    (lispm-comfyui-node-id inner)
                                    (lispm-comfyui-ensure-input inner merged name)
                                    (lispm-comfyui-socket-type spec))
           (unless hole (setf (alist-get input-name promoted nil nil #'equal) value)))))))
    (setf (alist-get 'properties (lispm-comfyui-node-extra node))
          (lispm-comfyui--without properties '(proxyWidgets)))
    (setf (lispm-comfyui-node-values node) (cons :promoted promoted))))

(defun lispm-comfyui-graph-convert-to-subgraph (graph types nodes &optional name)
  "Make NODES of GRAPH a subgraph, and put a node of it in their place.
An output outside feeding them becomes an input of the subgraph, and
an output of theirs feeding nodes outside one of its outputs.  TYPES
are the server's node types; NAME names the subgraph.  Return the
node."
  (let* ((top (lispm-comfyui-graph-top graph))
         (ids (mapcar #'lispm-comfyui-node-id nodes))
         (in (lambda (id) (member id ids)))
         (links (lispm-comfyui-graph-links graph))
         (internal (seq-filter (lambda (link) (and (funcall in (lispm-comfyui-link-from link))
                                                   (funcall in (lispm-comfyui-link-to link))))
                               links))
         (incoming (seq-filter (lambda (link) (and (not (funcall in (lispm-comfyui-link-from link)))
                                                   (funcall in (lispm-comfyui-link-to link))))
                               links))
         (outgoing (seq-filter (lambda (link) (and (funcall in (lispm-comfyui-link-from link))
                                                   (not (funcall in (lispm-comfyui-link-to link)))))
                               links))
         (inside (lispm-comfyui-graph-create :root top))
         (subgraph (lispm-comfyui-subgraph-create :id (lispm-comfyui-uuid) :name (or name "New Subgraph")
                                                  :graph inside))
         (input-keys nil) (output-keys nil)
         (x1 (apply #'min (mapcar #'lispm-comfyui-node-x nodes)))
         (y1 (apply #'min (mapcar #'lispm-comfyui-node-y nodes)))
         (x2 (apply #'max (mapcar (lambda (node) (+ (lispm-comfyui-node-x node)
                                                    (or (lispm-comfyui-node-width node) 240)))
                                  nodes))))
    (unless nodes (signal 'lispm-comfyui-error (list "No nodes to make a subgraph of")))
    (dolist (link incoming)
      (let ((key (cons (lispm-comfyui-link-from link) (lispm-comfyui-link-from-slot link))))
        (unless (assoc key input-keys)
          (let ((target (lispm-comfyui-node graph (lispm-comfyui-link-to link))))
            (push (cons key (lispm-comfyui-subgraph-add-slot
                             top subgraph 'input
                             (lispm-comfyui-slot-name (nth (lispm-comfyui-link-to-slot link)
                                                           (lispm-comfyui-node-inputs target)))
                             (lispm-comfyui-link-type link)))
                  input-keys)))))
    (dolist (link outgoing)
      (let ((key (cons (lispm-comfyui-link-from link) (lispm-comfyui-link-from-slot link))))
        (unless (assoc key output-keys)
          (let ((origin (lispm-comfyui-node graph (lispm-comfyui-link-from link))))
            (push (cons key (lispm-comfyui-subgraph-add-slot
                             top subgraph 'output
                             (lispm-comfyui-slot-name (nth (lispm-comfyui-link-from-slot link)
                                                           (lispm-comfyui-node-outputs origin)))
                             (lispm-comfyui-link-type link)))
                  output-keys)))))
    (setf (lispm-comfyui-subgraph-input-node subgraph)
          `((id . ,lispm-comfyui-input-node) (bounding . [,(- x1 260) ,y1 160 60]))
          (lispm-comfyui-subgraph-output-node subgraph)
          `((id . ,lispm-comfyui-output-node) (bounding . [,(+ x2 100) ,y1 160 60]))
          (lispm-comfyui-graph-nodes inside) nodes
          (lispm-comfyui-graph-links inside) internal)
    (dolist (link incoming)
      (lispm-comfyui--add-link inside lispm-comfyui-input-node
                               (cdr (assoc (cons (lispm-comfyui-link-from link) (lispm-comfyui-link-from-slot link))
                                           input-keys))
                               (lispm-comfyui-link-to link) (lispm-comfyui-link-to-slot link)
                               (lispm-comfyui-link-type link)))
    (pcase-dolist (`((,from . ,slot) . ,index) output-keys)
      (lispm-comfyui--add-link inside from slot lispm-comfyui-output-node index
                               (lispm-comfyui-link-type
                                (seq-find (lambda (link) (and (equal (lispm-comfyui-link-from link) from)
                                                              (equal (lispm-comfyui-link-from-slot link) slot)))
                                          outgoing))))
    (setf (lispm-comfyui-graph-subgraphs top)
          (append (lispm-comfyui-graph-subgraphs top) (list subgraph)))
    (let* ((merged (lispm-comfyui-graph-types top types))
           (node (lispm-comfyui-node-make graph (gethash (lispm-comfyui-subgraph-id subgraph) merged)
                                          (lispm-comfyui-subgraph-id subgraph) x1 y1)))
      (setf (lispm-comfyui-graph-nodes graph)
            (append (seq-remove (lambda (node) (memq node nodes)) (lispm-comfyui-graph-nodes graph))
                    (list node))
            (lispm-comfyui-graph-links graph)
            (seq-remove (lambda (link) (or (memq link internal) (memq link incoming) (memq link outgoing)))
                        links))
      (pcase-dolist (`((,from . ,slot) . ,index) input-keys)
        (lispm-comfyui--add-link graph from slot (lispm-comfyui-node-id node) index
                                 (lispm-comfyui-slot-type (nth index (lispm-comfyui-subgraph-inputs subgraph)))))
      (dolist (link outgoing)
        (lispm-comfyui--add-link graph (lispm-comfyui-node-id node)
                                 (cdr (assoc (cons (lispm-comfyui-link-from link) (lispm-comfyui-link-from-slot link))
                                             output-keys))
                                 (lispm-comfyui-link-to link) (lispm-comfyui-link-to-slot link)
                                 (lispm-comfyui-link-type link)))
      (lispm-comfyui-align-instances top types)
      node)))

(defun lispm-comfyui-graph-unpack (graph types node)
  "Put the nodes inside NODE, a subgraph's node of GRAPH, in its place.
Copies of them, numbered anew, linked as NODE was, the values NODE
held for widgets inside set on them.  TYPES are the server's node
types.  Return the copies."
  (let* ((top (lispm-comfyui-graph-top graph))
         (subgraph (or (lispm-comfyui-node-subgraph top node)
                       (signal 'lispm-comfyui-error (list "Not a subgraph's node"))))
         (merged (lispm-comfyui-graph-types top types))
         (inside (lispm-comfyui-subgraph-graph subgraph))
         (inner-nodes (lispm-comfyui-graph-nodes inside))
         (dx (- (lispm-comfyui-node-x node) (apply #'min 0 (mapcar #'lispm-comfyui-node-x inner-nodes))))
         (dy (- (lispm-comfyui-node-y node) (apply #'min 0 (mapcar #'lispm-comfyui-node-y inner-nodes))))
         (copies (make-hash-table))
         (promoted (and (eq (car-safe (lispm-comfyui-node-values node)) :promoted)
                        (cdr (lispm-comfyui-node-values node))))
         (id (lispm-comfyui-node-id node)))
    (when inner-nodes
      (setq dx (- (lispm-comfyui-node-x node) (apply #'min (mapcar #'lispm-comfyui-node-x inner-nodes)))
            dy (- (lispm-comfyui-node-y node) (apply #'min (mapcar #'lispm-comfyui-node-y inner-nodes)))))
    (dolist (inner inner-nodes)
      (let ((copy (lispm-comfyui-node-copy inner)))
        (setf (lispm-comfyui-node-id copy) (lispm-comfyui--new-node-id graph)
              (lispm-comfyui-node-x copy) (+ (lispm-comfyui-node-x inner) dx)
              (lispm-comfyui-node-y copy) (+ (lispm-comfyui-node-y inner) dy))
        (puthash (lispm-comfyui-node-id inner) copy copies)))
    (setf (lispm-comfyui-graph-nodes graph)
          (append (remq node (lispm-comfyui-graph-nodes graph))
                  (mapcar (lambda (inner) (gethash (lispm-comfyui-node-id inner) copies)) inner-nodes)))
    (let ((outer (lispm-comfyui-graph-links graph))
          (copy-id (lambda (inner-id) (lispm-comfyui-node-id (gethash inner-id copies)))))
      (setf (lispm-comfyui-graph-links graph)
            (seq-remove (lambda (link) (or (equal (lispm-comfyui-link-from link) id)
                                           (equal (lispm-comfyui-link-to link) id)))
                        outer))
      (dolist (link (lispm-comfyui-graph-links inside))
        (let* ((from (lispm-comfyui-link-from link))
               (to (lispm-comfyui-link-to link))
               (type (lispm-comfyui-link-type link))
               ;; Where an input of NODE came from outside.
               (source (lambda (index)
                         (seq-find (lambda (outer-link) (and (equal (lispm-comfyui-link-to outer-link) id)
                                                             (eql (lispm-comfyui-link-to-slot outer-link) index)))
                                   outer)))
               ;; Where an output of NODE went outside.
               (targets (lambda (index)
                          (seq-filter (lambda (outer-link) (and (equal (lispm-comfyui-link-from outer-link) id)
                                                                (eql (lispm-comfyui-link-from-slot outer-link) index)))
                                      outer))))
          (cond
           ((and (gethash from copies) (gethash to copies))
            (lispm-comfyui--add-link graph (funcall copy-id from) (lispm-comfyui-link-from-slot link)
                                     (funcall copy-id to) (lispm-comfyui-link-to-slot link) type))
           ((and (eql from lispm-comfyui-input-node) (gethash to copies))
            (let* ((index (lispm-comfyui-link-from-slot link))
                   (outer-link (funcall source index))
                   (copy (gethash to copies))
                   (slot (nth (lispm-comfyui-link-to-slot link) (lispm-comfyui-node-inputs copy))))
              (cond
               (outer-link
                (lispm-comfyui--add-link graph (lispm-comfyui-link-from outer-link)
                                         (lispm-comfyui-link-from-slot outer-link)
                                         (lispm-comfyui-node-id copy) (lispm-comfyui-link-to-slot link) type))
               ((and slot (lispm-comfyui-slot-widget slot))
                (let ((cell (assoc (lispm-comfyui-slot-name (nth index (lispm-comfyui-subgraph-inputs subgraph)))
                                   promoted)))
                  (when cell
                    (ignore-errors (lispm-comfyui-set-widget copy merged (lispm-comfyui-slot-widget slot)
                                                             (cdr cell)))))))))
           ((and (gethash from copies) (eql to lispm-comfyui-output-node))
            (dolist (outer-link (funcall targets (lispm-comfyui-link-to-slot link)))
              (lispm-comfyui--add-link graph (funcall copy-id from) (lispm-comfyui-link-from-slot link)
                                       (lispm-comfyui-link-to outer-link) (lispm-comfyui-link-to-slot outer-link)
                                       type)))
           ((and (eql from lispm-comfyui-input-node) (eql to lispm-comfyui-output-node))
            (when-let* ((outer-link (funcall source (lispm-comfyui-link-from-slot link))))
              (dolist (target (funcall targets (lispm-comfyui-link-to-slot link)))
                (lispm-comfyui--add-link graph (lispm-comfyui-link-from outer-link)
                                         (lispm-comfyui-link-from-slot outer-link)
                                         (lispm-comfyui-link-to target) (lispm-comfyui-link-to-slot target)
                                         type))))))))
    (mapcar (lambda (inner) (gethash (lispm-comfyui-node-id inner) copies)) inner-nodes)))

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
