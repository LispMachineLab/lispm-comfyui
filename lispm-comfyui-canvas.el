;;; lispm-comfyui-canvas.el --- A ComfyUI graph drawn  -*- lexical-binding: t; -*-

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

;; A graph drawn as one SVG, as ComfyUI's frontend draws it: each node a
;; box under its title, its inputs on the left and its outputs on the
;; right, a row each, then its widgets, then the image it made; links
;; curving from outputs to inputs, coloured by what they carry.
;;
;; `lispm-comfyui-canvas-layout' places what each node shows, its
;; items, on the canvas; `lispm-comfyui-canvas-svg' draws them, through
;; a view, a part of the canvas at a scale; `lispm-comfyui-canvas-hit'
;; finds the item at a point.  Nothing here knows a buffer or a
;; window: colours and fonts are given.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'xml)
(require 'url-util)
(require 'lispm-comfyui-graph)

(defconst lispm-comfyui-canvas-title-height 30
  "How tall a node's title is, on the canvas.")

(defconst lispm-comfyui-canvas-row-height 20
  "How tall a row of a node's inputs and outputs is, on the canvas.")

(defconst lispm-comfyui-canvas-widget-height 24
  "How tall a row of a node's widgets is, on the canvas.")

(defconst lispm-comfyui-canvas-text-size 13
  "How large text is, on the canvas.")

(defconst lispm-comfyui-canvas-text-lines 4
  "How many lines a widget of many lines of text shows, unless the node
was saved taller, which gives it the room left.")

(defconst lispm-comfyui-canvas-type-colours
  '(("MODEL" . "#B39DDB") ("CLIP" . "#FFD500") ("VAE" . "#FF6E6E")
    ("CONDITIONING" . "#FFA931") ("LATENT" . "#FF9CF9") ("IMAGE" . "#64B5F6")
    ("MASK" . "#81C784") ("CONTROL_NET" . "#6EE7B7") ("CLIP_VISION" . "#A8DADC")
    ("CLIP_VISION_OUTPUT" . "#AD7452") ("STYLE_MODEL" . "#C2FFAE") ("NOISE" . "#B0B0B0")
    ("GUIDER" . "#66FFFF") ("SAMPLER" . "#ECB4B4") ("SIGMAS" . "#CDFFCD")
    ("UPSCALE_MODEL" . "#C9A0DC") ("VIDEO" . "#F4A261") ("AUDIO" . "#E9C46A"))
  "The colours of links by the type they carry, as ComfyUI's frontend has them.")

(defun lispm-comfyui-canvas-type-colour (type palette)
  "The colour of a link or socket of TYPE, PALETTE giving the rest."
  (or (cdr (assoc (car (split-string (or type "*") ",")) lispm-comfyui-canvas-type-colours))
      (plist-get palette :link)))

(defconst lispm-comfyui-canvas-palette
  '(:background "#202020" :grid "#2c2c2c" :node "#353535" :title "#2a2a2a" :border "#4a4a4a"
    :text "#dddddd" :dim "#999999" :widget "#222222" :focus "#3b6fb6" :select "#ffffff"
    :run "#22c55e" :error "#ef4444" :bypass "#9c27b0" :link "#a0a0a0" :group-text "#cccccc"
    :font "sans-serif")
  "The colours and font a canvas is drawn with when none are given.")

(defun lispm-comfyui-canvas--escape (string)
  "STRING made safe in an XML attribute or text."
  (xml-escape-string (format "%s" string)))

(defun lispm-comfyui-canvas--width (string)
  "About how wide STRING is drawn, in units of the text's size."
  (* 0.56 (string-width string)))

(defun lispm-comfyui-canvas--fit (string width)
  "STRING cut to fit WIDTH on the canvas, ending in an ellipsis if cut."
  (let ((string (replace-regexp-in-string "[\n\t]+" " " (format "%s" string)))
        (size lispm-comfyui-canvas-text-size))
    (if (<= (* size (lispm-comfyui-canvas--width string)) width)
        string
      (let ((end (length string)))
        (while (and (> end 0)
                    (> (* size (lispm-comfyui-canvas--width (substring string 0 end))) (- width size)))
          (setq end (1- end)))
        (concat (substring string 0 end) "…")))))

(defun lispm-comfyui-canvas--wrap (string width lines)
  "STRING broken into at most LINES lines fitting WIDTH, the last cut."
  (let* ((size lispm-comfyui-canvas-text-size)
         (columns (max 4 (floor (/ width (* 0.56 size)))))
         (result nil))
    (dolist (paragraph (split-string (format "%s" string) "\n"))
      (if (string-empty-p paragraph)
          (push "" result)
        (while (not (string-empty-p paragraph))
          (let ((line (truncate-string-to-width paragraph columns)))
            (when (string-empty-p line) (setq line (substring paragraph 0 1)))
            (when (and (< (length line) (length paragraph))
                       (string-match ".*[ ,.;]" line)
                       (> (match-end 0) (/ (length line) 2)))
              (setq line (substring line 0 (match-end 0))))
            (push line result)
            (setq paragraph (string-trim-left (substring paragraph (length line)) " "))))))
    (setq result (nreverse result))
    (if (> (length result) lines)
        (append (seq-take result (1- lines))
                (list (let ((last (lispm-comfyui-canvas--fit (nth (1- lines) result) (- width size))))
                        (if (string-suffix-p "…" last) last (concat last "…")))))
      result)))

;;;; Where things are

(defun lispm-comfyui-canvas--value-text (widget value)
  "How the VALUE of WIDGET reads."
  (pcase (plist-get widget :kind)
    ('boolean (if (memq value '(nil :false :null)) "false" "true"))
    ('float (let ((round (lispm-comfyui-spec-option (plist-get widget :spec) 'round))
                  (step (lispm-comfyui-spec-option (plist-get widget :spec) 'step)))
              (if (numberp value)
                  (let* ((places (cond ((and (numberp round) (> round 0) (< round 1))
                                        (max 0 (ceiling (- (log round 10)))))
                                       ((and (numberp step) (> step 0) (< step 1))
                                        (max 0 (ceiling (- (log step 10)))))
                                       (t 2))))
                    (format (format "%%.%df" (min places 6)) value))
                (format "%s" value))))
    (_ (if (stringp value) value (format "%s" value)))))

(defun lispm-comfyui-canvas--multiline-p (widget)
  "Non-nil if WIDGET is a text of many lines."
  (and (eq (plist-get widget :kind) 'string)
       (lispm-comfyui-spec-option (plist-get widget :spec) 'multiline)))

(defun lispm-comfyui-canvas--node-layout (graph types node image)
  "Where what NODE of GRAPH, of TYPES, shows is, IMAGE the one it shows.
IMAGE is (FILE WIDTH HEIGHT) or nil.  Returns a plist: :node, :x, :y
\(the top left of its body), :w, :h (its body's size), :items, what can
be chosen in it, and :inputs and :outputs, where each slot's socket is,
alists from indexes to (X . Y)."
  (let* ((x (lispm-comfyui-node-x node))
         (y (lispm-comfyui-node-y node))
         (w (max 180 (or (lispm-comfyui-node-width node) 240)))
         (row lispm-comfyui-canvas-row-height)
         (widget-row lispm-comfyui-canvas-widget-height)
         (pad 6)
         (collapsed (let ((flags (alist-get 'flags (lispm-comfyui-node-extra node))))
                      (and (consp flags) (eq (alist-get 'collapsed flags) t))))
         (inputs (cl-loop for slot in (lispm-comfyui-node-inputs node)
                          for index from 0
                          when (or (not (lispm-comfyui-slot-widget slot))
                                   (lispm-comfyui-input-link graph node index))
                          collect (cons index slot)))
         (linked-widgets (cl-loop for (_ . slot) in inputs
                                  when (lispm-comfyui-slot-widget slot)
                                  collect (lispm-comfyui-slot-widget slot)))
         (outputs (cl-loop for slot in (lispm-comfyui-node-outputs node)
                           for index from 0 collect (cons index slot)))
         (widgets (seq-remove (lambda (widget) (member (plist-get widget :name) linked-widgets))
                              (lispm-comfyui-node-widgets node types)))
         (title-top (- y lispm-comfyui-canvas-title-height))
         (items (list (list :kind 'title :rect (list x title-top (+ x w) y))))
         (input-places nil) (output-places nil)
         (top (+ y pad)))
    (if collapsed
        (let* ((title (lispm-comfyui-node-name node types))
               (width (min w (+ 40 (* lispm-comfyui-canvas-text-size
                                      (lispm-comfyui-canvas--width title)))))
               (middle (- y (/ lispm-comfyui-canvas-title-height 2.0))))
          (setq w width)
          (setf (plist-get (car items) :rect) (list x title-top (+ x w) y))
          (dolist (input inputs) (push (cons (car input) (cons x middle)) input-places))
          (dolist (output outputs) (push (cons (car output) (cons (+ x w) middle)) output-places))
          (list :node node :x x :y y :w w :h 0 :collapsed t :items items
                :inputs input-places :outputs output-places))
      (dotimes (index (max (length inputs) (length outputs)))
        (let ((middle (+ top (/ row 2.0))))
          (when-let* ((input (nth index inputs)))
            (push (list :kind 'input :index (car input) :slot (cdr input)
                        :rect (list x top (+ x (/ w 2.0)) (+ top row)))
                  items)
            (push (cons (car input) (cons x middle)) input-places))
          (when-let* ((output (nth index outputs)))
            (push (list :kind 'output :index (car output) :slot (cdr output)
                        :rect (list (+ x (/ w 2.0)) top (+ x w) (+ top row)))
                  items)
            (push (cons (car output) (cons (+ x w) middle)) output-places))
          (setq top (+ top row))))
      (let* ((texts (seq-count #'lispm-comfyui-canvas--multiline-p widgets))
             (fixed (+ (- top y)
                       (* widget-row (+ (- (length widgets) texts)
                                        (seq-count (lambda (widget) (plist-get widget :control))
                                                   widgets)))
                       pad))
             ;; Texts of many lines share what room the node was saved
             ;; with, as in ComfyUI's frontend, or have a few lines each.
             (text-height (max (+ 22 (* 15 2))
                               (if (and (> texts 0) (lispm-comfyui-node-height node))
                                   (/ (- (lispm-comfyui-node-height node) fixed) (float texts))
                                 0)
                               (if (lispm-comfyui-node-height node) 0
                                 (+ 22 (* 15 lispm-comfyui-canvas-text-lines))))))
        (dolist (widget widgets)
          (let ((height (if (lispm-comfyui-canvas--multiline-p widget) text-height widget-row)))
            (push (list :kind 'widget :widget widget :name (plist-get widget :name)
                        :rect (list (+ x pad) (+ top 2) (- (+ x w) pad) (+ top height -2)))
                  items)
            (setq top (+ top height))
            (when (plist-get widget :control)
              (push (list :kind 'control :widget widget :name (plist-get widget :name)
                          :rect (list (+ x pad) (+ top 2) (- (+ x w) pad) (+ top widget-row -2)))
                    items)
              (setq top (+ top widget-row))))))
      (let ((picture nil))
        (when image
          (pcase-let* ((`(,file ,image-width ,image-height) image)
                       (width (- w (* 2 pad)))
                       (height (min (* 1.5 width)
                                    (* width (/ (float (max 1 image-height)) (max 1 image-width))))))
            (setq picture (list file (+ x pad) (+ top pad) width height)
                  top (+ top pad height))))
        (list :node node :x x :y y :w w :h (max row (+ (- top y) pad)) :items (nreverse items)
              :inputs input-places :outputs output-places :image picture)))))

(defun lispm-comfyui-canvas-layout (graph types &optional images extra)
  "Where what each node of GRAPH, of TYPES, shows is.
IMAGES is a function of a node returning the image it shows, (FILE
WIDTH HEIGHT), or nil.  EXTRA are nodes drawn besides GRAPH's, below
them, as a subgraph's inputs and outputs.  Returns a list of plists,
one a node, as `lispm-comfyui-canvas--node-layout' makes them."
  (mapcar (lambda (node)
            (lispm-comfyui-canvas--node-layout graph types node
                                               (and images (funcall images node))))
          (append extra (lispm-comfyui-graph-nodes graph))))

(defun lispm-comfyui-canvas-place (layout id)
  "The place in LAYOUT of the node numbered ID."
  (seq-find (lambda (place) (equal (lispm-comfyui-node-id (plist-get place :node)) id)) layout))

(defun lispm-comfyui-canvas-box (place)
  "The rectangle PLACE, a node's, covers, title included: (X1 Y1 X2 Y2)."
  (list (plist-get place :x) (- (plist-get place :y) lispm-comfyui-canvas-title-height)
        (+ (plist-get place :x) (plist-get place :w))
        (+ (plist-get place :y) (plist-get place :h))))

(defun lispm-comfyui-canvas-bounds (layout)
  "The rectangle all of LAYOUT covers, (X1 Y1 X2 Y2), or nil if empty."
  (when layout
    (let ((boxes (mapcar #'lispm-comfyui-canvas-box layout)))
      (list (apply #'min (mapcar #'car boxes)) (apply #'min (mapcar #'cadr boxes))
            (apply #'max (mapcar #'caddr boxes)) (apply #'max (mapcar #'cadddr boxes))))))

(defun lispm-comfyui-canvas-hit (layout x y)
  "What of LAYOUT is at X Y on the canvas: (ID . ITEM), or nil.
ITEM is the index of the item there among the node's, 0, its title,
where the node has no item there."
  (cl-loop for place in (reverse layout)
           for (x1 y1 x2 y2) = (lispm-comfyui-canvas-box place)
           when (and (<= x1 x x2) (<= y1 y y2))
           return (cons (lispm-comfyui-node-id (plist-get place :node))
                        (or (cl-loop for item in (plist-get place :items)
                                     for index from 0
                                     for (ix1 iy1 ix2 iy2) = (plist-get item :rect)
                                     when (and (<= ix1 x ix2) (<= iy1 y iy2))
                                     return index)
                            0))))

;;;; Drawing

(defun lispm-comfyui-canvas--text (x y string &rest attributes)
  "SVG text STRING at X Y, with ATTRIBUTES, alternating names and values."
  (format "<text x=\"%.1f\" y=\"%.1f\"%s>%s</text>" x y
          (lispm-comfyui-canvas--attributes attributes)
          (lispm-comfyui-canvas--escape string)))

(defun lispm-comfyui-canvas--attributes (attributes)
  "ATTRIBUTES, alternating names and values, as an SVG element's."
  (cl-loop for (name value) on attributes by #'cddr
           when value
           concat (format " %s=\"%s\"" (substring (symbol-name name) 1)
                          (lispm-comfyui-canvas--escape value))))

(defun lispm-comfyui-canvas--link (from to colour &optional width)
  "An SVG curve FROM (X . Y) TO (X . Y), in COLOUR, WIDTH wide."
  (let* ((x1 (car from)) (y1 (cdr from)) (x2 (car to)) (y2 (cdr to))
         (reach (max 40 (* 0.5 (abs (- x2 x1))))))
    (format "<path d=\"M%.1f %.1fC%.1f %.1f %.1f %.1f %.1f %.1f\" fill=\"none\" stroke=\"%s\" stroke-width=\"%s\" stroke-opacity=\"0.85\"/>"
            x1 y1 (+ x1 reach) y1 (- x2 reach) y2 x2 y2 colour (or width 3))))

(defun lispm-comfyui-canvas--group (group palette)
  "SVG of GROUP, as saved, PALETTE giving the colours."
  (let ((bounding (alist-get 'bounding group))
        (colour (let ((colour (alist-get 'color group)))
                  (if (and (stringp colour) (string-match-p "\\`#[0-9a-fA-F]\\{3,8\\}\\'" colour))
                      colour "#3f789e"))))
    (when (and (vectorp bounding) (= (length bounding) 4))
      (let ((x (aref bounding 0)) (y (aref bounding 1)) (w (aref bounding 2)) (h (aref bounding 3)))
        (concat
         (format "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" rx=\"6\" fill=\"%s\" fill-opacity=\"0.22\" stroke=\"%s\" stroke-opacity=\"0.6\"/>"
                 x y w h colour colour)
         (lispm-comfyui-canvas--text (+ x 10) (+ y 24) (or (alist-get 'title group) "")
                                     :fill (plist-get palette :group-text)
                                     :font-size 20 :font-weight "bold"))))))

(defun lispm-comfyui-canvas--item (item place focused state palette)
  "SVG of ITEM of the node at PLACE, FOCUSED or not.
STATE is a plist of what the canvas shows besides the graph, as
`lispm-comfyui-canvas-svg' takes it; PALETTE the colours."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (plist-get item :rect))
               (node (plist-get place :node))
               (graph (plist-get state :graph))
               (size lispm-comfyui-canvas-text-size)
               (middle (+ (/ (+ y1 y2) 2.0) (* 0.35 size)))
               (focus (when focused
                        (format "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" rx=\"4\" fill=\"%s\" fill-opacity=\"0.55\"/>"
                                x1 y1 (- x2 x1) (- y2 y1) (plist-get palette :focus)))))
    (pcase (plist-get item :kind)
      ('title (or focus ""))
      ('input
       (let* ((slot (plist-get item :slot))
              (linked (lispm-comfyui-input-link graph node (plist-get item :index)))
              (colour (lispm-comfyui-canvas-type-colour (lispm-comfyui-slot-type slot) palette))
              (cy (/ (+ y1 y2) 2.0)))
         (concat (or focus "")
                 (format "<circle cx=\"%.1f\" cy=\"%.1f\" r=\"5\" fill=\"%s\" stroke=\"%s\" stroke-width=\"2\"/>"
                         x1 cy (if linked colour (plist-get palette :node)) colour)
                 (lispm-comfyui-canvas--text
                  (+ x1 12) middle
                  (lispm-comfyui-canvas--fit (lispm-comfyui-slot-name slot) (- x2 x1 16))
                  :fill (plist-get palette :text) :font-size size))))
      ('output
       (let* ((slot (plist-get item :slot))
              (linked (lispm-comfyui-output-links graph node (plist-get item :index)))
              (colour (lispm-comfyui-canvas-type-colour (lispm-comfyui-slot-type slot) palette))
              (cy (/ (+ y1 y2) 2.0)))
         (concat (or focus "")
                 (format "<circle cx=\"%.1f\" cy=\"%.1f\" r=\"5\" fill=\"%s\" stroke=\"%s\" stroke-width=\"2\"/>"
                         x2 cy (if linked colour (plist-get palette :node)) colour)
                 (lispm-comfyui-canvas--text
                  (- x2 12) middle
                  (lispm-comfyui-canvas--fit (lispm-comfyui-slot-name slot) (- x2 x1 16))
                  :fill (plist-get palette :text) :font-size size :text-anchor "end"))))
      ((and kind (or 'widget 'control))
       (let* ((widget (plist-get item :widget))
              (values (lispm-comfyui-node-values node))
              (control (lispm-comfyui-widget-control widget values))
              (name (if (eq kind 'control) "control after generate" (plist-get widget :name)))
              (text (if (eq kind 'control) control
                      (lispm-comfyui-canvas--value-text
                       widget (lispm-comfyui-widget-value widget values))))
              (box (format "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" rx=\"%s\" fill=\"%s\" stroke=\"%s\"/>"
                           x1 y1 (- x2 x1) (- y2 y1)
                           (if (lispm-comfyui-canvas--multiline-p widget) 6 (/ (- y2 y1) 2.0))
                           (if focused (plist-get palette :focus) (plist-get palette :widget))
                           (if focused (plist-get palette :select) (plist-get palette :border)))))
         (if (and (eq kind 'widget) (lispm-comfyui-canvas--multiline-p widget))
             (concat box
                     (lispm-comfyui-canvas--text (+ x1 8) (+ y1 15) name
                                                 :fill (plist-get palette :dim) :font-size 11)
                     (cl-loop for line in (lispm-comfyui-canvas--wrap
                                           text (- x2 x1 16) (max 1 (floor (- y2 y1 22) 15)))
                              for top from (+ y1 31) by 15
                              concat (lispm-comfyui-canvas--text
                                      (+ x1 8) top line
                                      :fill (plist-get palette :text) :font-size size)))
           (let* ((width (- x2 x1 24))
                  (name (lispm-comfyui-canvas--fit name (* 0.5 width)))
                  (room (- width (* size (lispm-comfyui-canvas--width name)) 8)))
             (concat box
                     (lispm-comfyui-canvas--text (+ x1 12) middle name
                                                 :fill (plist-get palette :dim) :font-size size)
                     (lispm-comfyui-canvas--text
                      (- x2 12) middle
                      (concat (lispm-comfyui-canvas--fit text (max 30 (- room (if (memq (plist-get widget :kind) '(combo dynamic)) 14 0))))
                              (if (memq (plist-get widget :kind) '(combo dynamic)) " ▾" ""))
                      :fill (plist-get palette :text) :font-size size :text-anchor "end"))))))
      (_ ""))))

(defun lispm-comfyui-canvas--node (place types state palette)
  "SVG of the node at PLACE, of TYPES, with STATE, in PALETTE's colours."
  (let* ((node (plist-get place :node))
         (id (lispm-comfyui-node-id node))
         (x (plist-get place :x)) (y (plist-get place :y))
         (w (plist-get place :w)) (h (plist-get place :h))
         (title-height lispm-comfyui-canvas-title-height)
         (selected (equal id (plist-get state :selected)))
         (marked (member id (plist-get state :marked)))
         (subgraph (alist-get 'lispm-subgraph (gethash (lispm-comfyui-node-type node) types)))
         (io (and (numberp id) (< id 0)))
         (running (equal (format "%s" id) (plist-get state :executing)))
         (failure (cdr (assoc (format "%s" id) (plist-get state :errors))))
         (mode (lispm-comfyui-node-mode node))
         (note (member (lispm-comfyui-node-type node) '("Note" "MarkdownNote")))
         (border (cond (failure (plist-get palette :error))
                       (running (plist-get palette :run))
                       (selected (plist-get palette :select))
                       (marked (plist-get palette :focus))
                       (t (plist-get palette :border))))
         (body (cond ((eql mode 4) (plist-get palette :bypass))
                     (note "#3a3520")
                     (io (plist-get palette :title))
                     (t (plist-get palette :node))))
         (top (- y title-height)))
    (concat
     (format "<g%s>" (if (eql mode 2) " opacity=\"0.4\"" ""))
     (format "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" rx=\"8\" fill=\"%s\"%s/>"
             x top w (+ title-height h) body (if (eql mode 4) " fill-opacity=\"0.55\"" ""))
     (format "<path d=\"M%.1f %.1fv%.1fh%.1fv-%.1fa8 8 0 0 0 -8 -8h-%.1fa8 8 0 0 0 -8 8z\" fill=\"%s\"/>"
             x (+ top 8) (- title-height 8) w (- title-height 8) (- w 16)
             (plist-get palette :title))
     (format "<circle cx=\"%.1f\" cy=\"%.1f\" r=\"5\" fill=\"%s\"/>"
             (+ x 14) (- y (/ title-height 2.0))
             (cond (running (plist-get palette :run)) ((eql mode 4) (plist-get palette :bypass))
                   (t (plist-get palette :dim))))
     (lispm-comfyui-canvas--text
      (+ x 26) (- y (/ title-height 2.0) (* -0.35 lispm-comfyui-canvas-text-size))
      (lispm-comfyui-canvas--fit (concat (if subgraph "▸ " "")
                                         (lispm-comfyui-node-name node types)
                                         (pcase mode (4 "  (bypassed)") (2 "  (muted)") (_ ""))
                                         (if marked "  ✓" ""))
                                 (- w 36))
      :fill (plist-get palette :text) :font-size lispm-comfyui-canvas-text-size
      :font-weight "bold")
     (cl-loop for item in (plist-get place :items)
              for index from 0
              concat (lispm-comfyui-canvas--item
                      item place (and selected (eql index (plist-get state :focus)))
                      state palette))
     (if-let* ((image (plist-get place :image)))
         (pcase-let ((`(,file ,ix ,iy ,iw ,ih) image))
           (format "<image href=\"%s\" x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" preserveAspectRatio=\"xMidYMid meet\"/>"
                   (lispm-comfyui-canvas--escape (url-hexify-string (file-name-nondirectory file)))
                   ix iy iw ih))
       "")
     (if-let* (((and running (plist-get state :progress)))
               (progress (plist-get state :progress))
               ((> (cdr progress) 0)))
         (format "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"4\" fill=\"%s\"/>"
                 x (- (+ y h) 4) (* w (min 1.0 (/ (float (car progress)) (cdr progress))))
                 (plist-get palette :run))
       "")
     (format "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" rx=\"8\" fill=\"none\" stroke=\"%s\" stroke-width=\"%s\"/>"
             x top w (+ title-height h) border (if (or selected running failure marked) 2.5 1))
     (if failure
         (cl-loop for line in (lispm-comfyui-canvas--wrap failure w 3)
                  for top from (+ y h 18) by 16
                  concat (lispm-comfyui-canvas--text x top line :fill (plist-get palette :error)
                                                     :font-size 12))
       "")
     "</g>")))

(defun lispm-comfyui-canvas-svg (state width height &optional palette)
  "An SVG, WIDTH by HEIGHT pixels, of what STATE says, a string.
STATE is a plist: :graph and :types, the graph and its node types;
:layout, as `lispm-comfyui-canvas-layout' made it; :view, (X Y SCALE),
the canvas's point at the top left and how many pixels a unit of the
canvas is; :selected, the node chosen, and :focus, the index of its
item chosen; :executing, the number of the node running, a string;
:progress, (DONE . OF) for it; and :errors, an alist from node
numbers, strings, to what went wrong there; :marked, the numbers of the
nodes marked; :message, what to say over the canvas, as that the
server does not answer.  PALETTE is a plist of
colours and :font, `lispm-comfyui-canvas-palette' by default."
  (let* ((palette (or palette lispm-comfyui-canvas-palette))
         (graph (plist-get state :graph))
         (types (plist-get state :types))
         (layout (plist-get state :layout))
         (view (plist-get state :view))
         (scale (nth 2 view))
         (grid (* 40 scale)))
    (concat
     (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\">" width height)
     ;; A rule of its own: Emacs styles the svg element with its own
     ;; font, which CSS gives the text over an attribute.
     (format "<style>text{font-family:%s}</style>"
             (lispm-comfyui-canvas--escape (plist-get palette :font)))
     (format "<defs><pattern id=\"grid\" width=\"%.2f\" height=\"%.2f\" patternUnits=\"userSpaceOnUse\" x=\"%.2f\" y=\"%.2f\"><circle cx=\"1\" cy=\"1\" r=\"1\" fill=\"%s\"/></pattern></defs>"
             grid grid (- (mod (* (car view) scale) grid)) (- (mod (* (cadr view) scale) grid))
             (plist-get palette :grid))
     (format "<rect width=\"%d\" height=\"%d\" fill=\"%s\"/>" width height
             (plist-get palette :background))
     (if (> grid 6) (format "<rect width=\"%d\" height=\"%d\" fill=\"url(#grid)\"/>" width height) "")
     (format "<g transform=\"scale(%.4f) translate(%.2f %.2f)\">" scale (- (car view)) (- (cadr view)))
     (mapconcat (lambda (group) (or (lispm-comfyui-canvas--group group palette) ""))
                (lispm-comfyui-graph-groups graph) "")
     (mapconcat
      (lambda (link)
        (let ((from (lispm-comfyui-canvas-place layout (lispm-comfyui-link-from link)))
              (to (lispm-comfyui-canvas-place layout (lispm-comfyui-link-to link))))
          (if-let* ((start (and from (cdr (assq (lispm-comfyui-link-from-slot link)
                                                (plist-get from :outputs)))))
                    (end (and to (cdr (assq (lispm-comfyui-link-to-slot link)
                                            (plist-get to :inputs))))))
              (lispm-comfyui-canvas--link
               start end (lispm-comfyui-canvas-type-colour (lispm-comfyui-link-type link) palette))
            "")))
      (lispm-comfyui-graph-links graph) "")
     (mapconcat (lambda (place) (lispm-comfyui-canvas--node place types state palette))
                layout "")
     "</g>"
     (let ((message (or (plist-get state :message)
                        (unless (lispm-comfyui-graph-nodes graph)
                          "An empty workflow: a to add a node, o to open one"))))
       (if (null message) ""
         (lispm-comfyui-canvas--text (/ width 2.0)
                                     (if (lispm-comfyui-graph-nodes graph) 28 (/ height 2.0))
                                     message
                                     :fill (plist-get palette (if (plist-get state :message) :error :dim))
                                     :font-size 15 :text-anchor "middle")))
     "</svg>")))

(provide 'lispm-comfyui-canvas)
;;; lispm-comfyui-canvas.el ends here
