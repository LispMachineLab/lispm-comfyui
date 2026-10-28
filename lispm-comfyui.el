;;; lispm-comfyui.el --- ComfyUI's workflows, drawn, edited and run  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-comfyui
;; Version: 0.4.1
;; Package-Requires: ((emacs "30.1") (lispm-kit "0.1.0") (lispm-ui "0.2.1") (websocket "1.15"))
;; Keywords: multimedia, tools
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

;; ComfyUI (https://github.com/comfyanonymous/ComfyUI) generates images,
;; and video and sound, by workflows: graphs of nodes, a model loaded,
;; a prompt encoded, a sampler run, an image decoded and saved, each
;; node's outputs linked to the next ones' inputs.  M-x lispm-comfyui
;; draws a workflow as ComfyUI's own frontend does, from the server at
;; `lispm-comfyui-url', and edits and runs it from the keyboard: the
;; workflows of the server's library are opened and saved, nodes added,
;; linked, their values set, and the graph run, each node lit while it
;; runs and the images it makes shown in it.
;;
;; The arrow keys go from node to node, the way they point; n and p
;; from item to item of a node, its inputs, outputs and widgets; RET
;; does what the item chosen calls for: a value is set, an input linked
;; from an output, an output to an input.  Each key is also a menu
;; item: C-h m lists them.  The wheel zooms about the pointer; with
;; Shift, or turned sideways, it pans, as dragging the canvas does.
;;
;; With LispM Agent, its agents edit and run the workflow shown, with
;; the tools of lispm-comfyui-tools.el.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)
(require 'lispm-kit-commands)
(require 'lispm-ui-ask)
(require 'lispm-comfyui-graph)
(require 'lispm-comfyui-prompt)
(require 'lispm-comfyui-client)
(require 'lispm-comfyui-canvas)
(require 'lispm-comfyui-settings)

;; An Emacs built without images, as one running tests, has none.
(declare-function image-flush "image.c" (spec &optional frame))
(declare-function image-size "image.c" (spec &optional pixels frame))

(defcustom lispm-comfyui-dark t
  "Non-nil to draw workflows dark, as ComfyUI does; nil to follow Emacs's colours."
  :type 'boolean
  :group 'lispm-comfyui)

(defcustom lispm-comfyui-font "Helvetica Neue, Helvetica, Arial, sans-serif"
  "The fonts workflows are drawn with, as SVG names them, the first found used."
  :type 'string
  :group 'lispm-comfyui)

(defcustom lispm-comfyui-sidebar-width 28
  "The width of the workflows on the left, in columns."
  :type 'natnum
  :group 'lispm-comfyui)

(defvar-local lispm-comfyui--root nil
  "The workflow this buffer shows.")

(defvar-local lispm-comfyui--graph nil
  "The graph this buffer shows: the workflow, or the inside of a subgraph.")

(defvar-local lispm-comfyui--path nil
  "The numbers of the subgraphs' nodes entered, from the workflow in.")

(defvar-local lispm-comfyui--marked nil
  "The numbers of the nodes marked, to make a subgraph of.")

(defvar-local lispm-comfyui--tick 0
  "How many times the workflow changed, for what is made of it.")

(defvar-local lispm-comfyui--types-cache nil
  "The node types last made for the workflow: (SERVER ROOT TICK . TYPES).")

(defvar-local lispm-comfyui--name nil
  "The name the graph is kept under in the server's library, or nil.")

(defvar-local lispm-comfyui--modified nil
  "Non-nil if the graph changed since it was opened or saved.")

(defvar-local lispm-comfyui--selected nil
  "The number of the node chosen, or nil.")

(defvar-local lispm-comfyui--focus 0
  "The index of the item chosen among the chosen node's.")

(defvar-local lispm-comfyui--view (list 0 0 1.0)
  "The view: the canvas's point at the window's top left, and the scale.")

(defvar-local lispm-comfyui--undo nil
  "Workflows to go back to, the last first, each (WORKFLOW PATH SELECTED).")

(defvar-local lispm-comfyui--redo nil
  "Graphs undone, to go forward to again.")

(defvar-local lispm-comfyui--layout nil
  "Where each node's items were last drawn, see `lispm-comfyui-canvas-layout'.")

(defvar-local lispm-comfyui--images nil
  "The images each node made, an alist from the nodes' numbers in the
prompt, strings, 12:5 for one inside a subgraph, to files.")

(defvar-local lispm-comfyui--runs nil
  "This buffer's runs, the last first, each a plist: :id, the prompt's;
:files, the images it made; :pending, how many are still fetched;
:status, `queued', `running', `done', `failed' or `stopped';
:message; :start; :callbacks, called with the run once over.")

(defvar-local lispm-comfyui--executing nil
  "The number in the prompt of the node running, a string, or nil.")

(defvar-local lispm-comfyui--progress nil
  "How far the node running is, (DONE . OF), or nil.")

(defvar-local lispm-comfyui--preview nil
  "The preview of the image being made, (NODE . FILE), or nil.")

(defvar-local lispm-comfyui--errors nil
  "What went wrong at each node, an alist from its number in the prompt.")

(defvar-local lispm-comfyui--status nil
  "What is said of the server or the last run, or nil.")

(defvar-local lispm-comfyui--image nil
  "The image last drawn, let go when the next is.")

(defvar-local lispm-comfyui--timer nil
  "A timer drawing the graph again, or nil.")

(defvar lispm-comfyui--queue-remaining 0
  "How many prompts the server has yet to run.")

(defvar lispm-comfyui--sizes (make-hash-table :test #'equal)
  "The sizes of images, by file, (WIDTH . HEIGHT).")

(defvar lispm-comfyui--inputs-fetched (make-hash-table :test #'equal)
  "Input images asked of the server, by name: their files, or t.")

;;;; The node types

(defun lispm-comfyui--types ()
  "The node types of the server and of the workflow's subgraphs.
An empty table until the server's are read."
  (let ((server (or (lispm-comfyui-cached-types) (make-hash-table :test #'equal)))
        (root (or lispm-comfyui--root lispm-comfyui--graph)))
    (pcase lispm-comfyui--types-cache
      ((and `(,cached-server ,cached-root ,tick . ,types)
            (guard (and (eq cached-server server) (eq cached-root root)
                        (eql tick lispm-comfyui--tick))))
       types)
      (_ (let ((types (if root (lispm-comfyui-graph-types root server) server)))
           (setq lispm-comfyui--types-cache (cl-list* server root lispm-comfyui--tick types))
           types)))))

(defun lispm-comfyui--load-types (&optional again)
  "Read the server's node types, if not yet or AGAIN, and draw anew."
  (let ((buffer (current-buffer)))
    (setq lispm-comfyui--status "Reading the server's node types…")
    (lispm-comfyui-types-of-server
     (lambda (types error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq lispm-comfyui--status
                 (if types nil (format "Cannot reach %s: %s.  Press , for Settings"
                                       lispm-comfyui-url error)))
           (lispm-comfyui--draw-soon))))
     again)))

(defun lispm-comfyui--need-types ()
  "The server's node types, waited for if they are still being read."
  (or (lispm-comfyui-cached-types)
      (let ((done nil) (failure nil))
        (lispm-comfyui-types-of-server (lambda (types error) (setq done (or types t) failure error)))
        (with-timeout (60 (user-error "The server at %s did not answer.  Press , for Settings"
                                      lispm-comfyui-url))
          (while (not done) (accept-process-output nil 0.05)))
        (unless (lispm-comfyui-cached-types)
          (user-error "Cannot reach %s: %s.  Press , for Settings" lispm-comfyui-url failure))))
  (lispm-comfyui--types))

;;;; The workflow and its subgraphs

(defun lispm-comfyui--graph-at (root path)
  "The graph inside the subgraphs of the nodes PATH, in ROOT, or nil."
  (let ((graph root))
    (dolist (id path graph)
      (let* ((node (and graph (lispm-comfyui-node graph id)))
             (subgraph (and node (lispm-comfyui-node-subgraph root node))))
        (setq graph (and subgraph (lispm-comfyui-subgraph-graph subgraph)))))))

(defun lispm-comfyui--set-root (root &optional path)
  "Show ROOT, a workflow, inside the subgraphs of PATH if they are there."
  (let ((graph (lispm-comfyui--graph-at root path)))
    (setq lispm-comfyui--root root
          lispm-comfyui--path (and graph path)
          lispm-comfyui--graph (or graph root)
          lispm-comfyui--marked nil)
    (cl-incf lispm-comfyui--tick)))

(defun lispm-comfyui--subgraph-shown ()
  "The definition of the subgraph shown, or nil for the workflow."
  (when lispm-comfyui--path
    (let ((inside (lispm-comfyui--graph-at lispm-comfyui--root (butlast lispm-comfyui--path))))
      (lispm-comfyui-node-subgraph
       lispm-comfyui--root (lispm-comfyui-node inside (car (last lispm-comfyui--path)))))))

(defun lispm-comfyui--io-nodes ()
  "The nodes standing for the inputs and outputs of the subgraph shown."
  (when-let* ((subgraph (lispm-comfyui--subgraph-shown)))
    (list (lispm-comfyui-subgraph-io-node subgraph 'input)
          (lispm-comfyui-subgraph-io-node subgraph 'output))))

(defun lispm-comfyui--execution-key (id)
  "The number in the prompt of the node numbered ID shown, a string."
  (string-join (mapcar (lambda (part) (format "%s" part))
                       (append lispm-comfyui--path (list id)))
               ":"))

(defun lispm-comfyui--local-id (key)
  "The number, a string, of the node shown that KEY, a node's number in
the prompt, is or is inside; nil if neither."
  (let ((parts (split-string key ":"))
        (path (mapcar (lambda (id) (format "%s" id)) lispm-comfyui--path)))
    (when (and (> (length parts) (length path)) (equal (seq-take parts (length path)) path))
      (nth (length path) parts))))

;;;; Drawing

(defun lispm-comfyui--colour (colour)
  "COLOUR as (R G B), each 0 to 1, or nil."
  (ignore-errors (color-name-to-rgb colour)))

(defun lispm-comfyui--mix (from to amount)
  "The colour AMOUNT of the way from FROM to TO, as #RRGGBB."
  (let ((a (lispm-comfyui--colour from)) (b (lispm-comfyui--colour to)))
    (if (and a b)
        (apply #'color-rgb-to-hex (append (cl-mapcar (lambda (x y) (+ x (* amount (- y x)))) a b)
                                          '(2)))
      from)))

(defun lispm-comfyui--palette ()
  "The colours and font the graph is drawn with."
  (let* ((font lispm-comfyui-font)
         (background (face-background 'default nil t))
         (foreground (face-foreground 'default nil t)))
    (if (or lispm-comfyui-dark
            (not (lispm-comfyui--colour background)) (not (lispm-comfyui--colour foreground)))
        (plist-put (copy-sequence lispm-comfyui-canvas-palette) :font font)
      (list :background background
            :grid (lispm-comfyui--mix background foreground 0.15)
            :node (lispm-comfyui--mix background foreground 0.06)
            :title (lispm-comfyui--mix background foreground 0.14)
            :border (lispm-comfyui--mix background foreground 0.35)
            :text foreground
            :dim (lispm-comfyui--mix background foreground 0.55)
            :widget (lispm-comfyui--mix background foreground 0.02)
            :focus (or (face-background 'region nil t) "#3b6fb6")
            :select foreground :run "#16a34a" :error "#dc2626" :bypass "#9c27b0"
            :link (lispm-comfyui--mix background foreground 0.5) :group-text foreground
            :font font))))

(defun lispm-comfyui--image-size (file)
  "The size of the image FILE, (WIDTH . HEIGHT)."
  (or (gethash file lispm-comfyui--sizes)
      (puthash file
               (or (and (display-graphic-p)
                        (ignore-errors
                          (let ((size (image-size (create-image file nil nil :scale 1) t)))
                            (and (> (car size) 0) size))))
                   '(512 . 512))
               lispm-comfyui--sizes)))

(defun lispm-comfyui--input-image (node types)
  "The input image NODE, of TYPES, loads, fetched if need be, or nil.
For a node whose widget chooses an image of the server's input folder,
as LoadImage's, which ComfyUI shows in it."
  (when-let* ((widget (seq-find (lambda (widget)
                                  (lispm-comfyui-spec-option (plist-get widget :spec) 'image_upload))
                                (lispm-comfyui-node-widgets node types)))
              (value (lispm-comfyui-widget-value widget (lispm-comfyui-node-values node)))
              ((stringp value))
              ((not (string-empty-p value))))
    (let ((cached (gethash value lispm-comfyui--inputs-fetched)))
      (cond ((stringp cached) cached)
            (cached nil)
            (t (puthash value t lispm-comfyui--inputs-fetched)
               (let* ((type (if (string-match " \\[\\([a-z]+\\)\\]\\'" value)
                                (match-string 1 value) "input"))
                      (name (replace-regexp-in-string " \\[[a-z]+\\]\\'" "" value))
                      (buffer (current-buffer)))
                 (lispm-comfyui-fetch-image
                  `((filename . ,(file-name-nondirectory name))
                    (subfolder . ,(or (file-name-directory name) ""))
                    (type . ,type))
                  (lambda (file _error)
                    (when file
                      (puthash value file lispm-comfyui--inputs-fetched)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer (lispm-comfyui--draw-soon)))))))
               nil)))))

(defun lispm-comfyui--node-image (node types)
  "The image NODE, of TYPES, shows: (FILE WIDTH HEIGHT), or nil.
The preview of what it is making, the last image it made, or the
input image it loads."
  (let* ((key (lispm-comfyui--execution-key (lispm-comfyui-node-id node)))
         (inside (concat key ":"))
         (file (or (and lispm-comfyui--preview
                        (let ((of (format "%s" (car lispm-comfyui--preview))))
                          (or (equal of key) (string-prefix-p inside of)))
                        (cdr lispm-comfyui--preview))
                   (car (last (cdr (assoc key lispm-comfyui--images))))
                   ;; A subgraph's node shows the last image made inside.
                   (car (last (cdr (seq-find (lambda (entry) (string-prefix-p inside (car entry)))
                                             lispm-comfyui--images))))
                   (lispm-comfyui--input-image node types))))
    (when (and file (file-exists-p file))
      (let ((size (lispm-comfyui--image-size file)))
        (list file (car size) (cdr size))))))

(defun lispm-comfyui--relayout ()
  "Place each node's items anew, in `lispm-comfyui--layout'."
  (let ((types (lispm-comfyui--types)))
    (setq lispm-comfyui--layout
          (lispm-comfyui-canvas-layout lispm-comfyui--graph types
                                       (lambda (node) (lispm-comfyui--node-image node types))
                                       (lispm-comfyui--io-nodes)))))

(defun lispm-comfyui--window ()
  "The window showing this buffer, the selected one if it does."
  (if (eq (window-buffer) (current-buffer)) (selected-window)
    (get-buffer-window (current-buffer) t)))

(defun lispm-comfyui--draw ()
  "Draw the graph in this buffer, as large as its window."
  (when lispm-comfyui--timer
    (cancel-timer lispm-comfyui--timer)
    (setq lispm-comfyui--timer nil))
  (lispm-comfyui--relayout)
  (when-let* ((window (lispm-comfyui--window))
              ((display-images-p (window-frame window))))
    (let* ((width (max 50 (window-body-width window t)))
           (height (max 50 (1- (window-body-height window t))))
           (svg (lispm-comfyui-canvas-svg
                 (list :graph lispm-comfyui--graph :types (lispm-comfyui--types)
                       :layout lispm-comfyui--layout :view lispm-comfyui--view
                       :selected lispm-comfyui--selected :focus lispm-comfyui--focus
                       :marked lispm-comfyui--marked
                       :executing (and lispm-comfyui--executing
                                       (lispm-comfyui--local-id lispm-comfyui--executing))
                       :progress lispm-comfyui--progress
                       :message (unless (lispm-comfyui-cached-types)
                                  (if (and lispm-comfyui--status
                                           (string-prefix-p "Cannot reach" lispm-comfyui--status))
                                      (format "Cannot reach the ComfyUI server at %s.  Press , for Settings."
                                              lispm-comfyui-url)
                                    "Reading the server's node types…"))
                       :errors (cl-loop for (key . message) in lispm-comfyui--errors
                                        for local = (lispm-comfyui--local-id key)
                                        when local collect (cons local message)))
                 width height (lispm-comfyui--palette)))
           (image (create-image svg 'svg t :scale 1 :ascent 100
                                :base-uri (expand-file-name "canvas.svg" lispm-comfyui-directory)))
           (inhibit-read-only t))
      (make-directory lispm-comfyui-directory t)
      (erase-buffer)
      (insert (propertize " " 'display image 'pointer 'arrow))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (set-window-start window (point-min) t)
      ;; Each drawing is an image as large as the window; the last one
      ;; is let go at once, or the image cache would heap them up.
      (when lispm-comfyui--image
        (image-flush lispm-comfyui--image (window-frame window)))
      (setq lispm-comfyui--image image)))
  (force-mode-line-update))

(defun lispm-comfyui--draw-soon ()
  "Draw the graph again shortly, once for whatever comes meanwhile."
  (unless lispm-comfyui--timer
    (let ((buffer (current-buffer)))
      (setq lispm-comfyui--timer
            (run-with-timer 0.05 nil
                            (lambda ()
                              (when (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (setq lispm-comfyui--timer nil)
                                  (lispm-comfyui--draw)))))))))

(defun lispm-comfyui--window-changed (window)
  "Draw the graph anew for WINDOW, whose size or buffer changed."
  (with-current-buffer (window-buffer window)
    (when (derived-mode-p 'lispm-comfyui-mode)
      (with-selected-window window (lispm-comfyui--workflows-ensure))
      (lispm-comfyui--draw-soon))))

(defun lispm-comfyui--header ()
  "The header line: the workflow, and what the server or the last run says."
  (concat
   " " (propertize (or lispm-comfyui--name "Untitled") 'face 'bold)
   (if lispm-comfyui--modified " •" "")
   (let ((graph lispm-comfyui--root) (crumbs ""))
     (dolist (id lispm-comfyui--path crumbs)
       (let* ((node (and graph (lispm-comfyui-node graph id)))
              (subgraph (and node (lispm-comfyui-node-subgraph lispm-comfyui--root node))))
         (setq crumbs (concat crumbs " › " (if subgraph (lispm-comfyui-subgraph-name subgraph) "?"))
               graph (and subgraph (lispm-comfyui-subgraph-graph subgraph))))))
   (format "   %d nodes" (length (lispm-comfyui-graph-nodes lispm-comfyui--graph)))
   (if lispm-comfyui--marked (format ", %d marked" (length lispm-comfyui--marked)) "")
   (cond (lispm-comfyui--status (concat "   " lispm-comfyui--status))
         ((> lispm-comfyui--queue-remaining 0)
          (format "   %d in the server's queue" lispm-comfyui--queue-remaining))
         (t ""))))

;;;; The view

(defun lispm-comfyui--view-size ()
  "The size of the window on the canvas, (WIDTH . HEIGHT), in its units."
  (let ((window (lispm-comfyui--window)) (scale (nth 2 lispm-comfyui--view)))
    (if window
        (cons (/ (window-body-width window t) scale) (/ (window-body-height window t) scale))
      (cons 1200 800))))

(defun lispm-comfyui--canvas-point (position)
  "The point of the canvas at POSITION, pixels in the window, (X . Y)."
  (pcase-let ((`(,x ,y ,scale) lispm-comfyui--view))
    (cons (+ x (/ (car position) scale)) (+ y (/ (cdr position) scale)))))

(defun lispm-comfyui--set-scale (scale &optional around)
  "Make the view's scale SCALE, keeping AROUND (X . Y), on the canvas, still.
AROUND is the middle of the window by default."
  (pcase-let* ((`(,x ,y ,old) lispm-comfyui--view)
               (`(,width . ,height) (lispm-comfyui--view-size))
               (around (or around (cons (+ x (/ width 2)) (+ y (/ height 2)))))
               (scale (min 3.0 (max 0.15 scale)))
               (ratio (/ old scale)))
    (setq lispm-comfyui--view
          (list (- (car around) (* ratio (- (car around) x)))
                (- (cdr around) (* ratio (- (cdr around) y)))
                scale))))

(defun lispm-comfyui--fit ()
  "Make the view show the whole graph."
  (lispm-comfyui--relayout)
  (if-let* ((bounds (lispm-comfyui-canvas-bounds lispm-comfyui--layout))
            (window (lispm-comfyui--window)))
      (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) bounds)
                   (margin 40)
                   (width (window-body-width window t))
                   (height (window-body-height window t))
                   (scale (min 1.2 (/ width (float (+ (- x2 x1) margin margin)))
                               (/ height (float (+ (- y2 y1) margin margin))))))
        (setq scale (max 0.15 scale))
        (setq lispm-comfyui--view
              (list (- (/ (+ x1 x2) 2.0) (/ width scale 2.0))
                    (- (/ (+ y1 y2) 2.0) (/ height scale 2.0))
                    scale)))
    (setq lispm-comfyui--view (list -100 -100 1.0))))

(defun lispm-comfyui--show-selected ()
  "Move the view as little as shows the node chosen, if it can."
  (when-let* ((place (and lispm-comfyui--selected
                          (lispm-comfyui-canvas-place lispm-comfyui--layout lispm-comfyui--selected))))
    (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (lispm-comfyui-canvas-box place))
                 (`(,x ,y ,scale) lispm-comfyui--view)
                 (`(,width . ,height) (lispm-comfyui--view-size))
                 (margin 30))
      (when (or (> (- x2 x1) (- width margin margin)) (< x1 (+ x margin)) (> x2 (- (+ x width) margin)))
        (setq x (if (> (- x2 x1) (- width margin margin)) (- x1 margin)
                  (if (< x1 (+ x margin)) (- x1 margin) (- (+ x2 margin) width)))))
      (when (or (< y1 (+ y margin)) (> y2 (- (+ y height) margin)))
        (setq y (if (or (> (- y2 y1) (- height margin margin)) (< y1 (+ y margin))) (- y1 margin)
                  (- (+ y2 margin) height))))
      (setq lispm-comfyui--view (list x y scale)))))

;;;; Choosing

(defun lispm-comfyui--node ()
  "The node chosen, or nil: one of the graph's, or a subgraph's inputs or outputs."
  (and lispm-comfyui--selected
       (or (lispm-comfyui-node lispm-comfyui--graph lispm-comfyui--selected)
           (seq-find (lambda (node) (eql (lispm-comfyui-node-id node) lispm-comfyui--selected))
                     (lispm-comfyui--io-nodes)))))

(defun lispm-comfyui--io-p (node)
  "Non-nil if NODE stands for a subgraph's inputs or outputs."
  (and node (numberp (lispm-comfyui-node-id node)) (< (lispm-comfyui-node-id node) 0)))

(defun lispm-comfyui--need-node ()
  "The node chosen; a user error if none is."
  (or (lispm-comfyui--node) (user-error "No node is chosen")))

(defun lispm-comfyui--item ()
  "The item chosen of the node chosen, a plist, or nil."
  (when-let* ((place (and lispm-comfyui--selected
                          (lispm-comfyui-canvas-place lispm-comfyui--layout lispm-comfyui--selected))))
    (nth lispm-comfyui--focus (plist-get place :items))))

(defun lispm-comfyui--select (id &optional focus)
  "Choose the node numbered ID, and its item FOCUS, its title by default."
  (setq lispm-comfyui--selected id
        lispm-comfyui--focus (or focus 0))
  (lispm-comfyui--relayout)
  (lispm-comfyui--show-selected)
  (lispm-comfyui--draw))

(defun lispm-comfyui--centre (place)
  "The middle of the node at PLACE, (X . Y)."
  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (lispm-comfyui-canvas-box place)))
    (cons (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))))

(defun lispm-comfyui--towards (dx dy)
  "Choose the nearest node the way DX DY point from the node chosen."
  (let ((from (and lispm-comfyui--selected
                   (lispm-comfyui-canvas-place lispm-comfyui--layout lispm-comfyui--selected))))
    (if (null from)
        (lispm-comfyui--select-first)
      (let* ((origin (lispm-comfyui--centre from))
             (best nil) (best-score nil))
        (dolist (place lispm-comfyui--layout)
          (unless (eq place from)
            (pcase-let* ((`(,x . ,y) (lispm-comfyui--centre place))
                         (along (+ (* dx (- x (car origin))) (* dy (- y (cdr origin)))))
                         (across (abs (+ (* dy (- x (car origin))) (* (- dx) (- y (cdr origin)))))))
              (when (and (> along 0) (<= across (* 3 along)))
                (let ((score (+ along (* 2 across))))
                  (when (or (null best-score) (< score best-score))
                    (setq best place best-score score)))))))
        (if best
            (lispm-comfyui--select (lispm-comfyui-node-id (plist-get best :node)))
          (message "No node that way"))))))

(defun lispm-comfyui--reading-order ()
  "The nodes' places in reading order: by columns, left to right."
  (sort (copy-sequence lispm-comfyui--layout)
        (lambda (a b)
          (let ((ax (round (plist-get a :x) 100)) (bx (round (plist-get b :x) 100)))
            (if (= ax bx) (< (plist-get a :y) (plist-get b :y)) (< ax bx))))))

(defun lispm-comfyui--select-first ()
  "Choose the first node, in reading order."
  (if-let* ((first (car (lispm-comfyui--reading-order))))
      (lispm-comfyui--select (lispm-comfyui-node-id (plist-get first :node)))
    (message "The workflow has no nodes yet: a adds one")))

;;;; Changing the graph

(defun lispm-comfyui--remember ()
  "Keep the workflow as it is now, to undo to."
  (push (list (lispm-comfyui-graph-copy lispm-comfyui--root) lispm-comfyui--path lispm-comfyui--selected)
        lispm-comfyui--undo)
  (when (> (length lispm-comfyui--undo) 200)
    (setq lispm-comfyui--undo (seq-take lispm-comfyui--undo 200)))
  (setq lispm-comfyui--redo nil)
  (cl-incf lispm-comfyui--tick))

(defmacro lispm-comfyui--change (&rest body)
  "Run BODY, which changes the workflow, as one step to undo; draw anew.
If BODY fails, the workflow is as it was.  The subgraphs' nodes are
made to have their subgraphs' inputs after."
  (declare (indent 0) (debug t))
  (let ((before (make-symbol "before"))
        (selected (make-symbol "selected"))
        (path (make-symbol "path")))
    `(let ((,before (lispm-comfyui-graph-copy lispm-comfyui--root))
           (,selected lispm-comfyui--selected)
           (,path lispm-comfyui--path))
       (condition-case err
           (prog1 (progn ,@body)
             (lispm-comfyui-align-instances lispm-comfyui--root (lispm-comfyui-cached-types))
             (push (list ,before ,path ,selected) lispm-comfyui--undo)
             (setq lispm-comfyui--redo nil
                   lispm-comfyui--modified t)
             (cl-incf lispm-comfyui--tick)
             (lispm-comfyui--relayout)
             (lispm-comfyui--draw))
         (error (lispm-comfyui--set-root ,before ,path)
                (signal (car err) (cdr err)))))))

(defun lispm-comfyui--restore (step)
  "Show the workflow of STEP, (WORKFLOW PATH SELECTED), as it was."
  (pcase-let ((`(,root ,path ,selected) step))
    (lispm-comfyui--set-root root path)
    (setq lispm-comfyui--modified t)
    (lispm-comfyui--select (and (lispm-comfyui-node lispm-comfyui--graph selected) selected))))

(defun lispm-comfyui-undo ()
  "Undo the last change to the workflow."
  (interactive)
  (let ((step (or (pop lispm-comfyui--undo) (user-error "Nothing to undo"))))
    (push (list lispm-comfyui--root lispm-comfyui--path lispm-comfyui--selected) lispm-comfyui--redo)
    (lispm-comfyui--restore step)))

(defun lispm-comfyui-redo ()
  "Do again the change last undone."
  (interactive)
  (let ((step (or (pop lispm-comfyui--redo) (user-error "Nothing to redo"))))
    (push (list lispm-comfyui--root lispm-comfyui--path lispm-comfyui--selected) lispm-comfyui--undo)
    (lispm-comfyui--restore step)))

(defun lispm-comfyui--type-candidates (types &optional predicate)
  "Completion candidates for the node types of TYPES PREDICATE accepts.
An alist from what is shown, the type's title and name, to the name."
  (let (candidates)
    (maphash (lambda (name definition)
               (when (or (null predicate) (funcall predicate definition))
                 (let ((title (lispm-comfyui-type-title definition)))
                   (push (cons (if (equal title name) name (format "%s  (%s)" title name)) name)
                         candidates))))
             types)
    (sort candidates (lambda (a b) (string< (car a) (car b))))))

(defun lispm-comfyui--read-type (types prompt &optional predicate)
  "Read the name of a node type of TYPES PREDICATE accepts, asking PROMPT."
  (let* ((candidates (lispm-comfyui--type-candidates types predicate))
         (completion-extra-properties
          (list :annotation-function
                (lambda (candidate)
                  (when-let* ((definition (gethash (cdr (assoc candidate candidates)) types))
                              (category (lispm-comfyui--field definition 'category)))
                    (concat "  " (propertize category 'face 'completions-annotations))))))
         (choice (completing-read prompt candidates nil t)))
    (or (cdr (assoc choice candidates)) (user-error "No such node type"))))

(defun lispm-comfyui--accepts (definition type)
  "Non-nil if a node of DEFINITION has an input taking TYPE."
  (seq-some (pcase-lambda (`(,_ ,spec ,_))
              (lispm-comfyui-types-match-p type (lispm-comfyui-socket-type spec)))
            (lispm-comfyui-type-inputs definition)))

(defun lispm-comfyui--gives (definition type)
  "Non-nil if a node of DEFINITION has an output of TYPE."
  (seq-some (pcase-lambda (`(,_ ,output)) (lispm-comfyui-types-match-p output type))
            (lispm-comfyui-type-outputs definition)))

(defun lispm-comfyui-add-node ()
  "Add a node of a type asked for.
On an output chosen, the node is one taking what it gives, linked to
it; on an input, one giving what it takes, linked from it."
  (interactive)
  (let* ((types (lispm-comfyui--need-types))
         (item (lispm-comfyui--item))
         (from (lispm-comfyui--node))
         (slot (plist-get item :slot))
         (kind (and slot (plist-get item :kind)))
         (type (and slot (if (eq kind 'input)
                             (lispm-comfyui-input-type from types (plist-get item :index))
                           (lispm-comfyui-slot-type slot))))
         (around (lispm-comfyui--ancestors))
         (name (lispm-comfyui--read-type
                types
                (pcase kind
                  ('output (format "Add a node taking %s: " type))
                  ('input (format "Add a node giving %s: " type))
                  (_ "Add a node: "))
                ;; Not a node of a subgraph it would be inside.
                (lambda (definition)
                  (and (not (member (alist-get 'name definition) around))
                       (pcase kind
                         ('output (lispm-comfyui--accepts definition type))
                         ('input (lispm-comfyui--gives definition type))
                         (_ t)))))))
    (lispm-comfyui--change
      (let ((node (lispm-comfyui-graph-add-node lispm-comfyui--graph types name)))
        (pcase kind
          ('output
           (let ((input (car (seq-find (pcase-lambda (`(,_ ,spec ,_))
                                         (lispm-comfyui-types-match-p
                                          type (lispm-comfyui-socket-type spec)))
                                       (lispm-comfyui-type-inputs (gethash name types))))))
             (lispm-comfyui-graph-connect lispm-comfyui--graph types from (plist-get item :index)
                                    node input)))
          ('input
           (let ((output (seq-position (lispm-comfyui-node-outputs node) type
                                       (lambda (slot type)
                                         (lispm-comfyui-types-match-p
                                          (lispm-comfyui-slot-type slot) type)))))
             (lispm-comfyui-graph-connect lispm-comfyui--graph types node output from
                                    (lispm-comfyui-slot-name slot)))))
        (if (eq kind 'input)
            (progn (setf (lispm-comfyui-node-x node) (- (lispm-comfyui-node-x from) 300)
                         (lispm-comfyui-node-y node) (lispm-comfyui-node-y from))
                   (lispm-comfyui--clear-place node))
          (lispm-comfyui-place lispm-comfyui--graph node from)
          (lispm-comfyui--clear-place node))
        (setq lispm-comfyui--selected (lispm-comfyui-node-id node)
              lispm-comfyui--focus 0)))
    (lispm-comfyui--select lispm-comfyui--selected)))

(defun lispm-comfyui--ancestors ()
  "The types of the subgraphs' nodes entered: those the graph shown is inside."
  (let ((graph lispm-comfyui--root) (types nil))
    (dolist (id lispm-comfyui--path (nreverse types))
      (let ((node (and graph (lispm-comfyui-node graph id))))
        (when node
          (push (lispm-comfyui-node-type node) types)
          (setq graph (when-let* ((subgraph (lispm-comfyui-node-subgraph lispm-comfyui--root node)))
                        (lispm-comfyui-subgraph-graph subgraph))))))))

(defun lispm-comfyui--clear-place (node)
  "Move NODE down until it overlaps no other node, as each is drawn."
  (cl-loop repeat 200
           do (lispm-comfyui--relayout)
           for box = (lispm-comfyui-canvas-box
                      (lispm-comfyui-canvas-place lispm-comfyui--layout (lispm-comfyui-node-id node)))
           while (seq-find (lambda (place)
                             (and (not (eq (plist-get place :node) node))
                                  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (lispm-comfyui-canvas-box place)))
                                    (and (< (nth 0 box) (+ x2 20)) (< x1 (+ (nth 2 box) 20))
                                         (< (nth 1 box) (+ y2 30)) (< y1 (+ (nth 3 box) 30))))))
                           lispm-comfyui--layout)
           do (cl-incf (lispm-comfyui-node-y node) 40)))

(cl-defun lispm-comfyui-delete-node ()
  "Delete the node chosen, and its links.
On an input or output of the subgraph shown, that input or output goes."
  (interactive)
  (when (lispm-comfyui--io-p (lispm-comfyui--need-node))
    (cl-return-from lispm-comfyui-delete-node (lispm-comfyui--remove-io-slot)))
  (let* ((node (lispm-comfyui--need-node))
         (place (lispm-comfyui-canvas-place lispm-comfyui--layout (lispm-comfyui-node-id node)))
         (origin (lispm-comfyui--centre place))
         (next (car (sort (seq-remove (lambda (other) (eq other place)) (copy-sequence lispm-comfyui--layout))
                          (lambda (a b)
                            (let ((ca (lispm-comfyui--centre a)) (cb (lispm-comfyui--centre b)))
                              (< (+ (abs (- (car ca) (car origin))) (abs (- (cdr ca) (cdr origin))))
                                 (+ (abs (- (car cb) (car origin))) (abs (- (cdr cb) (cdr origin)))))))))))
    (lispm-comfyui--change
      (lispm-comfyui-remove-node lispm-comfyui--graph node)
      (setq lispm-comfyui--selected (and next (lispm-comfyui-node-id (plist-get next :node)))
            lispm-comfyui--focus 0))))

(defun lispm-comfyui-duplicate-node ()
  "Add a copy of the node chosen, without its links, beside it."
  (interactive)
  (let* ((node (lispm-comfyui--need-node))
         (types (lispm-comfyui--need-types)))
    (when (lispm-comfyui--io-p node) (user-error "A subgraph's inputs and outputs are not copied"))
    (lispm-comfyui--change
      (let ((copy (lispm-comfyui-graph-add-node lispm-comfyui--graph types (lispm-comfyui-node-type node))))
        (setf (lispm-comfyui-node-values copy) (copy-tree (lispm-comfyui-node-values node) t)
              (lispm-comfyui-node-title copy) (lispm-comfyui-node-title node)
              (lispm-comfyui-node-width copy) (lispm-comfyui-node-width node)
              (lispm-comfyui-node-x copy) (+ (lispm-comfyui-node-x node) 30)
              (lispm-comfyui-node-y copy) (+ (lispm-comfyui-node-y node) 30))
        (lispm-comfyui--clear-place copy)
        (setq lispm-comfyui--selected (lispm-comfyui-node-id copy))))
    (lispm-comfyui--select lispm-comfyui--selected)))

(defun lispm-comfyui--set (node name value)
  "Set NODE's widget NAME to VALUE, as one change."
  (lispm-comfyui--change
    (lispm-comfyui-set-widget node (lispm-comfyui--types) name value)))

(defvar-local lispm-comfyui--text-target nil
  "Where the text of this buffer goes: (BUFFER NODE-ID WIDGET).")

(define-minor-mode lispm-comfyui-text-mode
  "Editing the text of a widget of a ComfyUI workflow.
\\<lispm-comfyui-text-mode-map>\\[lispm-comfyui-text-done] sets it, \\[lispm-comfyui-text-cancel] leaves it as it was."
  :keymap (define-keymap
            "C-c C-c" #'lispm-comfyui-text-done
            "C-c C-k" #'lispm-comfyui-text-cancel)
  (setq-local header-line-format
              (substitute-command-keys
               " \\<lispm-comfyui-text-mode-map>\\[lispm-comfyui-text-done] sets the text, \\[lispm-comfyui-text-cancel] leaves it")))

(defun lispm-comfyui-text-done ()
  "Set the widget to this buffer's text."
  (interactive)
  (pcase-let ((`(,buffer ,id ,name) lispm-comfyui--text-target)
              (text (buffer-substring-no-properties (point-min) (point-max))))
    (quit-window t)
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer)
      (with-current-buffer buffer
        (when-let* ((node (lispm-comfyui-node lispm-comfyui--graph id)))
          (lispm-comfyui--set node name text))))))

(defun lispm-comfyui-text-cancel ()
  "Leave the widget's text as it was."
  (interactive)
  (let ((buffer (car lispm-comfyui--text-target)))
    (quit-window t)
    (when (buffer-live-p buffer) (pop-to-buffer buffer))))

(defun lispm-comfyui--edit-text (node name value)
  "Edit VALUE, the text of NODE's widget NAME, in a buffer of its own."
  (let ((buffer (current-buffer))
        (edit (generate-new-buffer (format "*ComfyUI: %s %s*" (lispm-comfyui-node-name node) name))))
    (with-current-buffer edit
      (text-mode)
      (insert value)
      (goto-char (point-min))
      (setq-local lispm-comfyui--text-target (list buffer (lispm-comfyui-node-id node) name))
      (lispm-comfyui-text-mode))
    (pop-to-buffer edit '((display-buffer-below-selected) (window-height . 12)))))

(defconst lispm-comfyui--upload-extensions '("png" "jpg" "jpeg" "webp")
  "The ends of the names of the pictures an image is uploaded from: PNG,
JPEG and WebP, as ComfyUI's frontend takes them.")

(defun lispm-comfyui--image-file-p (file)
  "Non-nil if FILE is a picture an image is uploaded from, by its name."
  (member (downcase (or (file-name-extension file) "")) lispm-comfyui--upload-extensions))

(defun lispm-comfyui--upload (node name)
  "Upload an image chosen in LispM UI's file dialog, which shows the
pictures ComfyUI takes, and make it the image NODE's widget NAME loads.
Nothing is uploaded when none is chosen."
  (when-let* ((file (lispm-ui-choose-file :title "Upload an Image"
                                          :predicate #'lispm-comfyui--image-file-p
                                          :preview t))
              (buffer (current-buffer))
              (id (lispm-comfyui-node-id node)))
    (setq lispm-comfyui--status (format "Uploading %s…" (file-name-nondirectory file)))
    (lispm-comfyui--draw-soon)
    (lispm-comfyui-upload-image
     file
     (lambda (answer error)
       (with-current-buffer buffer
         (if error
             (progn (setq lispm-comfyui--status nil) (message "Cannot upload: %s" error))
           (let ((uploaded (concat (let ((folder (alist-get 'subfolder answer)))
                                     (if (and (stringp folder) (not (string-empty-p folder)))
                                         (concat folder "/") ""))
                                   (alist-get 'name answer))))
             (lispm-comfyui-types-of-server
              (lambda (_types _error)
                (with-current-buffer buffer
                  (setq lispm-comfyui--status nil)
                  (when-let* ((node (lispm-comfyui-node lispm-comfyui--graph id)))
                    (condition-case err (lispm-comfyui--set node name uploaded)
                      (lispm-comfyui-error (message "%s" (cadr err)))))))
              t))))))))

(defun lispm-comfyui--edit-widget (node widget &optional control)
  "Ask for a new value of NODE's WIDGET, or of its CONTROL, and set it."
  (let* ((name (plist-get widget :name))
         (spec (plist-get widget :spec))
         (values (lispm-comfyui-node-values node))
         (value (lispm-comfyui-widget-value widget values)))
    (if control
        (lispm-comfyui--set node (concat name "/control")
                            (completing-read (format "%s after each run: " name)
                                             lispm-comfyui-controls nil t nil nil
                                             (lispm-comfyui-widget-control widget values)))
      (pcase (plist-get widget :kind)
        ('boolean (lispm-comfyui--set node name (if (memq value '(nil :false)) t :false)))
        ((or 'int 'float)
         (let ((min (lispm-comfyui-spec-option spec 'min))
               (max (lispm-comfyui-spec-option spec 'max)))
           (lispm-comfyui--set
            node name
            (read-number (format "%s%s: " name
                                 (if (or (numberp min) (numberp max))
                                     (format " (%s to %s)" (or min "") (or max "")) ""))
                         value))))
        ('string
         (if (lispm-comfyui-spec-option spec 'multiline)
             (lispm-comfyui--edit-text node name value)
           (lispm-comfyui--set node name (read-string (format "%s: " name) value))))
        (_
         (let* ((upload (lispm-comfyui-spec-option spec 'image_upload))
                (choices (lispm-comfyui-widget-choices spec))
                (choice (completing-read (format "%s: " name)
                                         (if upload (cons "Upload a file…" choices) choices)
                                         nil t nil nil value)))
           (if (and upload (equal choice "Upload a file…"))
               (lispm-comfyui--upload node name)
             (lispm-comfyui--set node name choice))))))))

(defun lispm-comfyui-edit-value ()
  "Set the value of a widget of the node chosen, asked for by name."
  (interactive)
  (let* ((node (lispm-comfyui--need-node))
         (widgets (lispm-comfyui-node-widgets node (lispm-comfyui--need-types)))
         (item (lispm-comfyui--item))
         (name (if (memq (plist-get item :kind) '(widget control))
                   (plist-get item :name)
                 (completing-read "Set: " (mapcar (lambda (widget) (plist-get widget :name)) widgets)
                                  nil t)))
         (widget (seq-find (lambda (widget) (equal (plist-get widget :name) name)) widgets)))
    (unless widget (user-error "The node has no widget %s" name))
    (lispm-comfyui--edit-widget node widget (eq (plist-get item :kind) 'control))))

(defun lispm-comfyui--sources (types type exclude)
  "Candidates for linking into an input of TYPE: (TEXT NODE OUTPUT).
Every output of TYPE of a node but EXCLUDE; inside a subgraph, its
inputs too, and a new one, NODE then being :new."
  (append
   (cl-loop for node in (append (lispm-comfyui--io-nodes) (lispm-comfyui-graph-nodes lispm-comfyui--graph))
           unless (eq node exclude)
           append (cl-loop for slot in (lispm-comfyui-node-outputs node)
                           for index from 0
                           when (lispm-comfyui-types-match-p (lispm-comfyui-slot-type slot) type)
                           collect (list (if (lispm-comfyui--io-p node)
                                             (format "Subgraph input › %s" (lispm-comfyui-slot-name slot))
                                           (format "#%s %s › %s" (lispm-comfyui-node-id node)
                                                   (lispm-comfyui-node-name node types)
                                                   (lispm-comfyui-slot-name slot)))
                                         node index)))
   (when (and lispm-comfyui--path (not (lispm-comfyui--io-p exclude)))
     (list (list "New input of the subgraph" :new nil)))))

(defun lispm-comfyui--targets (types type exclude)
  "Candidates for linking an output of TYPE to: (TEXT NODE INPUT).
Every input taking TYPE, a widget's too, of a node but EXCLUDE; inside a
subgraph, its outputs too, and a new one, NODE then being :new."
  (append
   (cl-loop for node in (append (lispm-comfyui--io-nodes) (lispm-comfyui-graph-nodes lispm-comfyui--graph))
           unless (eq node exclude)
           append
           (let ((names nil))
             (append
              (cl-loop for slot in (lispm-comfyui-node-inputs node)
                       for index from 0
                       do (push (lispm-comfyui-slot-name slot) names)
                       when (lispm-comfyui-types-match-p type (lispm-comfyui-input-type node types index))
                       collect (list (format "%s › %s%s"
                                             (if (lispm-comfyui--io-p node) "Subgraph output"
                                               (format "#%s %s" (lispm-comfyui-node-id node)
                                                       (lispm-comfyui-node-name node types)))
                                             (lispm-comfyui-slot-name slot)
                                             (if (lispm-comfyui-input-link lispm-comfyui--graph node index)
                                                 "  (linked already)" ""))
                                     node (lispm-comfyui-slot-name slot)))
              (cl-loop for widget in (lispm-comfyui-node-widgets node types)
                       for spec = (plist-get widget :spec)
                       unless (member (plist-get widget :name) names)
                       when (and (member (lispm-comfyui-socket-type spec) '("INT" "FLOAT" "STRING" "BOOLEAN"))
                                 (lispm-comfyui-types-match-p type (lispm-comfyui-socket-type spec)))
                       collect (list (format "#%s %s › %s" (lispm-comfyui-node-id node)
                                             (lispm-comfyui-node-name node types)
                                             (plist-get widget :name))
                                     node (plist-get widget :name))))))
   (when (and lispm-comfyui--path (not (lispm-comfyui--io-p exclude)))
     (list (list "New output of the subgraph" :new nil)))))

(defun lispm-comfyui-connect ()
  "Link the item chosen: an input from an output, an output to an input.
On a widget, an output is linked to feed it instead of its value."
  (interactive)
  (let* ((types (lispm-comfyui--need-types))
         (node (lispm-comfyui--need-node))
         (item (lispm-comfyui--item)))
    (pcase (plist-get item :kind)
      ((and kind (or 'input 'widget))
       (let* ((index (plist-get item :index))
              (name (if (eq kind 'input) (lispm-comfyui-slot-name (plist-get item :slot))
                      (plist-get item :name)))
              (type (if (eq kind 'input) (lispm-comfyui-input-type node types index)
                      (lispm-comfyui-socket-type (plist-get (plist-get item :widget) :spec))))
              (candidates (or (lispm-comfyui--sources types type node)
                              (user-error "No output gives %s; a adds a node that does" type)))
              (choice (assoc (completing-read (format "Link %s from: " name) candidates nil t)
                             candidates)))
         (lispm-comfyui--change
           (if (eq (nth 1 choice) :new)
               (let* ((subgraph (lispm-comfyui--subgraph-shown))
                      (index (lispm-comfyui-subgraph-add-slot lispm-comfyui--root subgraph 'input name type)))
                 (lispm-comfyui-graph-connect lispm-comfyui--graph types
                                              (lispm-comfyui-subgraph-io-node subgraph 'input) index node name))
             (lispm-comfyui-graph-connect lispm-comfyui--graph types (nth 1 choice) (nth 2 choice) node name)))))
      ('output
       (let* ((type (lispm-comfyui-slot-type (plist-get item :slot)))
              (candidates (or (lispm-comfyui--targets types type node)
                              (user-error "No input takes %s; a adds a node that does" type)))
              (choice (assoc (completing-read (format "Link %s to: "
                                                      (lispm-comfyui-slot-name (plist-get item :slot)))
                                              candidates nil t)
                             candidates)))
         (lispm-comfyui--change
           (if (eq (nth 1 choice) :new)
               (let* ((subgraph (lispm-comfyui--subgraph-shown))
                      (index (lispm-comfyui-subgraph-add-slot
                              lispm-comfyui--root subgraph 'output
                              (lispm-comfyui-slot-name (plist-get item :slot)) type))
                      (io (lispm-comfyui-subgraph-io-node subgraph 'output)))
                 (lispm-comfyui-graph-connect lispm-comfyui--graph types node (plist-get item :index)
                                              io (lispm-comfyui-slot-name (nth index (lispm-comfyui-node-inputs io)))))
             (lispm-comfyui-graph-connect lispm-comfyui--graph types node (plist-get item :index)
                                          (nth 1 choice) (nth 2 choice))))))
      (_ (user-error "Choose an input, an output or a widget to link (n and p)")))))

(defun lispm-comfyui-disconnect ()
  "Remove the links of the item chosen: into an input, or out of an output."
  (interactive)
  (let* ((node (lispm-comfyui--need-node))
         (item (lispm-comfyui--item)))
    (pcase (plist-get item :kind)
      ('input
       (unless (lispm-comfyui-input-link lispm-comfyui--graph node (plist-get item :index))
         (user-error "The input is not linked"))
       (lispm-comfyui--change
         (lispm-comfyui-graph-disconnect lispm-comfyui--graph node (plist-get item :index))))
      ('output
       (let ((links (lispm-comfyui-output-links lispm-comfyui--graph node (plist-get item :index))))
         (unless links (user-error "The output is not linked"))
         (lispm-comfyui--change
           (dolist (link links)
             (let ((to (lispm-comfyui-node lispm-comfyui--graph (lispm-comfyui-link-to link))))
               (lispm-comfyui-graph-disconnect lispm-comfyui--graph to (lispm-comfyui-link-to-slot link)))))))
      (_ (user-error "Choose an input or an output to unlink (n and p)")))))

(defun lispm-comfyui--remove-io-slot ()
  "Take from the subgraph shown the input or output chosen on its node."
  (let* ((item (lispm-comfyui--item))
         (subgraph (lispm-comfyui--subgraph-shown)))
    (pcase (plist-get item :kind)
      ('output (lispm-comfyui--change
                 (lispm-comfyui-subgraph-remove-slot lispm-comfyui--root subgraph 'input
                                                     (plist-get item :index))))
      ('input (lispm-comfyui--change
                (lispm-comfyui-subgraph-remove-slot lispm-comfyui--root subgraph 'output
                                                    (plist-get item :index))))
      (_ (user-error "Choose one of the subgraph's inputs or outputs to take away (n and p)")))))

(defun lispm-comfyui-act ()
  "Do what the item chosen calls for.
Set a widget's value; link an input or an output; rename the node on
its title."
  (interactive)
  (pcase (plist-get (lispm-comfyui--item) :kind)
    ((or 'widget 'control) (lispm-comfyui-edit-value))
    ((or 'input 'output) (lispm-comfyui-connect))
    ('title (lispm-comfyui-rename-node))
    (_ (lispm-comfyui--select-first))))

(defun lispm-comfyui-rename-node (&optional subgraph)
  "Give the node chosen a title; an empty one gives back its type's name.
On a subgraph's input or output, name that.  With SUBGRAPH, the prefix
argument, on a subgraph's node, name the subgraph, all its nodes'."
  (interactive "P")
  (let* ((node (lispm-comfyui--need-node))
         (item (lispm-comfyui--item))
         (definition (lispm-comfyui-node-subgraph lispm-comfyui--root node)))
    (cond
     ((lispm-comfyui--io-p node)
      (let ((which (pcase (plist-get item :kind)
                     ('output 'input) ('input 'output)
                     (_ (user-error "Choose one of the subgraph's inputs or outputs to name (n and p)")))))
        (let ((name (read-string "Name: " (lispm-comfyui-slot-name (plist-get item :slot)))))
          (when (string-blank-p name) (user-error "A name is needed"))
          (lispm-comfyui--change
            (lispm-comfyui-subgraph-rename-slot lispm-comfyui--root (lispm-comfyui--subgraph-shown)
                                                which (plist-get item :index) name)))))
     ((and subgraph definition)
      (let ((name (read-string "Name of the subgraph: " (lispm-comfyui-subgraph-name definition))))
        (when (string-blank-p name) (user-error "A name is needed"))
        (lispm-comfyui--change (setf (lispm-comfyui-subgraph-name definition) name))))
     (t
      (let ((title (read-string "Title: " (lispm-comfyui-node-name node (lispm-comfyui--types)))))
        (lispm-comfyui--change
          (setf (lispm-comfyui-node-title node) (if (string-blank-p title) nil title))))))))

(defun lispm-comfyui--place-io (node x y)
  "Put NODE, a subgraph's inputs' or outputs', at X Y, its body's top left."
  (let* ((subgraph (lispm-comfyui--subgraph-shown))
         (input (eql (lispm-comfyui-node-id node) lispm-comfyui-input-node))
         (saved (if input (lispm-comfyui-subgraph-input-node subgraph)
                  (lispm-comfyui-subgraph-output-node subgraph)))
         (bounding (copy-sequence (or (alist-get 'bounding saved) (vector 0 0 160 60)))))
    (aset bounding 0 x)
    (aset bounding 1 (- y 30))
    (setf (alist-get 'bounding saved) bounding)
    (if input (setf (lispm-comfyui-subgraph-input-node subgraph) saved)
      (setf (lispm-comfyui-subgraph-output-node subgraph) saved))
    (setf (lispm-comfyui-node-x node) x (lispm-comfyui-node-y node) y)))

(defun lispm-comfyui--toggle-mode (mode)
  "Make the node chosen of MODE, or run again if it is already."
  (let ((node (lispm-comfyui--need-node)))
    (when (lispm-comfyui--io-p node) (user-error "A subgraph's inputs and outputs always run"))
    (lispm-comfyui--change
      (setf (lispm-comfyui-node-mode node) (if (eql (lispm-comfyui-node-mode node) mode) 0 mode)))))

(defun lispm-comfyui-bypass ()
  "Bypass the node chosen, or no longer: its inputs pass to its outputs."
  (interactive)
  (lispm-comfyui--toggle-mode 4))

(defun lispm-comfyui-mute ()
  "Mute the node chosen, or no longer: it does not run."
  (interactive)
  (lispm-comfyui--toggle-mode 2))

(defun lispm-comfyui--move (dx dy)
  "Move the node chosen by DX DY on the canvas."
  (let ((node (lispm-comfyui--need-node)))
    (if (and lispm-comfyui--undo (eq last-command this-command))
        (setq lispm-comfyui--modified t)
      (lispm-comfyui--remember)
      (setq lispm-comfyui--modified t))
    (if (lispm-comfyui--io-p node)
        (lispm-comfyui--place-io node (+ (lispm-comfyui-node-x node) dx) (+ (lispm-comfyui-node-y node) dy))
      (cl-incf (lispm-comfyui-node-x node) dx)
      (cl-incf (lispm-comfyui-node-y node) dy))
    (lispm-comfyui--relayout)
    (lispm-comfyui--show-selected)
    (lispm-comfyui--draw)))

(defun lispm-comfyui-move-left () "Move the node chosen left." (interactive) (lispm-comfyui--move -20 0))
(defun lispm-comfyui-move-right () "Move the node chosen right." (interactive) (lispm-comfyui--move 20 0))
(defun lispm-comfyui-move-up () "Move the node chosen up." (interactive) (lispm-comfyui--move 0 -20))
(defun lispm-comfyui-move-down () "Move the node chosen down." (interactive) (lispm-comfyui--move 0 20))

;;;; Going about

(defun lispm-comfyui-left () "Choose the node on the left." (interactive) (lispm-comfyui--towards -1 0))
(defun lispm-comfyui-right () "Choose the node on the right." (interactive) (lispm-comfyui--towards 1 0))
(defun lispm-comfyui-up () "Choose the node above." (interactive) (lispm-comfyui--towards 0 -1))
(defun lispm-comfyui-down () "Choose the node below." (interactive) (lispm-comfyui--towards 0 1))

(defun lispm-comfyui--next-node (step)
  "Choose the node STEP nodes on, in reading order."
  (let* ((order (lispm-comfyui--reading-order))
         (position (or (seq-position order lispm-comfyui--selected
                                     (lambda (place id)
                                       (equal (lispm-comfyui-node-id (plist-get place :node)) id)))
                       (if (> step 0) -1 0))))
    (when order
      (lispm-comfyui--select (lispm-comfyui-node-id
                              (plist-get (nth (mod (+ position step) (length order)) order) :node))))))

(defun lispm-comfyui-next-node () "Choose the next node." (interactive) (lispm-comfyui--next-node 1))
(defun lispm-comfyui-previous-node () "Choose the previous node." (interactive) (lispm-comfyui--next-node -1))

(defun lispm-comfyui--next-item (step)
  "Choose the item STEP items on of the node chosen."
  (let* ((place (or (and lispm-comfyui--selected
                         (lispm-comfyui-canvas-place lispm-comfyui--layout lispm-comfyui--selected))
                    (progn (lispm-comfyui--select-first)
                           (lispm-comfyui-canvas-place lispm-comfyui--layout lispm-comfyui--selected))))
         (count (length (plist-get place :items))))
    (when (> count 0)
      (setq lispm-comfyui--focus (mod (+ lispm-comfyui--focus step) count))
      (lispm-comfyui--draw))))

(defun lispm-comfyui-next-item () "Choose the next item of the node." (interactive) (lispm-comfyui--next-item 1))
(defun lispm-comfyui-previous-item () "Choose the previous item of the node." (interactive) (lispm-comfyui--next-item -1))

(defun lispm-comfyui-go-to-node ()
  "Choose a node asked for by its number or title."
  (interactive)
  (let* ((types (lispm-comfyui--types))
         (candidates (mapcar (lambda (node)
                               (cons (format "#%s %s" (lispm-comfyui-node-id node)
                                             (lispm-comfyui-node-name node types))
                                     (lispm-comfyui-node-id node)))
                             (lispm-comfyui-graph-nodes lispm-comfyui--graph))))
    (unless candidates (user-error "The workflow has no nodes"))
    (lispm-comfyui--select (cdr (assoc (completing-read "Go to node: " candidates nil t) candidates)))))

(defun lispm-comfyui--pan (dx dy)
  "Move the view by DX DY of the window's size."
  (pcase-let ((`(,x ,y ,scale) lispm-comfyui--view)
              (`(,width . ,height) (lispm-comfyui--view-size)))
    (setq lispm-comfyui--view (list (+ x (* dx width)) (+ y (* dy height)) scale))
    (lispm-comfyui--draw)))

(defun lispm-comfyui-pan-left () "Look left." (interactive) (lispm-comfyui--pan -0.25 0))
(defun lispm-comfyui-pan-right () "Look right." (interactive) (lispm-comfyui--pan 0.25 0))
(defun lispm-comfyui-pan-up () "Look up." (interactive) (lispm-comfyui--pan 0 -0.25))
(defun lispm-comfyui-pan-down () "Look down." (interactive) (lispm-comfyui--pan 0 0.25))

(defun lispm-comfyui-zoom-in () "Draw larger." (interactive)
       (lispm-comfyui--set-scale (* 1.2 (nth 2 lispm-comfyui--view))) (lispm-comfyui--draw))
(defun lispm-comfyui-zoom-out () "Draw smaller." (interactive)
       (lispm-comfyui--set-scale (/ (nth 2 lispm-comfyui--view) 1.2)) (lispm-comfyui--draw))

(defun lispm-comfyui-fit ()
  "Show the whole workflow."
  (interactive)
  (lispm-comfyui--fit)
  (lispm-comfyui--draw))

(defun lispm-comfyui-show-selected ()
  "Show the node chosen in the middle of the window."
  (interactive)
  (when-let* ((place (and lispm-comfyui--selected
                          (lispm-comfyui-canvas-place lispm-comfyui--layout lispm-comfyui--selected))))
    (pcase-let ((`(,x . ,y) (lispm-comfyui--centre place))
                (`(,width . ,height) (lispm-comfyui--view-size)))
      (setq lispm-comfyui--view (list (- x (/ width 2)) (- y (/ height 2)) (nth 2 lispm-comfyui--view)))
      (lispm-comfyui--draw))))

;;;; The mouse

(defun lispm-comfyui--event-point (event)
  "Where EVENT is, as a point of the canvas, (X . Y), or nil."
  (let ((position (posn-x-y (event-start event))))
    (and position (lispm-comfyui--canvas-point position))))

(defun lispm-comfyui-mouse-down (event)
  "Choose what EVENT is on; drag a node to move it, or the canvas to look about."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (start (posn-x-y (event-start event)))
         (point (lispm-comfyui--event-point event))
         (hit (and point (lispm-comfyui-canvas-hit lispm-comfyui--layout (car point) (cdr point))))
         (node (and hit (or (lispm-comfyui-node lispm-comfyui--graph (car hit))
                            (seq-find (lambda (node) (eql (lispm-comfyui-node-id node) (car hit)))
                                      (lispm-comfyui--io-nodes)))))
         (origin (and node (cons (lispm-comfyui-node-x node) (lispm-comfyui-node-y node))))
         (view lispm-comfyui--view)
         (moved nil) (last nil))
    (select-window window)
    (if hit
        (setq lispm-comfyui--selected (car hit) lispm-comfyui--focus (cdr hit))
      (setq lispm-comfyui--selected nil lispm-comfyui--focus 0))
    (lispm-comfyui--draw)
    (track-mouse
      (while (progn (setq last (read-event)) (mouse-movement-p last))
        (when-let* ((position (posn-x-y (event-start last))))
          (let ((dx (/ (- (car position) (car start)) (nth 2 view)))
                (dy (/ (- (cdr position) (cdr start)) (nth 2 view))))
            (when (or moved (> (+ (abs dx) (abs dy)) 3))
              (unless (or moved (null node)) (lispm-comfyui--remember))
              (setq moved t)
              (if node
                  (progn
                    (if (lispm-comfyui--io-p node)
                        (lispm-comfyui--place-io node (round (+ (car origin) dx)) (round (+ (cdr origin) dy)))
                      (setf (lispm-comfyui-node-x node) (round (+ (car origin) dx))
                            (lispm-comfyui-node-y node) (round (+ (cdr origin) dy))))
                    (setq lispm-comfyui--modified t))
                (setq lispm-comfyui--view (list (- (car view) dx) (- (cadr view) dy) (nth 2 view))))
              (lispm-comfyui--draw))))))
    (unless (memq (event-basic-type last) '(mouse-1))
      (push last unread-command-events))))

(defun lispm-comfyui-mouse-double (event)
  "Do what the item EVENT is on calls for, as RET does."
  (interactive "e")
  (let* ((point (lispm-comfyui--event-point event))
         (hit (and point (lispm-comfyui-canvas-hit lispm-comfyui--layout (car point) (cdr point)))))
    (when hit
      (setq lispm-comfyui--selected (car hit) lispm-comfyui--focus (cdr hit))
      (lispm-comfyui--draw)
      (lispm-comfyui-act))))

(defun lispm-comfyui--wheel-buffer (event)
  "The buffer under wheel EVENT, or this buffer for a numeric amount."
  (let ((window (and (consp event) (posn-window (event-start event)))))
    (if (window-live-p window) (window-buffer window) (current-buffer))))

(defun lispm-comfyui--wheel (dx dy &optional event)
  "Scroll the view under EVENT by DX DY lines' worth.
Without EVENT, scroll this buffer's view."
  (with-current-buffer (lispm-comfyui--wheel-buffer event)
    (pcase-let ((`(,x ,y ,scale) lispm-comfyui--view))
      (setq lispm-comfyui--view (list (+ x (/ (* dx 40) scale)) (+ y (/ (* dy 40) scale)) scale))
      (lispm-comfyui--draw-soon))))

(defun lispm-comfyui--wheel-amount (amount &optional horizontal)
  "The lines turned by AMOUNT, a wheel event or a number of lines.
For a precise touchpad event, forty pixels count as one line, as
`lispm-comfyui--wheel' pans them.  HORIZONTAL uses its sideways delta.
Repeated events' click counts do not accelerate the movement."
  (let* ((delta (and (consp amount) (nth 4 amount)))
         (pixels (and (consp delta) (if horizontal (car delta) (cdr delta)))))
    (cond ((numberp amount) (abs amount))
          ((numberp pixels) (/ (abs pixels) 40.0))
          ((consp amount) (event-line-count amount))
          (t 1))))

(defun lispm-comfyui-wheel-up (&optional amount)
  "Look up by AMOUNT, a Shift-wheel event or a number of lines."
  (interactive "e")
  (lispm-comfyui--wheel 0 (- (lispm-comfyui--wheel-amount amount)) amount))

(defun lispm-comfyui-wheel-down (&optional amount)
  "Look down by AMOUNT, a Shift-wheel event or a number of lines."
  (interactive "e")
  (lispm-comfyui--wheel 0 (lispm-comfyui--wheel-amount amount) amount))

(defun lispm-comfyui-wheel-left (&optional amount)
  "Look left by AMOUNT, a sideways wheel event or a number of lines."
  (interactive "e")
  (lispm-comfyui--wheel (- (lispm-comfyui--wheel-amount amount t)) 0 amount))

(defun lispm-comfyui-wheel-right (&optional amount)
  "Look right by AMOUNT, a sideways wheel event or a number of lines."
  (interactive "e")
  (lispm-comfyui--wheel (lispm-comfyui--wheel-amount amount t) 0 amount))

(defun lispm-comfyui-wheel-zoom (event)
  "Zoom about where EVENT, a vertical wheel's turn, is.
Its line count, or precise touchpad delta, sets how far to zoom."
  (interactive "e")
  (with-current-buffer (lispm-comfyui--wheel-buffer event)
    (let ((point (lispm-comfyui--event-point event))
          (amount (lispm-comfyui--wheel-amount event)))
      (lispm-comfyui--set-scale (* (nth 2 lispm-comfyui--view)
                                   (expt 1.1 (if (memq (event-basic-type event) '(wheel-up mouse-4))
                                                 amount (- amount))))
                                point)
      (lispm-comfyui--draw-soon))))

(defun lispm-comfyui-pinch (event)
  "Zoom as EVENT, a pinch of a touchpad, says."
  (interactive "e")
  (let ((scale (nth 4 event)))
    (when (numberp scale)
      (lispm-comfyui--set-scale (* (nth 2 lispm-comfyui--view) (if (> scale 1) 1.04 (/ 1 1.04)))
                                (lispm-comfyui--event-point event))
      (lispm-comfyui--draw-soon))))

;;;; Running

(defun lispm-comfyui--canvas-buffers ()
  "The buffers showing workflows."
  (seq-filter (lambda (buffer)
                (eq (buffer-local-value 'major-mode buffer) 'lispm-comfyui-mode))
              (buffer-list)))

(defun lispm-comfyui--run-of (prompt-id)
  "The buffer and run of PROMPT-ID, (BUFFER . RUN), or nil."
  (cl-loop for buffer in (lispm-comfyui--canvas-buffers)
           for run = (seq-find (lambda (run) (equal (plist-get run :id) prompt-id))
                               (buffer-local-value 'lispm-comfyui--runs buffer))
           when run return (cons buffer run)))

(defun lispm-comfyui--active-run ()
  "The buffer and run the server runs now, (BUFFER . RUN), or nil."
  (cl-loop for buffer in (lispm-comfyui--canvas-buffers)
           for run = (seq-find (lambda (run) (eq (plist-get run :status) 'running))
                               (buffer-local-value 'lispm-comfyui--runs buffer))
           when run return (cons buffer run)))

(defun lispm-comfyui--finish (run status &optional message)
  "End RUN, of this buffer, as STATUS says, with MESSAGE."
  (unless (memq (plist-get run :status) '(done failed stopped))
    (plist-put run :status status)
    (plist-put run :message message)
    (setq lispm-comfyui--executing nil
          lispm-comfyui--progress nil
          lispm-comfyui--preview nil
          lispm-comfyui--status
          (pcase status
            ('done (format "Done in %.1f s" (float-time (time-since (plist-get run :start)))))
            ('stopped "Stopped")
            (_ (concat "Failed: " (or message "")))))
    (lispm-comfyui--draw-soon)
    (lispm-comfyui--call-back run)))

(defun lispm-comfyui--call-back (run)
  "Call RUN's callbacks, once it is over and its images are here."
  (when (and (memq (plist-get run :status) '(done failed stopped))
             (<= (or (plist-get run :pending) 0) 0))
    (let ((callbacks (plist-get run :callbacks)))
      (plist-put run :callbacks nil)
      (dolist (callback callbacks)
        (with-demoted-errors "ComfyUI: %S" (funcall callback run))))))

(defun lispm-comfyui--take-images (run node images)
  "Fetch IMAGES NODE made in RUN, of this buffer, and show them in it."
  (let ((buffer (current-buffer))
        (node (format "%s" node))
        (first t))
    (seq-doseq (image images)
      (when (and (consp image) (alist-get 'filename image)
                 (string-match-p "\\.\\(png\\|jpe?g\\|webp\\|gif\\)\\'" (alist-get 'filename image)))
        (plist-put run :pending (1+ (or (plist-get run :pending) 0)))
        (lispm-comfyui-fetch-image
         image
         (lambda (file _error)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (plist-put run :pending (1- (plist-get run :pending)))
               (when file
                 (plist-put run :files (append (plist-get run :files) (list file)))
                 (setf (alist-get node lispm-comfyui--images nil nil #'equal)
                       (append (and (not first) (alist-get node lispm-comfyui--images nil nil #'equal))
                               (list file)))
                 (setq first nil)
                 (lispm-comfyui--draw-soon))
               (lispm-comfyui--call-back run)))))))))

(defun lispm-comfyui--on-message (type data)
  "Take the message of TYPE, with DATA, the server's WebSocket sent."
  (pcase type
    ("status"
     (setq lispm-comfyui--queue-remaining
           (or (alist-get 'queue_remaining (alist-get 'exec_info (alist-get 'status data))) 0))
     (dolist (buffer (lispm-comfyui--canvas-buffers))
       (with-current-buffer buffer (force-mode-line-update))))
    ("closed" (lispm-comfyui--recover))
    ("preview"
     (when-let* ((found (or (and (alist-get 'node data) (lispm-comfyui--active-run))
                            (lispm-comfyui--active-run))))
       (with-current-buffer (car found)
         (setq lispm-comfyui--preview
               (cons (or (alist-get 'node data) lispm-comfyui--executing) (alist-get 'file data)))
         (lispm-comfyui--draw-soon))))
    (_
     (when-let* ((id (alist-get 'prompt_id data))
                 (found (lispm-comfyui--run-of id)))
       (with-current-buffer (car found)
         (let ((run (cdr found)))
           (pcase type
             ("execution_start"
              (plist-put run :status 'running)
              (setq lispm-comfyui--status "Running…"))
             ("executing"
              (let ((node (alist-get 'node data)))
                (if (stringp node)
                    (progn
                      (plist-put run :status 'running)
                      (setq lispm-comfyui--executing node
                            lispm-comfyui--progress nil
                            lispm-comfyui--status
                            (format "Running %s" (lispm-comfyui--execution-name node))))
                  (lispm-comfyui--finish run 'done))))
             ("progress"
              (setq lispm-comfyui--progress (cons (alist-get 'value data) (alist-get 'max data)))
              (when (stringp (alist-get 'node data))
                (setq lispm-comfyui--executing (alist-get 'node data)))
              (setq lispm-comfyui--status
                    (format "%s %s/%s" (replace-regexp-in-string " [0-9]+/[0-9]+\\'" ""
                                                                 (or lispm-comfyui--status "Running"))
                            (alist-get 'value data) (alist-get 'max data))))
             ("executed"
              (let ((output (alist-get 'output data)))
                (lispm-comfyui--take-images run (or (alist-get 'display_node data)
                                                    (alist-get 'node data))
                                            (or (alist-get 'images output) []))))
             ("execution_success" (lispm-comfyui--finish run 'done))
             ("execution_interrupted" (lispm-comfyui--finish run 'stopped))
             ("execution_error"
              (let ((node (alist-get 'node_id data))
                    (message (format "%s" (or (alist-get 'exception_message data) "failed"))))
                (when node
                  (setf (alist-get (format "%s" node) lispm-comfyui--errors nil nil #'equal)
                        (string-trim message)))
                (lispm-comfyui--finish run 'failed (string-trim message)))))
           (lispm-comfyui--draw-soon)))))))

(add-hook 'lispm-comfyui-message-functions #'lispm-comfyui--on-message)

(defun lispm-comfyui--recover ()
  "After the WebSocket closed: open it again, and ask how the runs went."
  (when-let* ((buffers (seq-filter (lambda (buffer)
                                     (seq-find (lambda (run) (memq (plist-get run :status) '(queued running)))
                                               (buffer-local-value 'lispm-comfyui--runs buffer)))
                                   (lispm-comfyui--canvas-buffers))))
    (run-with-timer
     2 nil
     (lambda ()
       (ignore-errors (lispm-comfyui-connect-socket))
       (dolist (buffer buffers)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (dolist (run lispm-comfyui--runs)
               (when (memq (plist-get run :status) '(queued running))
                 (lispm-comfyui--check-run run))))))))))

(defun lispm-comfyui--check-run (run)
  "Ask the server how RUN, of this buffer, went, and take what it made."
  (let ((buffer (current-buffer)))
    (lispm-comfyui-history
     (plist-get run :id)
     (lambda (entry _error)
       (when (and entry (buffer-live-p buffer))
         (with-current-buffer buffer
           (pcase-dolist (`(,node . ,output) (alist-get 'outputs entry))
             (lispm-comfyui--take-images run (symbol-name node) (or (alist-get 'images output) [])))
           (let ((status (alist-get 'status_str (alist-get 'status entry))))
             (lispm-comfyui--finish run (if (equal status "error") 'failed 'done)))))))))

(defun lispm-comfyui--execution-name (key)
  "What the node numbered KEY in the prompt, as 12:5, is called."
  (let ((graph lispm-comfyui--root) (node nil))
    (dolist (part (split-string key ":"))
      (setq node (and graph (lispm-comfyui-node graph (string-to-number part)))
            graph (when-let* ((subgraph (and node (lispm-comfyui-node-subgraph lispm-comfyui--root node))))
                    (lispm-comfyui-subgraph-graph subgraph))))
    (if node (lispm-comfyui-node-name node (lispm-comfyui--types)) (concat "#" key))))

(defun lispm-comfyui--node-errors (answer)
  "What the server said was wrong with each node, in ANSWER to a prompt.
An alist from node numbers, strings, to messages."
  (cl-loop for (id . entry) in (alist-get 'node_errors answer)
           collect (cons (symbol-name id)
                         (mapconcat (lambda (error)
                                      (string-join (seq-remove #'string-empty-p
                                                               (list (or (alist-get 'message error) "")
                                                                     (or (alist-get 'details error) "")))
                                                   ": "))
                                    (alist-get 'errors entry) "; "))))

(defun lispm-comfyui-run (&optional callback)
  "Run the workflow on the server; each node is lit while it runs.
CALLBACK, if given, is called with the run once it is over, a plist
as `lispm-comfyui--runs' holds."
  (interactive)
  (let* ((types (lispm-comfyui--need-types))
         (resolved (make-hash-table))
         (prompt (condition-case err (lispm-comfyui-prompt lispm-comfyui--root types resolved)
                   (lispm-comfyui-error (user-error "%s" (cadr err)))))
         ;; The workflow kept with the images holds the texts dynamic
         ;; prompts became, as the frontend keeps them.
         (workflow (let ((copy (lispm-comfyui-graph-copy lispm-comfyui--root)))
                     (maphash (lambda (id texts)
                                (when-let* ((node (lispm-comfyui-node copy id)))
                                  (pcase-dolist (`(,name . ,text) texts)
                                    (ignore-errors (lispm-comfyui-set-widget node types name text)))))
                              resolved)
                     (lispm-comfyui-graph-json copy)))
         (buffer (current-buffer)))
    (unless prompt (user-error "The workflow has no node that runs"))
    (condition-case err
        (lispm-comfyui-connect-socket)
      (error (user-error "Cannot reach %s: %s.  Press , for Settings"
                         lispm-comfyui-url (lispm-comfyui--network-error lispm-comfyui-url err))))
    (setq lispm-comfyui--errors nil
          lispm-comfyui--status "Queueing…")
    (lispm-comfyui--draw-soon)
    (lispm-comfyui-queue
     prompt workflow
     (lambda (answer error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (if error
               (let ((run (list :status 'failed :message error :start (current-time))))
                 (setq lispm-comfyui--errors (lispm-comfyui--node-errors answer)
                       lispm-comfyui--status (concat "Failed: " error))
                 (lispm-comfyui--draw-soon)
                 (unless callback (message "ComfyUI: %s" error))
                 (when callback (funcall callback run)))
             (let ((run (list :id (alist-get 'prompt_id answer) :status 'queued
                              :start (current-time) :pending 0
                              :callbacks (and callback (list callback)))))
               (push run lispm-comfyui--runs)
               (setq lispm-comfyui--status "Queued")
               ;; Seeds and the like change after each run, as the
               ;; frontend changes them.
               (let ((changed nil))
                 (dolist (graph (lispm-comfyui--graphs lispm-comfyui--root))
                   (dolist (node (lispm-comfyui-graph-nodes graph))
                     (when (lispm-comfyui-after-run node types) (setq changed t))))
                 (when changed
                   (setq lispm-comfyui--modified t)
                   (cl-incf lispm-comfyui--tick)))
               (lispm-comfyui--draw-soon)))))))))

(defun lispm-comfyui-stop ()
  "Stop the run of this workflow the server is running."
  (interactive)
  (let ((run (or (seq-find (lambda (run) (memq (plist-get run :status) '(queued running)))
                           lispm-comfyui--runs)
                 (user-error "Nothing of this workflow is running"))))
    (if (eq (plist-get run :status) 'running)
        (lispm-comfyui-interrupt (plist-get run :id))
      (lispm-comfyui-request "/queue" #'ignore :method "POST"
                             :data `((delete . [,(plist-get run :id)])))
      (lispm-comfyui--finish run 'stopped))
    (message "Stopping…")))

(defun lispm-comfyui-show-image ()
  "Show the image the node chosen made, or the last any made, in a window."
  (interactive)
  (let* ((node (lispm-comfyui--node))
         (files (or (and node (cdr (assoc (lispm-comfyui--execution-key (lispm-comfyui-node-id node))
                                          lispm-comfyui--images)))
                    (plist-get (car lispm-comfyui--runs) :files)
                    (user-error "No image yet: r runs the workflow"))))
    (display-buffer (find-file-noselect (car (last files))))))

;;;; Workflows

(defvar-local lispm-comfyui--workflows-main nil
  "The window whose workflows this sidebar lists.")

(defvar-local lispm-comfyui--workflows-names nil
  "The names last read from the server's library.")

(defvar-local lispm-comfyui--workflows-server nil
  "The address the workflows were asked of.")

(defvar-local lispm-comfyui--workflows-status nil
  "What the sidebar says while waiting, or when a request failed.")

(defvar-local lispm-comfyui--workflows-request nil
  "The current request for names, whose identity discards older answers.")

(defvar-local lispm-comfyui--workflows-timer nil
  "The timer ending the wait for the list, or nil.")

(defvar-local lispm-comfyui--workflows-opening nil
  "The current request to open a workflow, or nil.")

(defvar-local lispm-comfyui--workflows-open-timer nil
  "The timer ending the wait for a workflow, or nil.")

(defun lispm-comfyui--workflows-draw ()
  "Draw the library's names, marking the workflow in the main window."
  (let ((inhibit-read-only t)
        (chosen (get-text-property (point) 'lispm-comfyui-workflow))
        (name (when (window-live-p lispm-comfyui--workflows-main)
                (buffer-local-value 'lispm-comfyui--name
                                    (window-buffer lispm-comfyui--workflows-main)))))
    (erase-buffer)
    (insert "\n  " (propertize "Workflows" 'face 'bold) "\n\n")
    (when lispm-comfyui--workflows-status
      (insert "  " (replace-regexp-in-string "[\n\r\t]+" " " lispm-comfyui--workflows-status) "\n"))
    (dolist (entry lispm-comfyui--workflows-names)
      (insert (propertize (concat (if (equal name entry) "  • " "    ")
                                 (replace-regexp-in-string "[\n\r\t]+" " " (file-name-sans-extension entry))
                                 "\n")
                          'lispm-comfyui-workflow entry
                          'mouse-face 'highlight 'help-echo entry
                          'face (and (equal name entry) 'bold))))
    (unless (or lispm-comfyui--workflows-status lispm-comfyui--workflows-names)
      (insert "  No workflows in the library.\n"))
    (goto-char (point-min))
    (let ((position nil))
      (while (and (not (eobp)) (not position))
        (let ((entry (get-text-property (point) 'lispm-comfyui-workflow)))
          (when (and entry (equal entry (or chosen name))) (setq position (point))))
        (forward-line 1))
      (goto-char (or position (text-property-not-all (point-min) (point-max)
                                                    'lispm-comfyui-workflow nil)
                     (point-min))))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window (point)))
    (set-buffer-modified-p nil)))

(defun lispm-comfyui--workflows-cancel ()
  "Discard requests and cancel their timers when this sidebar goes."
  (dolist (timer (list lispm-comfyui--workflows-timer lispm-comfyui--workflows-open-timer))
    (when timer (cancel-timer timer)))
  (setq lispm-comfyui--workflows-request nil lispm-comfyui--workflows-timer nil
        lispm-comfyui--workflows-opening nil lispm-comfyui--workflows-open-timer nil))

(defun lispm-comfyui-workflows-refresh ()
  "Read the server's workflow list again, without waiting for it."
  (interactive)
  (lispm-comfyui--workflows-cancel)
  (let ((buffer (current-buffer))
        (server lispm-comfyui-url)
        (request (list t)))
    (setq lispm-comfyui--workflows-request request
          lispm-comfyui--workflows-server server
          lispm-comfyui--workflows-names nil
          lispm-comfyui--workflows-status "Reading workflows…")
    (lispm-comfyui--workflows-draw)
    (cl-labels ((answer (names error)
                 (when (and (buffer-live-p buffer) (equal server lispm-comfyui-url))
                   (with-current-buffer buffer
                     (when (eq request lispm-comfyui--workflows-request)
                       (when lispm-comfyui--workflows-timer
                         (cancel-timer lispm-comfyui--workflows-timer))
                       (setq lispm-comfyui--workflows-request nil
                             lispm-comfyui--workflows-timer nil
                             lispm-comfyui--workflows-names (unless error names)
                             lispm-comfyui--workflows-status
                             (and error (concat "Cannot list workflows: " error)))
                       (lispm-comfyui--workflows-draw))))))
      (setq lispm-comfyui--workflows-timer
            (run-with-timer 30 nil (lambda () (answer nil "The server did not answer."))))
      (lispm-comfyui-list-workflows #'answer))))

(defun lispm-comfyui--refresh-workflows ()
  "Read the library again in every workflows sidebar."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'lispm-comfyui-workflows-mode)
        (lispm-comfyui-workflows-refresh)))))

(defun lispm-comfyui--workflows-ensure (&optional refresh)
  "Show this workflow's sidebar; with REFRESH, read the library again.
Return its window, or nil if the workflow's window is too narrow."
  (when-let* ((main (lispm-comfyui--window)))
    (let ((side (window-parameter main 'lispm-comfyui-workflows)))
      (unless (and (window-live-p side)
                   (eq (buffer-local-value 'major-mode (window-buffer side))
                       'lispm-comfyui-workflows-mode))
        (setq side nil)
        (when (> (window-total-width main) (+ lispm-comfyui-sidebar-width 40))
          (setq side (split-window main (- lispm-comfyui-sidebar-width) 'left))
          (let ((buffer (generate-new-buffer "*ComfyUI Workflows*")))
            (with-current-buffer buffer
              (lispm-comfyui-workflows-mode)
              (setq lispm-comfyui--workflows-main main))
            (set-window-buffer side buffer))
          (set-window-parameter main 'lispm-comfyui-workflows side)
          (set-window-dedicated-p side t)
          (set-window-fringes side 0 0)
          (set-window-scroll-bars side 0 nil 0 nil)
          (window-preserve-size side t t)
          (setq refresh t)))
      (when side
        (with-current-buffer (window-buffer side)
          (if (or refresh (not (equal lispm-comfyui-url lispm-comfyui--workflows-server)))
              (lispm-comfyui-workflows-refresh)
            (lispm-comfyui--workflows-draw))))
      side)))

(defun lispm-comfyui-show-workflows ()
  "Go to the workflows on the left, or back to the workflow shown."
  (interactive)
  (if (derived-mode-p 'lispm-comfyui-workflows-mode)
      (when (window-live-p lispm-comfyui--workflows-main)
        (select-window lispm-comfyui--workflows-main))
    (when-let* ((side (lispm-comfyui--workflows-ensure)))
      (select-window side))))

(defun lispm-comfyui-workflows-next (&optional backward)
  "Choose the next workflow, or the previous one with BACKWARD."
  (interactive)
  (let ((start (point)))
    (while (and (zerop (forward-line (if backward -1 1)))
                (not (get-text-property (point) 'lispm-comfyui-workflow))))
    (unless (get-text-property (point) 'lispm-comfyui-workflow)
      (goto-char start))))

(defun lispm-comfyui-workflows-previous ()
  "Choose the previous workflow."
  (interactive)
  (lispm-comfyui-workflows-next t))

(defun lispm-comfyui-workflows-open (&optional event)
  "Open the workflow at point, or at mouse EVENT, in the main window."
  (interactive (list (and (mouse-event-p last-input-event) last-input-event)))
  (when event (mouse-set-point event))
  (when-let* ((name (get-text-property (point) 'lispm-comfyui-workflow))
              (main lispm-comfyui--workflows-main)
              ((window-live-p main)))
    (let ((sidebar (current-buffer))
          (buffer (window-buffer main))
          (server lispm-comfyui-url)
          (request (list t)))
      (when lispm-comfyui--workflows-open-timer (cancel-timer lispm-comfyui--workflows-open-timer))
      (setq lispm-comfyui--workflows-opening request
            lispm-comfyui--workflows-status (concat "Opening " name "…"))
      (lispm-comfyui--workflows-draw)
      (cl-labels ((current ()
                   (and (buffer-live-p sidebar) (window-live-p main)
                        (eq buffer (window-buffer main)) (equal server lispm-comfyui-url)
                        (eq request (buffer-local-value 'lispm-comfyui--workflows-opening sidebar))))
                 (finish (json types error)
                   (when (current)
                     (with-current-buffer sidebar
                       (when lispm-comfyui--workflows-open-timer
                         (cancel-timer lispm-comfyui--workflows-open-timer))
                       (setq lispm-comfyui--workflows-opening nil
                             lispm-comfyui--workflows-open-timer nil
                             lispm-comfyui--workflows-status nil)
                       (condition-case err
                           (if error
                               (setq lispm-comfyui--workflows-status (concat "Cannot open workflow: " error))
                             (with-selected-window main
                               (lispm-comfyui--show (lispm-comfyui-graph-from-json json types) name)))
                         (error (setq lispm-comfyui--workflows-status (error-message-string err))))
                       (lispm-comfyui--workflows-draw)))))
        (setq lispm-comfyui--workflows-open-timer
              (run-with-timer 30 nil (lambda () (finish nil nil "The server did not answer."))))
        (lispm-comfyui-types-of-server
         (lambda (types error)
           (when (current)
             (if error (finish nil nil error)
               (lispm-comfyui-read-workflow
                name (lambda (json failure) (finish json types failure)))))))))))

(defvar lispm-comfyui-workflows-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key '("n" "<down>")) (keymap-set map key #'lispm-comfyui-workflows-next))
    (dolist (key '("p" "<up>")) (keymap-set map key #'lispm-comfyui-workflows-previous))
    (keymap-set map "RET" #'lispm-comfyui-workflows-open)
    (keymap-set map "<mouse-1>" #'lispm-comfyui-workflows-open)
    (keymap-set map "g" #'lispm-comfyui-workflows-refresh)
    (keymap-set map "w" #'lispm-comfyui-show-workflows)
    map)
  "Keys of the server's workflow list.")

(define-derived-mode lispm-comfyui-workflows-mode special-mode "ComfyUI Workflows"
  "The server's workflows: RET opens one; w goes back to the canvas.
\\{lispm-comfyui-workflows-mode-map}"
  (setq-local truncate-lines t
              cursor-type nil
              header-line-format nil)
  ;; A name too long for the sidebar ends in an ellipsis, not a dollar.
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table 'truncation ?…)
  (add-hook 'kill-buffer-hook #'lispm-comfyui--workflows-cancel nil t))

(defun lispm-comfyui--buffer-name (name)
  "The name of the buffer of the workflow NAME, nil for a new one."
  (if name (format "*ComfyUI: %s*" (file-name-sans-extension name)) "*ComfyUI*"))

(defun lispm-comfyui--show (graph name)
  "Show GRAPH, the workflow NAME, in a buffer; return the buffer."
  (let ((buffer (if (and (derived-mode-p 'lispm-comfyui-mode)
                         (not lispm-comfyui--modified) (null lispm-comfyui--name)
                         (null (lispm-comfyui-graph-nodes lispm-comfyui--root)))
                    (current-buffer)
                  (or (and name (get-buffer (lispm-comfyui--buffer-name name)))
                      (generate-new-buffer (lispm-comfyui--buffer-name name))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-comfyui-mode) (lispm-comfyui-mode))
      (when (and lispm-comfyui--modified (lispm-comfyui-graph-nodes lispm-comfyui--root)
                 (not (yes-or-no-p (format "The workflow %s has changes not saved; let them go? "
                                           (or lispm-comfyui--name "Untitled")))))
        (user-error "Kept as it was"))
      (lispm-comfyui--set-root graph)
      (setq lispm-comfyui--name name
            lispm-comfyui--modified nil
            lispm-comfyui--undo nil lispm-comfyui--redo nil
            lispm-comfyui--images nil lispm-comfyui--errors nil
            lispm-comfyui--selected nil lispm-comfyui--focus 0)
      (rename-buffer (lispm-comfyui--buffer-name name) t))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (lispm-comfyui--workflows-ensure t)
      (lispm-comfyui--fit)
      (lispm-comfyui--draw))
    buffer))

(defun lispm-comfyui-open (name)
  "Open the workflow NAME of the server's library."
  (interactive
   (list (let ((names nil) (failure nil))
           (lispm-comfyui-list-workflows (lambda (list error) (setq names (or list t) failure error)))
           (with-timeout (30 (user-error "The server at %s did not answer" lispm-comfyui-url))
             (while (not names) (accept-process-output nil 0.05)))
           (when failure (user-error "Cannot list the workflows: %s" failure))
           (completing-read "Open workflow: " (if (listp names) names nil) nil t))))
  (lispm-comfyui--need-types)
  (let ((json nil) (failure nil))
    (lispm-comfyui-read-workflow name (lambda (value error) (setq json (or value t) failure error)))
    (with-timeout (30 (user-error "The server at %s did not answer" lispm-comfyui-url))
      (while (not json) (accept-process-output nil 0.05)))
    (when failure (user-error "Cannot read %s: %s" name failure))
    (lispm-comfyui--show (lispm-comfyui-graph-from-json json (lispm-comfyui-cached-types)) name)))

(defun lispm-comfyui-open-file (file)
  "Open the workflow in FILE, saved by ComfyUI, or a prompt in its API's form."
  (interactive "fOpen workflow file: ")
  (let* ((json (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (json-parse-buffer :object-type 'alist)))
         (graph (if (assq 'nodes json)
                    (progn (lispm-comfyui--need-types)
                           (lispm-comfyui-graph-from-json json (lispm-comfyui-cached-types)))
                  (lispm-comfyui-graph-from-prompt json (lispm-comfyui--need-types)))))
    (with-current-buffer (lispm-comfyui--show graph nil)
      (setq lispm-comfyui--modified t))))

(defun lispm-comfyui-new ()
  "Start a new, empty workflow."
  (interactive)
  (lispm-comfyui--show (lispm-comfyui-graph-create) nil))

(defun lispm-comfyui-save (&optional name)
  "Save the workflow in the server's library, as NAME if given.
Without a name, it is asked for the first time."
  (interactive)
  (let* ((name (or name lispm-comfyui--name
                   (let ((name (read-string "Save workflow as: ")))
                     (when (string-blank-p name) (user-error "A workflow needs a name"))
                     name)))
         (name (if (string-suffix-p ".json" name) name (concat name ".json")))
         (buffer (current-buffer))
         (tick lispm-comfyui--tick))
    (lispm-comfyui-write-workflow
     name (lispm-comfyui-graph-string lispm-comfyui--root)
     (lambda (_answer error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (if error
               (message "Cannot save %s: %s" name error)
             (setq lispm-comfyui--name name
                   lispm-comfyui--modified (/= tick lispm-comfyui--tick))
             (rename-buffer (lispm-comfyui--buffer-name name) t)
             (force-mode-line-update)
             (lispm-comfyui--refresh-workflows)
             (message "Saved %s" name))))))))

(defun lispm-comfyui-save-as ()
  "Save the workflow in the server's library under a name asked for."
  (interactive)
  (lispm-comfyui-save (read-string "Save workflow as: "
                                   (and lispm-comfyui--name
                                        (file-name-sans-extension lispm-comfyui--name)))))

(defun lispm-comfyui-reload (&rest _)
  "Read the server's node types again, and draw anew."
  (interactive)
  (clrhash lispm-comfyui--inputs-fetched)
  (lispm-comfyui--load-types t)
  (lispm-comfyui--draw))

(defun lispm-comfyui--modified-p () "Non-nil if the workflow changed." lispm-comfyui--modified)

;;;; Subgraphs

(defun lispm-comfyui-enter-subgraph ()
  "Show the inside of the subgraph of the node chosen, to change it.
What changes there changes in all its subgraph's nodes."
  (interactive)
  (let ((node (lispm-comfyui--need-node)))
    (unless (lispm-comfyui-node-subgraph lispm-comfyui--root node)
      (user-error "Not a subgraph's node"))
    (setq lispm-comfyui--path (append lispm-comfyui--path (list (lispm-comfyui-node-id node)))
          lispm-comfyui--graph (lispm-comfyui--graph-at lispm-comfyui--root lispm-comfyui--path)
          lispm-comfyui--marked nil
          lispm-comfyui--selected nil
          lispm-comfyui--focus 0)
    (lispm-comfyui--fit)
    (lispm-comfyui--draw)))

(defun lispm-comfyui-leave-subgraph ()
  "Show what the subgraph shown is inside, its node chosen."
  (interactive)
  (unless lispm-comfyui--path (user-error "Not inside a subgraph"))
  (let ((id (car (last lispm-comfyui--path))))
    (setq lispm-comfyui--path (butlast lispm-comfyui--path)
          lispm-comfyui--graph (lispm-comfyui--graph-at lispm-comfyui--root lispm-comfyui--path)
          lispm-comfyui--marked nil)
    (lispm-comfyui--fit)
    (lispm-comfyui--select id)))

(defun lispm-comfyui-mark ()
  "Mark the node chosen, or no longer, to make a subgraph of the nodes marked."
  (interactive)
  (let ((node (lispm-comfyui--need-node)))
    (when (lispm-comfyui--io-p node) (user-error "A subgraph's inputs and outputs are not marked"))
    (let ((id (lispm-comfyui-node-id node)))
      (setq lispm-comfyui--marked (if (member id lispm-comfyui--marked)
                                      (delete id lispm-comfyui--marked)
                                    (cons id lispm-comfyui--marked))))
    (lispm-comfyui--draw)))

(defun lispm-comfyui-unmark-all ()
  "Mark no node."
  (interactive)
  (setq lispm-comfyui--marked nil)
  (lispm-comfyui--draw))

(defun lispm-comfyui-make-subgraph (name)
  "Make a subgraph, NAME, of the nodes marked, or of the node chosen.
A node of it takes their place, linked as they were."
  (interactive (list (read-string "Name of the subgraph: " nil nil "New Subgraph")))
  (let* ((types (lispm-comfyui--need-types))
         (nodes (or (delq nil (mapcar (lambda (id) (lispm-comfyui-node lispm-comfyui--graph id))
                                      lispm-comfyui--marked))
                    (let ((node (lispm-comfyui--need-node)))
                      (when (lispm-comfyui--io-p node) (user-error "Choose or mark the nodes"))
                      (list node)))))
    (ignore types)
    (lispm-comfyui--change
      (let ((node (lispm-comfyui-graph-convert-to-subgraph
                   lispm-comfyui--graph (lispm-comfyui-cached-types) nodes name)))
        (setq lispm-comfyui--marked nil
              lispm-comfyui--selected (lispm-comfyui-node-id node)
              lispm-comfyui--focus 0)))
    (lispm-comfyui--select lispm-comfyui--selected)))

(defun lispm-comfyui-unpack-subgraph ()
  "Put the nodes inside the subgraph's node chosen in its place."
  (interactive)
  (let ((node (lispm-comfyui--need-node)))
    (unless (lispm-comfyui-node-subgraph lispm-comfyui--root node)
      (user-error "Not a subgraph's node"))
    (lispm-comfyui--need-types)
    (lispm-comfyui--change
      (let ((nodes (lispm-comfyui-graph-unpack lispm-comfyui--graph (lispm-comfyui-cached-types) node)))
        (setq lispm-comfyui--selected (and nodes (lispm-comfyui-node-id (car nodes)))
              lispm-comfyui--focus 0)))
    (lispm-comfyui--select lispm-comfyui--selected)))

(defun lispm-comfyui--subgraph-node-p ()
  "Non-nil if the node chosen is a subgraph's."
  (when-let* ((node (lispm-comfyui--node)))
    (lispm-comfyui-node-subgraph lispm-comfyui--root node)))
(defun lispm-comfyui--running-p ()
  "Non-nil while a run of this workflow is queued or running."
  (seq-find (lambda (run) (memq (plist-get run :status) '(queued running))) lispm-comfyui--runs))
(defun lispm-comfyui--chosen-p () "Non-nil if a node is chosen." (lispm-comfyui--node))

;;;; The window

;; Each command once: its keys, its menu item and, for the main ones,
;; the words of a button, for an interface showing them as buttons.
(lispm-kit-define-commands lispm-comfyui-mode
  (lispm-comfyui-act :key "RET" :help "Set the value, link the socket, or rename the node")
  (lispm-comfyui-left :key ("<left>" "b" "C-b") :help "Choose the node on the left")
  (lispm-comfyui-right :key ("<right>" "f" "C-f") :help "Choose the node on the right")
  (lispm-comfyui-up :key ("<up>" "M-p") :help "Choose the node above")
  (lispm-comfyui-down :key ("<down>" "M-n") :help "Choose the node below")
  (lispm-comfyui-next-item :key ("n" "C-n") :help "Choose the next item of the node")
  (lispm-comfyui-previous-item :key ("p" "C-p") :help "Choose the previous item of the node")
  (lispm-comfyui-move-left :key "M-<left>" :help "Move the node left")
  (lispm-comfyui-move-right :key "M-<right>" :help "Move the node right")
  (lispm-comfyui-move-up :key "M-<up>" :help "Move the node up")
  (lispm-comfyui-move-down :key "M-<down>" :help "Move the node down")
  (lispm-comfyui-pan-left :key "S-<left>" :help "Look left")
  (lispm-comfyui-pan-right :key "S-<right>" :help "Look right")
  (lispm-comfyui-pan-up :key "S-<up>" :help "Look up")
  (lispm-comfyui-pan-down :key "S-<down>" :help "Look down")
  (lispm-comfyui-mouse-down :key "<down-mouse-1>")
  (lispm-comfyui-mouse-double :key "<double-down-mouse-1>")
  (ignore :key ("<mouse-1>" "<double-mouse-1>" "<drag-mouse-1>"))
  (lispm-comfyui-wheel-up :key ("S-<wheel-up>" "S-<mouse-4>"))
  (lispm-comfyui-wheel-down :key ("S-<wheel-down>" "S-<mouse-5>"))
  (lispm-comfyui-wheel-left :key ("<wheel-left>" "<mouse-6>"))
  (lispm-comfyui-wheel-right :key ("<wheel-right>" "<mouse-7>"))
  (lispm-comfyui-wheel-zoom :key ("<wheel-up>" "<wheel-down>" "<mouse-4>" "<mouse-5>"
                                 "C-<wheel-up>" "C-<wheel-down>" "C-<mouse-4>" "C-<mouse-5>"))
  (lispm-comfyui-pinch :key "<pinch>")
  :menu "Workflow"
  (lispm-comfyui-new :key "N" :help "Start a new workflow" :label "New Workflow")
  (lispm-comfyui-open :key "o" :help "Open a workflow of the server's library"
                      :label "Open…" :button "Open")
  (lispm-comfyui-open-file :key "O" :help "Open a workflow file" :label "Open File…")
  "---"
  (lispm-comfyui-save :key ("s" "C-x C-s") :help "Save the workflow in the server's library"
                      :label "Save" :button "Save")
  (lispm-comfyui-save-as :key ("S" "C-x C-w") :help "Save the workflow under another name"
                         :label "Save As…")
  "---"
  (lispm-comfyui-run :key ("r" "C-c C-c") :help "Run the workflow on the server"
                     :label "Run" :button "Run")
  (lispm-comfyui-stop :key ("x" "C-c C-k") :help "Stop the workflow running"
                      :label "Stop" :button "Stop" :enabled (lispm-comfyui--running-p))
  (lispm-comfyui-show-image :key "i" :help "Show the image made in a window" :label "Show Image")
  "---"
  (lispm-comfyui-reload :key "g" :help "Read the server's node types again"
                        :label "Read Node Types Again")
  (lispm-comfyui-settings :key "," :help "The ComfyUI server, the colours, the font"
                          :label "Settings…" :button "Settings")
  :menu "Edit"
  (lispm-comfyui-undo :key ("u" "C-/" "C-_" "C-x u") :help "Undo the last change" :label "Undo")
  (lispm-comfyui-redo :key ("U" "C-?" "M-_") :help "Redo the change undone" :label "Redo")
  "---"
  (lispm-comfyui-add-node :key "a" :help "Add a node, linked to the socket chosen"
                          :label "Add Node…" :button "Add Node")
  (lispm-comfyui-edit-value :key "e" :help "Set a value of the node" :label "Set Value…"
                            :enabled (lispm-comfyui--chosen-p))
  (lispm-comfyui-connect :key "c" :help "Link the socket chosen" :label "Link…"
                         :enabled (lispm-comfyui--chosen-p))
  (lispm-comfyui-disconnect :key "k" :help "Unlink the socket chosen" :label "Unlink"
                            :enabled (lispm-comfyui--chosen-p))
  (lispm-comfyui-rename-node :key "t" :help "Give the node a title" :label "Rename Node…"
                             :enabled (lispm-comfyui--chosen-p))
  (lispm-comfyui-duplicate-node :key "D" :help "Copy the node" :label "Duplicate Node"
                                :enabled (lispm-comfyui--chosen-p))
  (lispm-comfyui-bypass :key "B" :help "Bypass the node, or no longer" :label "Bypass Node"
                        :enabled (lispm-comfyui--chosen-p))
  (lispm-comfyui-mute :key "M" :help "Mute the node, or no longer" :label "Mute Node"
                      :enabled (lispm-comfyui--chosen-p))
  (lispm-comfyui-delete-node :key ("d" "<delete>" "<deletechar>") :help "Delete the node"
                             :label "Delete Node" :enabled (lispm-comfyui--chosen-p))
  :menu "Subgraph"
  (lispm-comfyui-enter-subgraph :key ("C-M-d" ">") :help "Show the inside of the subgraph's node"
                                :label "Enter Subgraph" :enabled (lispm-comfyui--subgraph-node-p))
  (lispm-comfyui-leave-subgraph :key ("C-M-u" "<" "^") :help "Show what the subgraph is inside"
                                :label "Leave Subgraph" :enabled lispm-comfyui--path)
  "---"
  (lispm-comfyui-mark :key "m" :help "Mark the node, or no longer" :label "Mark Node"
                      :enabled (lispm-comfyui--chosen-p))
  (lispm-comfyui-unmark-all :key "M-m" :help "Mark no node" :label "Unmark All"
                            :enabled lispm-comfyui--marked)
  (lispm-comfyui-make-subgraph :key "G" :help "Make a subgraph of the nodes marked"
                               :label "Make Subgraph…" :enabled (lispm-comfyui--chosen-p))
  (lispm-comfyui-unpack-subgraph :key "X" :help "Put the nodes inside the subgraph in its place"
                                 :label "Unpack Subgraph" :enabled (lispm-comfyui--subgraph-node-p))
  :menu "View"
  (lispm-comfyui-show-workflows :key "w" :help "Go to the workflows on the left"
                                :label "Show Workflows")
  (lispm-comfyui-go-to-node :key ("j" "/") :help "Go to a node by its title" :label "Go to Node…")
  (lispm-comfyui-next-node :key "TAB" :help "Choose the next node" :label "Next Node")
  (lispm-comfyui-previous-node :key "<backtab>" :help "Choose the previous node" :label "Previous Node")
  "---"
  (lispm-comfyui-zoom-in :key ("+" "=") :help "Draw larger" :label "Zoom In")
  (lispm-comfyui-zoom-out :key "-" :help "Draw smaller" :label "Zoom Out")
  (lispm-comfyui-fit :key ("0" "z") :help "Show the whole workflow" :label "Show All")
  (lispm-comfyui-show-selected :key "." :help "Show the node chosen in the middle"
                               :label "Show Chosen Node"))

(define-derived-mode lispm-comfyui-mode special-mode "ComfyUI"
  "A ComfyUI workflow, drawn as ComfyUI's frontend draws it.
The arrow keys go from node to node; n and p from item to item of the
node chosen, its inputs, outputs and widgets; RET does what the item
calls for.  The wheel zooms about the pointer; Shift-wheel and sideways
wheel turns pan the canvas, as dragging it does.
w goes to the workflows on the left, and back.
\\{lispm-comfyui-mode-map}"
  (let ((graph (lispm-comfyui-graph-create)))
    (setq-local lispm-comfyui--root graph
                lispm-comfyui--graph graph))
  (setq buffer-read-only t
        truncate-lines t
        cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil
              line-spacing nil
              auto-hscroll-mode nil
              scroll-margin 0
              auto-window-vscroll nil
              global-hl-line-mode nil
              vertical-scroll-bar nil
              horizontal-scroll-bar nil
              left-fringe-width 0
              right-fringe-width 0
              header-line-format '(:eval (lispm-comfyui--header))
              revert-buffer-function #'lispm-comfyui-reload
              mwheel-scroll-up-function #'lispm-comfyui-wheel-down
              mwheel-scroll-down-function #'lispm-comfyui-wheel-up
              mwheel-scroll-left-function #'lispm-comfyui-wheel-right
              mwheel-scroll-right-function #'lispm-comfyui-wheel-left)
  ;; The canvas owns wheel events even when precise text scrolling is on.
  (setq-local minor-mode-overriding-map-alist
              (cons (cons 'pixel-scroll-precision-mode lispm-comfyui-mode-map)
                    minor-mode-overriding-map-alist))
  (add-hook 'window-size-change-functions #'lispm-comfyui--window-changed nil t)
  (add-hook 'window-buffer-change-functions #'lispm-comfyui--window-changed nil t)
  (add-hook 'kill-buffer-query-functions #'lispm-comfyui--may-kill nil t)
  (unless (lispm-comfyui-cached-types) (lispm-comfyui--load-types)))

(defun lispm-comfyui--may-kill ()
  "Non-nil if this buffer may go: its workflow saved, or let go."
  (or (not lispm-comfyui--modified)
      (null (lispm-comfyui-graph-nodes lispm-comfyui--root))
      (yes-or-no-p (format "The workflow %s changed since it was saved; close it anyway? "
                           (or lispm-comfyui--name "Untitled")))))

;;;###autoload
(defun lispm-comfyui ()
  "Show ComfyUI's workflows: the one last shown, or a new one."
  (interactive)
  (when (and (derived-mode-p 'lispm-comfyui-workflows-mode)
             (window-live-p lispm-comfyui--workflows-main))
    (select-window lispm-comfyui--workflows-main))
  (if-let* ((buffer (car (lispm-comfyui--canvas-buffers))))
      (progn (pop-to-buffer-same-window buffer)
             (lispm-comfyui--workflows-ensure t)
             (lispm-comfyui--draw))
    (lispm-comfyui-new)))

(provide 'lispm-comfyui)

;; LispM's agents get their tools once LispM's tools are there.
(with-eval-after-load 'lispm-tools
  (require 'lispm-comfyui-tools))
;;; lispm-comfyui.el ends here
