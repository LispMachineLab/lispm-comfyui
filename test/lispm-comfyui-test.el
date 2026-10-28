;;; lispm-comfyui-test.el --- Tests of lispm-comfyui  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

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

;; The workflow of test/workflow.json is ComfyUI's first one, a model
;; loaded, a prompt encoded, a latent sampled, decoded and saved, with
;; a reroute, a bypassed node and a note besides.  test/object-info.json
;; holds what a server says of those node types, and of a node whose
;; format brings widgets, as Video Helper Suite's do.  No test speaks
;; to a server.

;;; Code:

(require 'ert)
(require 'lispm-comfyui)

(defconst lispm-comfyui-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun lispm-comfyui-test--json (name)
  "The JSON of the file NAME of the tests' directory, read."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name lispm-comfyui-test--directory))
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist)))

(defun lispm-comfyui-test--types ()
  "The node types of test/object-info.json."
  (lispm-comfyui-types (lispm-comfyui-test--json "object-info.json")))

(defun lispm-comfyui-test--graph ()
  "The graph of test/workflow.json."
  (lispm-comfyui-graph-from-json (lispm-comfyui-test--json "workflow.json")))

(defun lispm-comfyui-test--normal (value)
  "VALUE, a prompt or part of one, with objects sorted, for comparing.
Whole numbers are integers, as JSON does not tell 8 from 8.0."
  (cond ((and (floatp value) (= value (ffloor value))) (truncate value))
        ((vectorp value) (mapcar #'lispm-comfyui-test--normal value))
        ((and (consp value) (consp (car value)) (symbolp (caar value)))
         (sort (mapcar (lambda (entry) (cons (car entry) (lispm-comfyui-test--normal (cdr entry))))
                       value)
               (lambda (a b) (string< (car a) (car b)))))
        (t value)))

(defmacro lispm-comfyui-test--with-canvas (&rest body)
  "Run BODY in a buffer showing the test workflow, its types known."
  (declare (indent 0))
  `(let ((lispm-comfyui-url "http://comfy.test:8188")
         (lispm-comfyui--types (make-hash-table :test #'equal)))
     (puthash "http://comfy.test:8188" (lispm-comfyui-test--types) lispm-comfyui--types)
     (with-temp-buffer
       (lispm-comfyui-mode)
       (lispm-comfyui--set-root (lispm-comfyui-test--graph))
       (lispm-comfyui--relayout)
       ,@body)))

;;;; Widgets

(ert-deftest lispm-comfyui-test-widgets-named ()
  "A node's values are named after its type's inputs; a seed has a control."
  (let* ((types (lispm-comfyui-test--types))
         (node (lispm-comfyui-node (lispm-comfyui-test--graph) 3))
         (alist (lispm-comfyui-node-widget-alist node types)))
    (should (equal (mapcar #'car alist)
                   '("seed" "seed/control" "steps" "cfg" "sampler_name" "scheduler" "denoise")))
    (should (equal (cdr (assoc "seed" alist)) 156680208700286))
    (should (equal (cdr (assoc "seed/control" alist)) "randomize"))
    (should (equal (cdr (assoc "scheduler" alist)) "normal"))))

(ert-deftest lispm-comfyui-test-dynamic-combo ()
  "A dynamic combo's choice brings widgets named after it, and changes them."
  (let* ((types (lispm-comfyui-test--types))
         (graph (lispm-comfyui-graph-create))
         (node (lispm-comfyui-graph-add-node graph types "SaveVideo"))
         (names (mapcar #'car (lispm-comfyui-node-widget-alist node types))))
    (should (member "format" names))
    (should (seq-find (lambda (name) (string-prefix-p "format." name)) names))
    (let* ((choices (lispm-comfyui-widget-choices
                     (plist-get (seq-find (lambda (widget) (equal (plist-get widget :name) "format"))
                                          (lispm-comfyui-node-widgets node types))
                                :spec)))
           (other (car (remove (cdr (assoc "format" (lispm-comfyui-node-widget-alist node types)))
                               choices))))
      (lispm-comfyui-set-widget node types "format" other)
      (should (equal (cdr (assoc "format" (lispm-comfyui-node-widget-alist node types))) other))
      ;; The values hold as many as the widgets of the new choice need.
      (should (= (length (lispm-comfyui-node-values node))
                 (lispm-comfyui--values-used (lispm-comfyui-node-widgets node types)))))))

(ert-deftest lispm-comfyui-test-format-widgets ()
  "Widgets a format brings, which a list of values has no room for, are
in the prompt with their defaults, and set by name."
  (let* ((types (lispm-comfyui-test--types))
         (graph (lispm-comfyui-graph-create))
         (node (lispm-comfyui-graph-add-node graph types "FormatsNode")))
    (lispm-comfyui-set-widget node types "format" "video/h264-mp4")
    (let ((inputs (alist-get 'inputs (cdar (lispm-comfyui-prompt graph types)))))
      (should (equal (alist-get 'crf inputs) 19))
      (should (equal (alist-get 'pix_fmt inputs) "yuv420p")))
    (lispm-comfyui-set-widget node types "crf" 23)
    (should (lispm-comfyui-values-by-name-p (lispm-comfyui-node-values node)))
    (let ((inputs (alist-get 'inputs (cdar (lispm-comfyui-prompt graph types)))))
      (should (equal (alist-get 'crf inputs) 23))
      (should (equal (alist-get 'format inputs) "video/h264-mp4")))))

(ert-deftest lispm-comfyui-test-check-value ()
  "Values are kept within bounds, and choices among the choices."
  (let* ((types (lispm-comfyui-test--types))
         (node (lispm-comfyui-node (lispm-comfyui-test--graph) 3)))
    (should (equal (lispm-comfyui-set-widget node types "steps" 0) 1))
    (should (equal (lispm-comfyui-set-widget node types "steps" "30") 30))
    (should (equal (lispm-comfyui-set-widget node types "cfg" 7) 7.0))
    (should-error (lispm-comfyui-set-widget node types "sampler_name" "no such sampler")
                  :type 'lispm-comfyui-error)
    (should-error (lispm-comfyui-set-widget node types "seed/control" "sometimes")
                  :type 'lispm-comfyui-error)
    (should-error (lispm-comfyui-set-widget node types "no_such_widget" 1)
                  :type 'lispm-comfyui-error)
    (should (equal (cdr (assoc "steps" (lispm-comfyui-node-widget-alist node types))) 30))))

;;;; Prompts

(ert-deftest lispm-comfyui-test-prompt ()
  "A workflow makes the prompt ComfyUI's frontend makes of it.
Through the reroute and the bypassed node; the note does not run."
  (let ((prompt (lispm-comfyui-prompt (lispm-comfyui-test--graph) (lispm-comfyui-test--types))))
    (should (equal (sort (mapcar (lambda (entry) (symbol-name (car entry))) prompt) #'string<)
                   '("12" "3" "4" "5" "6" "7" "8" "9")))
    (should (equal (lispm-comfyui-test--normal (alist-get 'inputs (alist-get '\3 prompt)))
                   '((cfg . 8) (denoise . 1) (latent_image "5" 0) (model "4" 0) (negative "7" 0)
                     (positive "6" 0) (sampler_name . "euler") (scheduler . "normal")
                     (seed . 156680208700286) (steps . 20))))
    (should (equal (lispm-comfyui-test--normal (alist-get 'inputs (alist-get '\8 prompt)))
                   '((samples "3" 0) (vae "4" 2))))
    (should (equal (lispm-comfyui-test--normal (alist-get 'inputs (alist-get '\12 prompt)))
                   '((images "8" 0))))
    (should (equal (alist-get 'class_type (alist-get '\6 prompt)) "CLIPTextEncode"))
    (should (equal (alist-get 'title (alist-get '_meta (alist-get '\6 prompt))) "Positive"))))

(ert-deftest lispm-comfyui-test-prompt-muted ()
  "A muted node does not run, and what it fed is fed by nothing."
  (let* ((types (lispm-comfyui-test--types))
         (graph (lispm-comfyui-test--graph)))
    (setf (lispm-comfyui-node-mode (lispm-comfyui-node graph 8)) 2)
    (let ((prompt (lispm-comfyui-prompt graph types)))
      (should-not (alist-get '\8 prompt))
      (should-not (alist-get 'images (alist-get 'inputs (alist-get '\9 prompt)))))))

(ert-deftest lispm-comfyui-test-prompt-unknown-type ()
  "A node of a type the server has not stops the prompt, saying which."
  (let ((graph (lispm-comfyui-test--graph)))
    (setf (lispm-comfyui-node-type (lispm-comfyui-node graph 5)) "NoSuchNode")
    (should (string-match-p "#5.*NoSuchNode"
                            (cadr (should-error (lispm-comfyui-prompt graph (lispm-comfyui-test--types))
                                                :type 'lispm-comfyui-error))))))

(ert-deftest lispm-comfyui-test-round-trip ()
  "A workflow written and read again is the same; what is not used is kept."
  (let* ((graph (lispm-comfyui-test--graph))
         (again (lispm-comfyui-graph-from-string (lispm-comfyui-graph-string graph)))
         (json (lispm-comfyui-graph-json again)))
    (should (equal (lispm-comfyui-test--normal (lispm-comfyui-graph-json graph))
                   (lispm-comfyui-test--normal json)))
    (should (equal (alist-get 'color (seq-find (lambda (node) (equal (alist-get 'id node) 13))
                                               (alist-get 'nodes json)))
                   "#432"))
    (should (equal (alist-get 'ds (alist-get 'extra json)) '((scale . 1) (offset . [0 0]))))
    (should (equal (aref (alist-get 'links json) 0) [1 4 0 3 0 "MODEL"]))))

;;;; Editing

(ert-deftest lispm-comfyui-test-connect ()
  "Links join outputs and inputs of one type, replacing the one before."
  (let* ((types (lispm-comfyui-test--types))
         (graph (lispm-comfyui-test--graph))
         (loader (lispm-comfyui-node graph 4))
         (sampler (lispm-comfyui-node graph 3))
         (latent (lispm-comfyui-graph-add-node graph types "EmptyLatentImage")))
    (should-error (lispm-comfyui-graph-connect graph types loader "CLIP" sampler "model")
                  :type 'lispm-comfyui-error)
    (let ((links (length (lispm-comfyui-graph-links graph))))
      (lispm-comfyui-graph-connect graph types latent "LATENT" sampler "latent_image")
      (should (= links (length (lispm-comfyui-graph-links graph)))))
    (should (equal (lispm-comfyui-link-from (lispm-comfyui-input-link graph sampler 3))
                   (lispm-comfyui-node-id latent)))
    ;; A widget fed by a link gets an input, and the prompt the link.
    (let ((steps (lispm-comfyui-graph-add-node graph types "EmptyLatentImage")))
      (should-error (lispm-comfyui-graph-connect graph types steps 0 sampler "steps")
                    :type 'lispm-comfyui-error))))

(ert-deftest lispm-comfyui-test-remove-and-disconnect ()
  "Removing a node removes its links; unlinking an input removes one."
  (let* ((graph (lispm-comfyui-test--graph))
         (sampler (lispm-comfyui-node graph 3)))
    (lispm-comfyui-graph-disconnect graph sampler 0)
    (should-not (lispm-comfyui-input-link graph sampler 0))
    (lispm-comfyui-remove-node graph sampler)
    (should-not (lispm-comfyui-node graph 3))
    (should-not (seq-find (lambda (link) (or (eql (lispm-comfyui-link-from link) 3)
                                             (eql (lispm-comfyui-link-to link) 3)))
                          (lispm-comfyui-graph-links graph)))))

(ert-deftest lispm-comfyui-test-new-node ()
  "A new node has its type's sockets and its widgets' defaults."
  (let* ((types (lispm-comfyui-test--types))
         (graph (lispm-comfyui-test--graph))
         (node (lispm-comfyui-graph-add-node graph types "KSampler")))
    (should (= (lispm-comfyui-node-id node) 14))
    (should (equal (mapcar #'lispm-comfyui-slot-name (lispm-comfyui-node-inputs node))
                   '("model" "positive" "negative" "latent_image")))
    (should (equal (mapcar #'lispm-comfyui-slot-name (lispm-comfyui-node-outputs node)) '("LATENT")))
    (should (equal (cdr (assoc "steps" (lispm-comfyui-node-widget-alist node types))) 20))
    (should (equal (cdr (assoc "seed/control" (lispm-comfyui-node-widget-alist node types)))
                   "randomize"))))

(ert-deftest lispm-comfyui-test-after-run ()
  "After a run, a seed set to randomize changes, within its bounds; a fixed one stays."
  (let* ((types (lispm-comfyui-test--types))
         (node (lispm-comfyui-node (lispm-comfyui-test--graph) 3)))
    (should (lispm-comfyui-after-run node types))
    (let ((seed (cdr (assoc "seed" (lispm-comfyui-node-widget-alist node types)))))
      (should (integerp seed))
      (should (>= seed 0)))
    (lispm-comfyui-set-widget node types "seed/control" "fixed")
    (should-not (lispm-comfyui-after-run node types))
    (lispm-comfyui-set-widget node types "seed/control" "increment")
    (let ((seed (cdr (assoc "seed" (lispm-comfyui-node-widget-alist node types)))))
      (lispm-comfyui-after-run node types)
      (should (equal (cdr (assoc "seed" (lispm-comfyui-node-widget-alist node types))) (1+ seed))))))

(ert-deftest lispm-comfyui-test-from-prompt ()
  "A prompt in the API's form becomes a graph laid out left to right."
  (let* ((types (lispm-comfyui-test--types))
         (prompt (json-parse-string
                  (json-serialize (lispm-comfyui-prompt (lispm-comfyui-test--graph) types))
                  :object-type 'alist))
         (graph (lispm-comfyui-graph-from-prompt prompt types)))
    (should (equal (lispm-comfyui-test--normal (lispm-comfyui-prompt graph types))
                   (lispm-comfyui-test--normal (lispm-comfyui-prompt (lispm-comfyui-test--graph) types))))
    (should (< (lispm-comfyui-node-x (lispm-comfyui-node graph 4))
               (lispm-comfyui-node-x (lispm-comfyui-node graph 3))
               (lispm-comfyui-node-x (lispm-comfyui-node graph 8))))))

(ert-deftest lispm-comfyui-test-upload-chooses-in-lispm-ui ()
  "Uploading an image chooses it in LispM UI's file dialog, which shows the
PNG, JPEG and WebP pictures and the one selected; the image uploaded is
then the one the widget loads.  Choosing none uploads nothing."
  (lispm-comfyui-test--with-canvas
    (let* ((node (lispm-comfyui-graph-add-node lispm-comfyui--graph (lispm-comfyui--types) "LoadImage"))
           (chosen nil) (dialogs nil) (uploaded nil)
           (lispm-ui-choose-file-function (lambda (&rest keys) (push keys dialogs) chosen)))
      (cl-letf (((symbol-function 'lispm-comfyui-upload-image)
                 (lambda (file callback)
                   (push file uploaded)
                   (funcall callback '((name . "photo.jpg") (subfolder . "")) nil)))
                ((symbol-function 'lispm-comfyui-types-of-server)
                 (lambda (callback &optional _again) (funcall callback (lispm-comfyui--types) nil))))
        (lispm-comfyui--upload node "image")
        (should-not uploaded)
        (let ((dialog (car dialogs)))
          (should (equal (plist-get dialog :title) "Upload an Image"))
          (should (plist-get dialog :preview))
          (should (funcall (plist-get dialog :predicate) "/tmp/photo.jpg"))
          (should (funcall (plist-get dialog :predicate) "/tmp/Cat.PNG"))
          (should (funcall (plist-get dialog :predicate) "/tmp/dog.webp"))
          ;; As ComfyUI's frontend, no other picture.
          (should-not (funcall (plist-get dialog :predicate) "/tmp/logo.svg"))
          (should-not (funcall (plist-get dialog :predicate) "/tmp/notes.txt"))
          (should-not (funcall (plist-get dialog :predicate) "/tmp/README")))
        (setq chosen "/tmp/photo.jpg")
        (lispm-comfyui--upload node "image")
        (should (equal uploaded '("/tmp/photo.jpg")))
        (should (equal (cdr (assoc "image" (lispm-comfyui-node-widget-alist node (lispm-comfyui--types))))
                       "photo.jpg"))))))

;;;; The canvas

(ert-deftest lispm-comfyui-test-canvas ()
  "The canvas draws each node, escapes what it says, and finds what is where."
  (let* ((types (lispm-comfyui-test--types))
         (graph (lispm-comfyui-test--graph))
         (layout (lispm-comfyui-canvas-layout graph types))
         (svg (lispm-comfyui-canvas-svg
               (list :graph graph :types types :layout layout :view '(0 0 1.0)
                     :selected 3 :focus 1)
               1200 800)))
    (should (string-prefix-p "<svg" svg))
    (should (string-search ">KSampler<" svg))
    (should (string-search "cat &amp; a dog &lt;in" svg))
    (should (string-search "Sampling" svg))
    (with-temp-buffer
      (insert svg)
      (should (libxml-parse-xml-region (point-min) (point-max))))
    (let* ((place (lispm-comfyui-canvas-place layout 3))
           (item (nth 3 (plist-get place :items)))
           (rect (plist-get item :rect)))
      (should (equal (lispm-comfyui-canvas-hit layout (1+ (nth 0 rect)) (1+ (nth 1 rect)))
                     (cons 3 3))))
    (should-not (lispm-comfyui-canvas-hit layout -5000 -5000))))

(ert-deftest lispm-comfyui-test-wrap ()
  "Text is broken into lines that fit, the last cut with an ellipsis."
  (let ((lines (lispm-comfyui-canvas--wrap (make-string 400 ?a) 200 3)))
    (should (= (length lines) 3))
    (should (string-suffix-p "…" (nth 2 lines)))
    (should-not (string-suffix-p "……" (nth 2 lines)))))

;;;; The window

(ert-deftest lispm-comfyui-test-undo ()
  "Each change is a step to undo and redo."
  (lispm-comfyui-test--with-canvas
    (let ((node (lispm-comfyui-node lispm-comfyui--graph 3)))
      (lispm-comfyui--set node "steps" 33)
      (should lispm-comfyui--modified)
      (lispm-comfyui-undo)
      (should (equal (cdr (assoc "steps" (lispm-comfyui-node-widget-alist
                                          (lispm-comfyui-node lispm-comfyui--graph 3)
                                          (lispm-comfyui--types))))
                     20))
      (lispm-comfyui-redo)
      (should (equal (cdr (assoc "steps" (lispm-comfyui-node-widget-alist
                                          (lispm-comfyui-node lispm-comfyui--graph 3)
                                          (lispm-comfyui--types))))
                     33))
      ;; A change that fails leaves the graph as it was.
      (should-error (lispm-comfyui--change
                      (lispm-comfyui-remove-node lispm-comfyui--graph
                                                 (lispm-comfyui-node lispm-comfyui--graph 4))
                      (error "No")))
      (should (lispm-comfyui-node lispm-comfyui--graph 4)))))

(ert-deftest lispm-comfyui-test-navigation ()
  "The arrows choose the nearest node the way they point."
  (lispm-comfyui-test--with-canvas
    (lispm-comfyui--select 3)
    (lispm-comfyui-right)
    (should (equal lispm-comfyui--selected 8))
    (lispm-comfyui-left)
    (should (equal lispm-comfyui--selected 3))
    (lispm-comfyui-left)
    (should (memql lispm-comfyui--selected '(6 7)))
    (lispm-comfyui-next-item)
    (should (eq (plist-get (lispm-comfyui--item) :kind) 'input))))

(ert-deftest lispm-comfyui-test-run-messages ()
  "What the server's WebSocket says of a run lights its nodes and brings its images."
  (lispm-comfyui-test--with-canvas
    (let* ((image (make-temp-file "lispm-comfyui-test" nil ".png"))
           (ended nil)
           (run (list :id "p1" :status 'queued :start (current-time) :pending 0
                      :callbacks (list (lambda (run) (setq ended run))))))
      (unwind-protect
          (cl-letf (((symbol-function 'lispm-comfyui-fetch-image)
                     (lambda (_image callback) (funcall callback image nil))))
            (push run lispm-comfyui--runs)
            (lispm-comfyui--on-message "executing" '((node . "3") (prompt_id . "p1")))
            (should (equal lispm-comfyui--executing "3"))
            (should (eq (plist-get run :status) 'running))
            (lispm-comfyui--on-message "progress" '((value . 5) (max . 20) (node . "3") (prompt_id . "p1")))
            (should (equal lispm-comfyui--progress '(5 . 20)))
            (lispm-comfyui--on-message
             "executed" `((node . "9") (prompt_id . "p1")
                          (output . ((images . [((filename . "ComfyUI_00001_.png") (subfolder . "")
                                                 (type . "output"))])))))
            (should (equal (cdr (assoc "9" lispm-comfyui--images)) (list image)))
            (should-not ended)
            (lispm-comfyui--on-message "execution_success" '((prompt_id . "p1")))
            (should (eq (plist-get ended :status) 'done))
            (should (equal (plist-get ended :files) (list image)))
            (should-not lispm-comfyui--executing))
        (delete-file image)))))

(ert-deftest lispm-comfyui-test-run-error ()
  "A node failing is marked with what went wrong."
  (lispm-comfyui-test--with-canvas
    (let ((run (list :id "p2" :status 'running :start (current-time) :pending 0)))
      (push run lispm-comfyui--runs)
      (lispm-comfyui--on-message "execution_error"
                                 '((prompt_id . "p2") (node_id . "3")
                                   (exception_message . "Out of memory\n")))
      (should (eq (plist-get run :status) 'failed))
      (should (equal (cdr (assoc "3" lispm-comfyui--errors)) "Out of memory")))))

;;;; Dynamic prompts

(ert-deftest lispm-comfyui-test-dynamic-prompt ()
  "Choices are made as ComfyUI's frontend makes them, given the same draws.
The texts wanted are what the frontend's processDynamicPrompt gave for
the same draws."
  (let ((draws '(0.7 0.1 0.9 0.4 0.55 0.05 0.95 0.3)))
    (pcase-dolist (`(,text . ,wanted)
                   '(("a {red|green|blue} apple" . "a blue apple")
                     ("{x|{y|z}} and {p|q}" . "y and q")
                     ("keep \\{braces\\} and \\| bars" . "keep {braces} and | bars")
                     ("line // comment\nnext" . "line \nnext")
                     ("/* block\ncomment */start {a|b}" . "start b")
                     ("weights (red:1.2) \\(lit\\)" . "weights (red:1.2) \\(lit\\)")
                     ("{|empty|} end" . " end")
                     ("unclosed {a|b" . "unclosed b")
                     ("nested {a{b|c}d|e}" . "nested e")
                     ("{ {a|b} | {c|d} }" . " c ")))
      (let ((k 0))
        (cl-letf (((symbol-function 'random)
                   (lambda (n) (prog1 (floor (* (nth (% k (length draws)) draws) n)) (cl-incf k)))))
          (should (equal (lispm-comfyui-dynamic-prompt text) wanted)))))))

(ert-deftest lispm-comfyui-test-dynamic-prompt-in-prompt ()
  "A text of dynamic prompts is chosen in the prompt, and told back."
  (let* ((types (lispm-comfyui-test--types))
         (graph (lispm-comfyui-test--graph))
         (resolved (make-hash-table)))
    (lispm-comfyui-set-widget (lispm-comfyui-node graph 6) types "text" "a {cat|cat} on a mat")
    (let ((prompt (lispm-comfyui-prompt graph types resolved)))
      (should (equal (alist-get 'text (alist-get 'inputs (alist-get '\6 prompt))) "a cat on a mat"))
      (should (equal (gethash 6 resolved) '(("text" . "a cat on a mat")))))))

;;;; Widgets as the frontend makes them

(ert-deftest lispm-comfyui-test-widget-type-and-seed ()
  "An input of several types is the widget its widgetType names; an
integer named seed has a control without its type saying so, one inside
a dynamic combo not."
  (let* ((types (lispm-comfyui-test--types))
         (widgets (lispm-comfyui-widgets (gethash "MixedNode" types) '(97 25.0 5 "fixed"))))
    (should (equal (mapcar (lambda (widget) (plist-get widget :name)) widgets)
                   '("frames" "frame_rate" "seed")))
    (should (eql (plist-get (nth 2 widgets) :control) 3))
    (should (equal (lispm-comfyui-widget-control (nth 2 widgets) '(97 25.0 5 "fixed")) "fixed"))))

(ert-deftest lispm-comfyui-test-stale-link ()
  "Of two links into one input, the one the input names is kept."
  (let* ((json (lispm-comfyui-test--json "workflow.json"))
         (links (alist-get 'links json)))
    (setf (alist-get 'links json) (vconcat links (vector [99 7 0 3 1 "CONDITIONING"])))
    (let ((graph (lispm-comfyui-graph-from-json json)))
      (should (equal (lispm-comfyui-link-from (lispm-comfyui-input-link graph (lispm-comfyui-node graph 3) 1))
                     6)))))

;;;; Subgraphs

(defun lispm-comfyui-test--subgraphed ()
  "The test workflow, its sampler and decoder made a subgraph: (GRAPH . NODE)."
  (let* ((types (lispm-comfyui-test--types))
         (graph (lispm-comfyui-test--graph))
         (node (lispm-comfyui-graph-convert-to-subgraph
                graph types (list (lispm-comfyui-node graph 3) (lispm-comfyui-node graph 8)) "Sample")))
    (cons graph node)))

(defun lispm-comfyui-test--renamed (prompt map)
  "PROMPT, its numbers as MAP, an alist of strings, says, for comparing."
  (lispm-comfyui-test--normal
   (mapcar (lambda (entry)
             (let ((rename (lambda (id) (or (cdr (assoc id map)) id))))
               (cons (intern (funcall rename (symbol-name (car entry))))
                     (list (cons 'inputs
                                 (mapcar (lambda (input)
                                           (let ((value (cdr input)))
                                             (cons (car input)
                                                   (if (and (vectorp value) (= (length value) 2) (stringp (aref value 0)))
                                                       (vector (funcall rename (aref value 0)) (aref value 1))
                                                     value))))
                                         (alist-get 'inputs (cdr entry))))
                           (assq 'class_type (cdr entry))))))
           prompt)))

(ert-deftest lispm-comfyui-test-subgraph-prompt ()
  "A subgraph's nodes run as the nodes inside, numbered with its node's."
  (let* ((types (lispm-comfyui-test--types))
         (before (lispm-comfyui-prompt (lispm-comfyui-test--graph) types))
         (made (lispm-comfyui-test--subgraphed))
         (graph (car made))
         (id (format "%s" (lispm-comfyui-node-id (cdr made))))
         (after (lispm-comfyui-prompt graph types)))
    (should (assq (intern (concat id ":3")) after))
    (should (assq (intern (concat id ":8")) after))
    (should-not (assq (intern id) after))
    (should (equal (lispm-comfyui-test--renamed after `((,(concat id ":3") . "3") (,(concat id ":8") . "8")))
                   (lispm-comfyui-test--renamed before nil)))
    ;; Saved and read again, it runs the same.
    (let ((again (lispm-comfyui-graph-from-string (lispm-comfyui-graph-string graph) types)))
      (should (equal (lispm-comfyui-test--normal (lispm-comfyui-prompt again types))
                     (lispm-comfyui-test--normal after))))))

(ert-deftest lispm-comfyui-test-subgraph-unpack ()
  "Unpacking a subgraph's node gives back the nodes, linked as before."
  (let* ((types (lispm-comfyui-test--types))
         (before (lispm-comfyui-prompt (lispm-comfyui-test--graph) types))
         (made (lispm-comfyui-test--subgraphed))
         (graph (car made))
         (copies (lispm-comfyui-graph-unpack graph types (cdr made)))
         (map (mapcar (lambda (copy)
                        (cons (format "%s" (lispm-comfyui-node-id copy))
                              (if (equal (lispm-comfyui-node-type copy) "KSampler") "3" "8")))
                      copies)))
    (should (= (length copies) 2))
    (should (equal (lispm-comfyui-test--renamed (lispm-comfyui-prompt graph types) map)
                   (lispm-comfyui-test--renamed before nil)))))

(ert-deftest lispm-comfyui-test-subgraph-promoted ()
  "An input of a subgraph linked to a widget inside is a widget of its
nodes, each holding its own value, given to the widget inside."
  (let* ((types (lispm-comfyui-test--types))
         (made (lispm-comfyui-test--subgraphed))
         (graph (car made))
         (node (cdr made))
         (subgraph (lispm-comfyui-node-subgraph graph node))
         (inside (lispm-comfyui-subgraph-graph subgraph))
         (index (lispm-comfyui-subgraph-add-slot graph subgraph 'input "steps" "INT")))
    (lispm-comfyui-graph-connect inside types (lispm-comfyui-subgraph-io-node subgraph 'input) index
                                 (lispm-comfyui-node inside 3) "steps")
    (lispm-comfyui-align-instances graph types)
    (let ((merged (lispm-comfyui-graph-types graph types)))
      (should (equal (cdr (assoc "steps" (lispm-comfyui-node-widget-alist node merged))) 20))
      (lispm-comfyui-set-widget node merged "steps" 33)
      (let ((prompt (lispm-comfyui-prompt graph types)))
        (should (equal (alist-get 'steps (alist-get 'inputs (alist-get (intern (format "%s:3" (lispm-comfyui-node-id node)))
                                                                         prompt)))
                       33)))
      ;; Saved, its value is the subgraph node's.
      (let* ((json (lispm-comfyui-graph-json graph))
             (saved (seq-find (lambda (entry) (equal (alist-get 'id entry) (lispm-comfyui-node-id node)))
                              (alist-get 'nodes json))))
        (should (equal (alist-get 'widgets_values saved) [33]))))))

(ert-deftest lispm-comfyui-test-subgraph-nested ()
  "A subgraph inside another runs with the path of both nodes."
  (let* ((types (lispm-comfyui-test--types))
         (made (lispm-comfyui-test--subgraphed))
         (graph (car made))
         (outer (lispm-comfyui-graph-convert-to-subgraph
                 graph types (list (cdr made) (lispm-comfyui-node graph 9)) "Outer"))
         (prompt (lispm-comfyui-prompt graph types)))
    (should (seq-find (lambda (entry) (string-match-p (format "\\`%s:[0-9]+:3\\'" (lispm-comfyui-node-id outer))
                                                      (symbol-name (car entry))))
                      prompt))
    (should (= (length prompt) 8))))

(ert-deftest lispm-comfyui-test-subgraph-legacy-and-renumber ()
  "A workflow saved by an older frontend reads as the frontend reads it:
inner nodes numbered as outer ones renumbered, proxyWidgets made inputs."
  (let* ((types (lispm-comfyui-test--types))
         (made (lispm-comfyui-test--subgraphed))
         (json (lispm-comfyui-graph-json (car made)))
         (id (lispm-comfyui-node-id (cdr made)))
         (definition (aref (alist-get 'subgraphs (alist-get 'definitions json)) 0))
         (instance (seq-find (lambda (entry) (equal (alist-get 'id entry) id)) (alist-get 'nodes json))))
    ;; The sampler inside numbered as the checkpoint loader outside.
    (dolist (node (append (alist-get 'nodes definition) nil))
      (when (equal (alist-get 'id node) 3) (setf (alist-get 'id node) 4)))
    (dolist (link (append (alist-get 'links definition) nil))
      (when (equal (alist-get 'origin_id link) 3) (setf (alist-get 'origin_id link) 4))
      (when (equal (alist-get 'target_id link) 3) (setf (alist-get 'target_id link) 4)))
    (setf (alist-get 'properties instance) '((proxyWidgets . [["4" "steps"]]))
          (alist-get 'widgets_values instance) [42])
    (let* ((graph (lispm-comfyui-graph-from-json json types))
           (prompt (lispm-comfyui-prompt graph types))
           (sampler (seq-find (lambda (entry) (equal (alist-get 'class_type (cdr entry)) "KSampler")) prompt)))
      (should-not (equal (car sampler) (intern (format "%s:4" id))))
      (should (equal (alist-get 'steps (alist-get 'inputs (cdr sampler))) 42))
      (should (equal (alist-get 'model (alist-get 'inputs (cdr sampler))) ["4" 0]))
      (should-not (alist-get 'proxyWidgets (alist-get 'properties (lispm-comfyui-node-extra
                                                                   (lispm-comfyui-node graph id))))))))

(ert-deftest lispm-comfyui-test-subgraph-window ()
  "The window enters a subgraph, shows its inputs and outputs, and leaves."
  (lispm-comfyui-test--with-canvas
    (lispm-comfyui--select 3)
    (setq lispm-comfyui--marked (list 3 8))
    (lispm-comfyui-make-subgraph "Sample")
    (let ((id lispm-comfyui--selected))
      (should (lispm-comfyui-node-subgraph lispm-comfyui--root (lispm-comfyui-node lispm-comfyui--graph id)))
      (lispm-comfyui-enter-subgraph)
      (should (equal lispm-comfyui--path (list id)))
      (should (lispm-comfyui-node lispm-comfyui--graph 3))
      (should (lispm-comfyui-canvas-place lispm-comfyui--layout lispm-comfyui-input-node))
      (should (string-search "› Sample" (lispm-comfyui--header)))
      ;; A change inside is one step to undo, with the workflow.
      (lispm-comfyui--set (lispm-comfyui-node lispm-comfyui--graph 3) "steps" 9)
      (lispm-comfyui-undo)
      (should (equal (cdr (assoc "steps" (lispm-comfyui-node-widget-alist
                                          (lispm-comfyui-node lispm-comfyui--graph 3) (lispm-comfyui--types))))
                     20))
      (lispm-comfyui-leave-subgraph)
      (should-not lispm-comfyui--path)
      (should (equal lispm-comfyui--selected id))
      (lispm-comfyui-unpack-subgraph)
      (should (= (length (lispm-comfyui-graph-nodes lispm-comfyui--root)) 11)))))

;;;; Settings


(defmacro lispm-comfyui-test--with-settings (answer &rest body)
  "Run BODY with settings of its own, the server answering ANSWER.
ANSWER is what /system_stats answers, or a string, the error;
/object_info answers test/object-info.json.  What is saved is pushed
onto `saved' instead, the addresses asked onto `asked', and how the
workflows were drawn anew onto `redrawn'."
  (declare (indent 1))
  `(let ((lispm-comfyui-url "http://comfy.test:8188")
         (lispm-comfyui-dark t)
         (lispm-comfyui-font "Sans")
         (lispm-comfyui-settings--server nil)
         (saved nil)
         (asked nil)
         (redrawn nil))
     (cl-letf (((symbol-function 'lispm-comfyui-settings--save)
                (lambda (variable value) (set variable value) (push (cons variable value) saved)))
               ((symbol-function 'lispm-comfyui-request)
                (lambda (path callback &rest _)
                  (push (concat lispm-comfyui-url path) asked)
                  (cond ((equal path "/object_info")
                         (funcall callback (lispm-comfyui-test--json "object-info.json") nil))
                        ((stringp ,answer) (funcall callback nil ,answer))
                        (t (funcall callback ,answer nil)))))
               ((symbol-function 'lispm-comfyui-settings--redraw-workflows)
                (lambda (&optional again) (push (if again 'again 'drawn) redrawn)))
               ((symbol-function 'lispm-comfyui-close-socket) #'ignore))
       (unwind-protect (progn ,@body)
         (when-let* ((buffer (get-buffer (format "*%s*" lispm-comfyui-settings-title))))
           (kill-buffer buffer))))))

(defconst lispm-comfyui-test--stats
  '((system (comfyui_version . "0.37.0") (os . "linux"))
    (devices . [((name . "cuda:0 Tesla V100-SXM3-32GB : cudaMallocAsync") (vram_total . 34359738368))]))
  "What a server's /system_stats answers.")

(defun lispm-comfyui-test--form-text (key text)
  "Make TEXT what the field KEY of the form in this buffer holds, as typed."
  (let ((widget (alist-get key lispm-ui-form--widgets)))
    (delete-region (widget-field-start widget) (widget-field-text-end widget))
    (goto-char (widget-field-start widget))
    (insert text)))

(defun lispm-comfyui-test--form-press (key &optional nth)
  "Press the NTH (default first) control of the field KEY, as Return would."
  (let ((widgets nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((widget (widget-at (point))))
          (when (and (eq (widget-get widget :lispm-form-key) key) (not (memq widget widgets)))
            (push widget widgets)))
        (forward-char 1)))
    (widget-apply-action (nth (or nth 0) (nreverse widgets)))))

(ert-deftest lispm-comfyui-test-settings-form ()
  "The settings are a form: the address, the colours and the font, saved."
  (lispm-comfyui-test--with-settings lispm-comfyui-test--stats
    (let ((buffer (lispm-comfyui-settings)))
      (with-current-buffer buffer
        (should (derived-mode-p 'lispm-ui-form-mode))
        (should (equal (lispm-ui-form-value 'address) "http://comfy.test:8188"))
        (should (eq (lispm-ui-form-value 'colours) 'dark))
        (should (equal (lispm-ui-form-value 'font) "Sans"))
        (should (string-match-p "(\\*) Dark, as ComfyUI draws them\n.*( ) Emacs's own" (buffer-string)))
        ;; An address not a server's is refused.
        (lispm-comfyui-test--form-text 'address "ftp://comfy.lan")
        (should (equal (cadr (should-error (lispm-ui-form-submit) :type 'user-error))
                       "Address: it is http://HOST:PORT, as http://127.0.0.1:8188"))
        ;; One typed short is made whole.
        (lispm-comfyui-test--form-text 'address "comfy.lan:8188/")
        (lispm-comfyui-test--form-press 'colours 1)
        (should (eq (lispm-ui-form-value 'colours) 'emacs))
        (lispm-ui-form-submit))
      (should-not (buffer-live-p buffer))
      (should (equal lispm-comfyui-url "http://comfy.lan:8188"))
      (should-not lispm-comfyui-dark)
      (should (equal (reverse saved) '((lispm-comfyui-url . "http://comfy.lan:8188") (lispm-comfyui-dark))))
      ;; Another server's node types are read for the workflows.
      (should (equal redrawn '(again))))))

(ert-deftest lispm-comfyui-test-settings-cancel ()
  "Cancelled, the settings stay as they were."
  (lispm-comfyui-test--with-settings lispm-comfyui-test--stats
    (with-current-buffer (lispm-comfyui-settings)
      (lispm-comfyui-test--form-text 'font "Geneva")
      (lispm-comfyui-test--form-press 'colours 1)
      (lispm-ui-form-cancel))
    (should-not saved)
    (should-not redrawn)
    (should (equal lispm-comfyui-font "Sans"))))

(ert-deftest lispm-comfyui-test-settings-test-connection ()
  "The server is asked at once, and again at the address typed, said under it."
  (lispm-comfyui-test--with-settings lispm-comfyui-test--stats
    (let ((opened nil)
          (types (length (lispm-comfyui-test--json "object-info.json"))))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (push url opened))))
        (with-current-buffer (lispm-comfyui-settings)
          (should (member "http://comfy.test:8188/system_stats" asked))
          (should (string-search (format "● ComfyUI 0.37.0 · Tesla V100-SXM3-32GB, 32 GB · %d node types" types)
                                 (buffer-string)))
          (should (string-match-p "\\[Test Connection\\] \\[Open ComfyUI.s Page\\]" (buffer-string)))
          (lispm-comfyui-test--form-text 'address "comfy.lan:8188")
          (lispm-comfyui-test--form-press 'actions 0)
          (should (equal (car asked) "http://comfy.lan:8188/object_info"))
          (should (equal (plist-get lispm-comfyui-settings--server :url) "http://comfy.lan:8188"))
          (should (string-search "● ComfyUI 0.37.0" (buffer-string)))
          (lispm-comfyui-test--form-press 'actions 1)
          (should (equal opened '("http://comfy.lan:8188")))
          ;; Asking is not saving.
          (should-not saved)
          (should (equal lispm-comfyui-url "http://comfy.test:8188")))))))

(ert-deftest lispm-comfyui-test-settings-no-answer ()
  "A server not answering is said to, under its address."
  (lispm-comfyui-test--with-settings "nothing answers at comfy.test:8188"
    (with-current-buffer (lispm-comfyui-settings)
      (should (eq (plist-get lispm-comfyui-settings--server :state) 'failed))
      (should (string-search "○ No answer: nothing answers at comfy.test:8188." (buffer-string))))))

(ert-deftest lispm-comfyui-test-settings-after-update ()
  "The settings' window of an older LispM-ComfyUI, left open, is a form anew.
Its buffer has the form's name; once LispM-ComfyUI is updated, its
commands are gone."
  (lispm-comfyui-test--with-settings lispm-comfyui-test--stats
    (let ((older (get-buffer-create (format "*%s*" lispm-comfyui-settings-title))))
      (with-current-buffer older
        (insert "Address  http://comfy.test:8188  a changes it\n"))
      (let ((buffer (lispm-comfyui-settings)))
        (should-not (buffer-live-p older))
        (with-current-buffer buffer
          (should (derived-mode-p 'lispm-ui-form-mode))
          (should (equal (lispm-ui-form-value 'address) "http://comfy.test:8188")))
        ;; A form already there is shown again, as it was left.
        (with-current-buffer buffer
          (lispm-comfyui-test--form-text 'font "Geneva"))
        (should (eq (lispm-comfyui-settings) buffer))
        (with-current-buffer buffer
          (should (equal (lispm-ui-form-value 'font) "Geneva")))))))


(ert-deftest lispm-comfyui-test-canvas-message ()
  "What the canvas has to say is said over it, as the server not answering."
  (let* ((types (lispm-comfyui-test--types))
         (graph (lispm-comfyui-test--graph))
         (svg (lispm-comfyui-canvas-svg
               (list :graph graph :types types :layout (lispm-comfyui-canvas-layout graph types)
                     :view '(0 0 1.0) :message "Cannot reach the server: , opens Settings")
               800 600)))
    (should (string-search "Cannot reach the server: , opens Settings" svg))))

;;;; The client

(ert-deftest lispm-comfyui-test-preview-messages ()
  "Binary previews are kept as files, with the node they are of when said."
  (let* ((lispm-comfyui-directory (make-temp-file "lispm-comfyui-test" t))
         (seen nil)
         (lispm-comfyui-message-functions (list (lambda (type data) (push (cons type data) seen))))
         (bytes (unibyte-string 0 0 0 1 0 0 0 2 ?P ?N ?G)))
    (unwind-protect
        (progn
          (lispm-comfyui--binary bytes)
          (should (equal (caar seen) "preview"))
          (should (string-suffix-p ".png" (alist-get 'file (cdar seen))))
          (should (null (alist-get 'node (cdar seen))))
          (let* ((metadata "{\"node_id\":\"3\",\"image_type\":\"image/jpeg\"}")
                 (bytes (concat (unibyte-string 0 0 0 4 0 0 0 (length metadata)) metadata
                                (unibyte-string 255 216))))
            (lispm-comfyui--binary bytes)
            (should (equal (alist-get 'node (cdar seen)) "3"))
            (should (string-suffix-p ".jpg" (alist-get 'file (cdar seen))))))
      (delete-directory lispm-comfyui-directory t))))

(ert-deftest lispm-comfyui-test-error-message ()
  "The server's errors are told in its words."
  (should (equal (lispm-comfyui--error-message
                  400 '((error . ((type . "prompt_outputs_failed_validation")
                                  (message . "Prompt outputs failed validation")
                                  (details . ""))))
                  "")
                 "Prompt outputs failed validation"))
  (should (equal (lispm-comfyui--error-message 404 nil "404: Not Found") "The server answered 404: 404: Not Found")))

(ert-deftest lispm-comfyui-test-local-network ()
  "macOS keeping Emacs off the local network is told, and where to allow it."
  (should (lispm-comfyui--local-address-p [192 168 1 192 0]))
  (should (lispm-comfyui--local-address-p [172 20 0 1 0]))
  (should (lispm-comfyui--local-address-p [#xfe80 0 0 0 0 0 0 1 0]))
  (should-not (lispm-comfyui--local-address-p [172 32 0 1 0]))
  (should-not (lispm-comfyui--local-address-p [8 8 8 8 0]))
  (should-not (lispm-comfyui--local-address-p [#x2001 #xdb8 0 0 0 0 0 1 0]))
  (should (lispm-comfyui--local-host-p "comfy.example.local"))
  (should-not (lispm-comfyui--local-host-p "127.0.0.1"))
  (let ((invocation-directory "/Applications/LispM.app/Contents/MacOS/"))
    (should (equal (lispm-comfyui--application-name) "LispM")))
  (let ((invocation-directory "/usr/local/bin/"))
    (should (equal (lispm-comfyui--application-name) "Emacs")))
  (should (equal (lispm-comfyui--failed-place
                  "http://192.168.1.192:8188"
                  '(error "Could not create connection to 127.0.0.1:6152"))
                 '("127.0.0.1" . 6152)))
  (should (equal (lispm-comfyui--failed-place
                  "http://192.168.1.192:8188"
                  '(file-error "make client process failed" "No route to host"
                               :name "x" :host "192.168.1.192" :service 8188))
                 '("192.168.1.192" . 8188)))
  (let ((tried nil)
        (invocation-directory "/Applications/LispM.app/Contents/MacOS/")
        (err '(error "Could not create connection to 192.168.1.192:8188")))
    (cl-letf (((symbol-function 'lispm-comfyui--refused-at-once)
               (lambda (host port) (push (cons host port) tried) "No route to host")))
      (let ((system-type 'darwin))
        (should (equal (lispm-comfyui--network-error "http://192.168.1.192:8188" err)
                       "macOS keeps LispM off the local network: turn LispM on in System Settings › Privacy & Security › Local Network"))
        (should (equal tried '(("192.168.1.192" . 8188))))
        ;; Not a machine of the local network: not macOS's doing.
        (should-not (string-prefix-p "macOS" (lispm-comfyui--network-error
                                              "http://192.0.2.1:8188"
                                              '(error "Could not create connection to 192.0.2.1:8188")))))
      (let ((system-type 'gnu/linux))
        (should-not (string-prefix-p "macOS" (lispm-comfyui--network-error "http://192.168.1.192:8188" err))))))
  ;; Tried at once, a connection that does not fail at once is let go.
  (let ((server (make-network-process :name "lispm-comfyui-test-server" :server t
                                      :host "127.0.0.1" :service t :noquery t)))
    (unwind-protect
        (should-not (lispm-comfyui--refused-at-once "127.0.0.1" (process-contact server :service)))
      (delete-process server))))

;;;; The agents' tools

(when (require 'lispm-tools nil t)
  (require 'lispm-comfyui-tools)

  (ert-deftest lispm-comfyui-test-tools-describe ()
    "The workflow is told to a model node by node."
    (lispm-comfyui-test--with-canvas
      (let ((text (lispm-comfyui-tools-describe (current-buffer))))
        (should (string-search "#3 KSampler" text))
        (should (string-search "  model <- #4.MODEL" text))
        (should (string-search "  seed = 156680208700286 (randomize after each run)" text))
        (should (string-search "#11 ImageScale \"Upscale Image\" [bypassed]" text))
        (should (string-search "#13 Note\n  values = \"Remember to save.\"" text))
        (should (string-search "  -> LATENT: LATENT to #8.samples" text)))))

  (ert-deftest lispm-comfyui-test-tools-apply ()
    "Operations change the workflow all at once, or not at all."
    (lispm-comfyui-test--with-canvas
      (let ((text (lispm-comfyui-tools-apply
                   (current-buffer)
                   (vector '((op . "add") (type . "UpscaleModelLoader") (ref . "loader"))
                           '((op . "add") (type . "ImageUpscaleWithModel") (ref . "up"))
                           '((op . "connect") (from_node . "loader") (from_output . "UPSCALE_MODEL")
                             (to_node . "up") (to_input . "upscale_model"))
                           '((op . "connect") (from_node . 8) (from_output . 0)
                             (to_node . "up") (to_input . "image"))
                           '((op . "set") (node . 3) (name . "steps") (value . 28))))))
        (should (string-search "added #14 UpscaleModelLoader as loader" text))
        (should (= (length (lispm-comfyui-graph-nodes lispm-comfyui--graph)) 13))
        (should (equal (cdr (assoc "steps" (lispm-comfyui-node-widget-alist
                                            (lispm-comfyui-node lispm-comfyui--graph 3)
                                            (lispm-comfyui--types))))
                       28))
        (should-error (lispm-comfyui-tools-apply
                       (current-buffer)
                       (vector '((op . "remove") (node . 4))
                               '((op . "connect") (from_node . 5) (from_output . 0)
                                 (to_node . 3) (to_input . "model")))))
        (should (lispm-comfyui-node lispm-comfyui--graph 4))
        (lispm-comfyui-undo)
        (should (= (length (lispm-comfyui-graph-nodes lispm-comfyui--graph)) 11)))))

  (ert-deftest lispm-comfyui-test-tools-types ()
    "Node types are found by words, and told in full by name."
    (let ((types (lispm-comfyui-test--types)))
      (should (string-search "UpscaleModelLoader" (lispm-comfyui-tools-find-types types "upscale model")))
      (let ((text (lispm-comfyui-tools-find-types types "KSampler")))
        (should (string-search "seed: INT" text))
        (should (string-search "model: socket of MODEL" text))
        (should (string-search "LATENT: LATENT" text))))))

(provide 'lispm-comfyui-test)
;;; lispm-comfyui-test.el ends here
