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
       (setq lispm-comfyui--graph (lispm-comfyui-test--graph))
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
            (should (equal (alist-get 9 lispm-comfyui--images) (list image)))
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
