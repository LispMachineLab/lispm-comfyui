EMACS ?= emacs
# LispM Kit, with which the window declares its commands: a checkout
# beside this one by default.
LISPM_KIT ?= ../lispm-kit
# websocket.el, from GNU ELPA: the directory it is installed in.  By
# default, the one package.el installed it in, if any.
WEBSOCKET ?= $(firstword $(wildcard $(HOME)/.emacs.d/elpa/websocket-*))
# lisp/ of lispm-agent, a checkout beside this one by default.  The
# application needs it not; with it on the load path, the tests of the
# agents' tools run too.
LISPM_AGENT ?= ../lisp_machine/lisp
# LispM UI, whose forms the simple view and settings are and whose dialog chooses an
# image to upload: a checkout beside this one by default.
LISPM_UI ?= ../lispm-ui
LOAD_PATH = -L . -L $(LISPM_KIT) -L $(LISPM_UI) $(foreach dir,$(WEBSOCKET),-L $(dir)) \
	$(foreach dir,$(wildcard $(LISPM_AGENT)),-L $(dir))

FILES = lispm-comfyui-graph.el lispm-comfyui-prompt.el lispm-comfyui-client.el lispm-comfyui-canvas.el \
	lispm-comfyui-settings.el lispm-comfyui-simple.el lispm-comfyui.el
TOOLS = lispm-comfyui-tools.el

.PHONY: test compile

test:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -l test/lispm-comfyui-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.  The tools compile only with
# LispM Agent on the load path.
compile:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES) $(if $(wildcard $(LISPM_AGENT)),$(TOOLS)); \
	status=$$?; rm -f *.elc; exit $$status
