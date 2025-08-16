# Makefile for Experiment 001: Ollama Elisp-Only Responses

# Configuration
EXPERIMENT_DIR := $(shell pwd)
EMACS = emacs
EMACS_BATCH = $(EMACS) --batch --no-init-file
EXPERIMENT_NAME = 001-ollama-elisp-only

# Generated files from org-mode tangling
TANGLED_FILES = ollama-elisp-only.el test-ollama-elisp.el test-with-curl.sh
EL_FILES = ollama-elisp-only.el test-ollama-elisp.el
ELC_FILES = $(EL_FILES:.el=.elc)

# Source files
ORG_FILE = experiment.org
README_FILE = README.md

.PHONY: all clean tangle compile test check help install verify

# Default target
all: tangle compile

# Help target
help:
	@echo "Experiment $(EXPERIMENT_NAME) - Makefile targets:"
	@echo ""
	@echo "Building:"
	@echo "  all         - Tangle and compile (default)"
	@echo "  tangle      - Extract files from org document"
	@echo "  compile     - Byte compile Elisp files"
	@echo "  clean       - Remove generated files"
	@echo ""
	@echo "Testing:"
	@echo "  test        - Run curl tests (requires Ollama)"
	@echo "  test-elisp  - Run Elisp test suite"
	@echo "  check       - Check syntax and validate"
	@echo "  verify      - Verify all components work"
	@echo ""
	@echo "Development:"
	@echo "  install     - Install to load-path"
	@echo "  lint        - Check code style"

# Tangle org file to extract code
tangle: $(TANGLED_FILES)

$(TANGLED_FILES): $(ORG_FILE)
	@echo "📝 Tangling $(ORG_FILE)..."
	@$(EMACS_BATCH) \
		--eval "(require 'org)" \
		--eval "(require 'ob-tangle)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		$(ORG_FILE) \
		--eval "(org-babel-tangle)" \
		--kill
	@chmod +x test-with-curl.sh
	@echo "✅ Tangled files: $(TANGLED_FILES)"

# Compile Elisp files
compile: $(ELC_FILES)

$(ELC_FILES): %.elc: %.el
	@echo "🔨 Compiling $<..."
	@$(EMACS_BATCH) \
		--eval "(add-to-list 'load-path \"$(EXPERIMENT_DIR)\")" \
		--eval "(setq byte-compile-error-on-warn nil)" \
		-f batch-byte-compile $<

# Syntax checking
check: tangle
	@echo "🔍 Checking syntax..."
	@for file in $(EL_FILES); do \
		echo "Checking $$file..."; \
		$(EMACS_BATCH) --eval "(check-parens)" $$file || exit 1; \
	done
	@echo "✅ Syntax checks passed"

# Linting
lint: tangle
	@echo "🧹 Linting code..."
	@for file in $(EL_FILES); do \
		echo "Linting $$file..."; \
		if ! grep -q "lexical-binding: t" $$file; then \
			echo "⚠️  Warning: Missing lexical-binding in $$file"; \
		fi; \
	done
	@echo "✅ Linting complete"

# Test with curl (requires Ollama)
test: tangle
	@echo "🧪 Running curl tests..."
	@if command -v curl >/dev/null 2>&1; then \
		./test-with-curl.sh; \
	else \
		echo "❌ curl not found - cannot run HTTP tests"; \
		exit 1; \
	fi

# Test Elisp functionality
test-elisp: compile
	@echo "🧪 Running Elisp test suite..."
	@$(EMACS_BATCH) \
		--eval "(add-to-list 'load-path \"$(EXPERIMENT_DIR)\")" \
		--eval "(require 'test-ollama-elisp)" \
		--eval "(ert-run-tests-batch-and-exit)"

# Verify all components
verify: check lint
	@echo "✅ Verification complete"
	@echo ""
	@echo "Generated files:"
	@ls -la $(TANGLED_FILES) 2>/dev/null || echo "  No files generated yet"
	@echo ""
	@echo "To test:"
	@echo "  1. Ensure Ollama is running: ollama serve"
	@echo "  2. Run: make test"
	@echo "  3. Or in Emacs: (load-file \"ollama-elisp-only.el\")"

# Install to Emacs load-path
install: compile
	@echo "📦 Installing to Emacs..."
	@SITE_LISP=$$($(EMACS) --batch --eval "(princ (car load-path))" 2>/dev/null); \
	if [ -n "$$SITE_LISP" ] && [ -d "$$SITE_LISP" ]; then \
		echo "Installing to $$SITE_LISP/$(EXPERIMENT_NAME)/"; \
		mkdir -p "$$SITE_LISP/$(EXPERIMENT_NAME)"; \
		cp $(EL_FILES) $(ELC_FILES) "$$SITE_LISP/$(EXPERIMENT_NAME)/"; \
		echo "✅ Installed successfully"; \
		echo "Add to your init.el:"; \
		echo "  (add-to-list 'load-path \"$$SITE_LISP/$(EXPERIMENT_NAME)\")"; \
		echo "  (require 'ollama-elisp-only)"; \
	else \
		echo "❌ Could not determine Emacs load-path"; \
		exit 1; \
	fi

# Clean generated files
clean:
	@echo "🧹 Cleaning generated files..."
	@rm -f $(TANGLED_FILES) $(ELC_FILES)
	@echo "✅ Clean complete"

# Show current configuration
config:
	@echo "Experiment Configuration:"
	@echo "  Name: $(EXPERIMENT_NAME)"
	@echo "  Directory: $(EXPERIMENT_DIR)"
	@echo "  Emacs: $(EMACS)"
	@echo "  Org file: $(ORG_FILE)"
	@echo "  Tangled files: $(TANGLED_FILES)"
	@echo "  Elisp files: $(EL_FILES)"
	@echo ""
	@echo "Dependencies:"
	@echo -n "  Emacs: "; $(EMACS) --version | head -1 || echo "not found"
	@echo -n "  curl: "; curl --version | head -1 || echo "not found"
	@echo -n "  Ollama: "; curl -s http://localhost:11434/api/tags >/dev/null 2>&1 && echo "running" || echo "not running"

# Development target for continuous integration
ci: clean tangle check lint compile verify
	@echo "🎉 CI pipeline complete"