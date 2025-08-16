# Makefile for Experiment 002: Remote Queue Deep Dive

# Configuration
EXPERIMENT_DIR := $(shell pwd)
EMACS = emacs
EMACS_BATCH = $(EMACS) --batch --no-init-file
EXPERIMENT_NAME = 002-remote-queue-deep-dive

# Generated files from org-mode tangling
TANGLED_FILES = test-queue.sh analyze-queue.el
EL_FILES = analyze-queue.el
ELC_FILES = $(EL_FILES:.el=.elc)

# Source files
ORG_FILE = experiment.org
README_FILE = README.md

.PHONY: all clean tangle compile test check help analyze monitor

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
	@echo "  test        - Run queue analysis tests"
	@echo "  analyze     - Analyze queue structure and patterns"
	@echo "  monitor     - Start queue activity monitoring"
	@echo "  check       - Check syntax and validate"
	@echo ""
	@echo "Development:"
	@echo "  verify      - Verify all components work"

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
	@chmod +x test-queue.sh
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

# Run queue analysis tests
test: tangle
	@echo "🧪 Running queue analysis tests..."
	@echo ""
	@echo "⚠️  Note: This requires efrit-remote-queue to be running in Emacs"
	@echo "   Start with: M-x efrit-remote-queue-start"
	@echo ""
	@./test-queue.sh

# Analyze queue structure
analyze: compile
	@echo "📊 Analyzing queue structure..."
	@$(EMACS_BATCH) \
		--eval "(add-to-list 'load-path \"$(EXPERIMENT_DIR)\")" \
		--eval "(require 'analyze-queue)" \
		--eval "(efrit-analyze-queue-structure)" \
		--eval "(efrit-analyze-request-patterns)"

# Start monitoring (interactive mode only)
monitor: compile
	@echo "🔍 Starting queue monitor..."
	@echo "This will open Emacs interactively for monitoring."
	@$(EMACS) \
		--eval "(add-to-list 'load-path \"$(EXPERIMENT_DIR)\")" \
		--eval "(require 'analyze-queue)" \
		--eval "(efrit-monitor-queue-activity)"

# Verify all components
verify: check
	@echo "✅ Verification complete"
	@echo ""
	@echo "Generated files:"
	@ls -la $(TANGLED_FILES) 2>/dev/null || echo "  No files generated yet"
	@echo ""
	@echo "Queue directory status:"
	@if [ -d "$$HOME/.emacs.d/efrit-queue" ]; then \
		echo "  ✅ Queue directory exists"; \
		ls -la "$$HOME/.emacs.d/efrit-queue" 2>/dev/null; \
	else \
		echo "  ❌ Queue directory not found"; \
		echo "     Start efrit-remote-queue in Emacs first"; \
	fi
	@echo ""
	@echo "To run analysis:"
	@echo "  1. Start Efrit: M-x efrit-remote-queue-start"
	@echo "  2. Run: make test"
	@echo "  3. Or: make analyze"

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
	@echo ""
	@echo "Queue System Status:"
	@if [ -d "$$HOME/.emacs.d/efrit-queue" ]; then \
		echo "  Directory: ✅ exists"; \
		find "$$HOME/.emacs.d/efrit-queue" -name "*.json" | wc -l | \
		sed 's/^/  JSON files: /'; \
	else \
		echo "  Directory: ❌ not found"; \
	fi

# Development target for continuous integration
ci: clean tangle check compile verify
	@echo "🎉 CI pipeline complete"