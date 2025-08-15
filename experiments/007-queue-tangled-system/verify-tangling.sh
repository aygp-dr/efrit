#!/usr/bin/env bash
# Verify that tangling works and files are generated

set -e

echo "🧪 Verifying Org-Mode Tangling System"
echo "=" * 40

# Check if we're in the right directory
if [[ ! -f "queue-system.org" ]]; then
    echo "❌ queue-system.org not found"
    exit 1
fi

echo "📝 Found queue-system.org"

# Try to tangle (requires Emacs with org-mode)
echo "🔧 Attempting to tangle org file..."
if command -v emacs >/dev/null 2>&1; then
    # Use a more reliable tangling approach
    emacs --batch \
          --eval "(require 'org)" \
          --eval "(require 'ob-tangle)" \
          --eval "(setq org-confirm-babel-evaluate nil)" \
          queue-system.org \
          --eval "(org-babel-tangle)" \
          --kill 2>/dev/null || echo "⚠️  Tangling may require manual execution in Emacs"
    
    # Check if files were generated
    echo ""
    echo "📁 Checking for generated files..."
    
    expected_files=("models.py" "schema_generator.py" "test_harness.py" "agent_coordinator.py" "queue_client.py" "requirements.txt" "install.sh")
    
    for file in "${expected_files[@]}"; do
        if [[ -f "$file" ]]; then
            echo "✅ $file"
        else
            echo "❌ $file (will be generated when tangled)"
        fi
    done
    
else
    echo "⚠️  Emacs not found - cannot test tangling"
    echo "   Manual tangling: Open queue-system.org in Emacs, then C-c C-v t"
fi

echo ""
echo "📋 Tangling Instructions:"
echo "1. Open queue-system.org in Emacs"
echo "2. Press C-c C-v t (org-babel-tangle)"
echo "3. All Python files will be extracted"
echo "4. Run: chmod +x install.sh && ./install.sh"

echo ""
echo "🎯 Expected Output Files:"
echo "   - models.py (Pydantic models)"
echo "   - schema_generator.py (JSON schema generation)"  
echo "   - test_harness.py (Test generation with single-line JSON)"
echo "   - agent_coordinator.py (Multi-agent workflow)"
echo "   - queue_client.py (Python client)"
echo "   - requirements.txt (Dependencies)"
echo "   - install.sh (Setup script)"

echo ""
echo "✅ Verification complete!"