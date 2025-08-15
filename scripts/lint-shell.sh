#!/usr/bin/env sh
# lint-shell.sh - Shell script linting utility
# Can be called from Makefile, cron, or directly
# Usage: lint-shell.sh [file1.sh file2.sh ...]
#        If no files provided, finds all .sh files in project

set -e

# Resolve project root based on script location
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output (disabled if not in terminal)
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    NC=''
fi

# Get list of shell files to check
if [ $# -eq 0 ]; then
    # No arguments - find all shell scripts in project
    echo "Finding shell scripts in $PROJECT_ROOT..."
    SHELL_FILES=$(find "$PROJECT_ROOT" -name "*.sh" -type f \
        -path "*/test/*" -o \
        -path "*/bin/*" -o \
        -path "*/experiments/*" -o \
        -path "*/scripts/*" | sort)
else
    # Use provided file list
    SHELL_FILES="$@"
fi

# Check if any files found
if [ -z "$SHELL_FILES" ]; then
    echo "${YELLOW}No shell scripts found to check${NC}"
    exit 0
fi

echo "Checking shell scripts..."
echo ""

# Track results
TOTAL=0
PASSED=0
WARNINGS=0
ERRORS=0

# Check for shellcheck availability
HAS_SHELLCHECK=0
if command -v shellcheck >/dev/null 2>&1; then
    HAS_SHELLCHECK=1
    echo "Using shellcheck for comprehensive linting"
else
    echo "${YELLOW}shellcheck not found - performing basic checks only${NC}"
    echo "Install shellcheck for more thorough analysis"
fi
echo ""

# Check each file
for file in $SHELL_FILES; do
    TOTAL=$((TOTAL + 1))
    printf "Checking %s... " "$file"
    
    # Check if file exists
    if [ ! -f "$file" ]; then
        printf "${RED}FILE NOT FOUND${NC}\n"
        ERRORS=$((ERRORS + 1))
        continue
    fi
    
    # Basic checks always performed
    ISSUES=""
    
    # Check shebang
    SHEBANG=$(head -1 "$file")
    case "$SHEBANG" in
        "#!/usr/bin/env "*)
            # Good - portable shebang
            ;;
        "#!/bin/sh"|"#!/bin/bash"|"#!/bin/dash"|"#!/bin/ksh")
            ISSUES="${ISSUES}non-portable shebang; "
            WARNINGS=$((WARNINGS + 1))
            ;;
        "#!"*)
            # Other shebang - might be okay
            ;;
        *)
            ISSUES="${ISSUES}missing shebang; "
            WARNINGS=$((WARNINGS + 1))
            ;;
    esac
    
    # Basic syntax check with sh
    if ! sh -n "$file" 2>/dev/null; then
        ISSUES="${ISSUES}syntax error; "
        ERRORS=$((ERRORS + 1))
    fi
    
    # Shellcheck if available
    if [ $HAS_SHELLCHECK -eq 1 ]; then
        # Run shellcheck with common exclusions
        # SC2086: Double quote to prevent globbing (often intentional)
        # SC2034: Variable appears unused (may be used in sourced files)
        if ! shellcheck -e SC2086,SC2034 "$file" >/dev/null 2>&1; then
            ISSUES="${ISSUES}shellcheck warnings; "
            WARNINGS=$((WARNINGS + 1))
        fi
    fi
    
    # Report results for this file
    if [ -z "$ISSUES" ]; then
        printf "${GREEN}✓${NC}\n"
        PASSED=$((PASSED + 1))
    else
        printf "${YELLOW}⚠ ${ISSUES}${NC}\n"
    fi
done

echo ""
echo "=================="
echo "Summary:"
echo "  Total files:  $TOTAL"
echo "  Passed:       $PASSED"
echo "  Warnings:     $WARNINGS"
echo "  Errors:       $ERRORS"

# Exit status
if [ $ERRORS -gt 0 ]; then
    echo ""
    echo "${RED}❌ Shell script linting failed with errors${NC}"
    exit 1
elif [ $WARNINGS -gt 0 ]; then
    echo ""
    echo "${YELLOW}⚠️  Shell script linting passed with warnings${NC}"
    exit 0
else
    echo ""
    echo "${GREEN}✅ All shell script checks passed${NC}"
    exit 0
fi