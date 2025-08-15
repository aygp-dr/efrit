# Queue Usage Examples

This document provides practical examples of using the efrit-remote-queue system for agent coordination.

## Example 1: L5 and L7 Engineers Collaborating

### Scenario
An L7 (architect) designs a system while an L5 (implementation engineer) builds it incrementally.

### Agent Definitions

**L7 Agent (Architect)**
```json
{
  "id": "l7-architect",
  "role": "system-architect",
  "continue_logic": "Continue while design incomplete or L5 needs guidance"
}
```

**L5 Agent (Implementation)**
```json
{
  "id": "l5-engineer", 
  "role": "implementation",
  "continue_logic": "Continue while tasks remain or tests fail"
}
```

### Communication Flow

#### Step 1: L7 Creates Architecture
```bash
# L7 writes design request
cat > ~/.emacs.d/efrit-queue/requests/l7-design-001.json << 'EOF'
{
  "id": "l7-design-001",
  "action": "eval",
  "content": "(progn (find-file \"~/project/ARCHITECTURE.md\") (insert \"## System Design\\n\\n### Components\\n1. API Gateway\\n2. Service Layer\\n3. Data Store\\n\") (save-buffer))",
  "metadata": {
    "agent": "l7-architect",
    "phase": "design",
    "continue": "YES"
  }
}
EOF
```

#### Step 2: L5 Reads Design and Starts Implementation
```bash
# L5 checks for design docs
cat > ~/.emacs.d/efrit-queue/requests/l5-impl-001.json << 'EOF'
{
  "id": "l5-impl-001",
  "action": "eval",
  "content": "(progn (find-file \"~/project/src/api_gateway.py\") (insert \"class APIGateway:\\n    def __init__(self):\\n        self.routes = {}\\n\") (save-buffer))",
  "metadata": {
    "agent": "l5-engineer",
    "phase": "implementation",
    "component": "api-gateway",
    "continue": "YES"
  }
}
EOF
```

#### Step 3: L7 Reviews and Provides Feedback
```bash
# L7 reviews implementation
cat > ~/.emacs.d/efrit-queue/requests/l7-review-001.json << 'EOF'
{
  "id": "l7-review-001",
  "action": "eval",
  "content": "(progn (find-file \"~/project/src/api_gateway.py\") (goto-char (point-max)) (insert \"\\n# TODO: Add rate limiting\\n# TODO: Add authentication middleware\\n\") (save-buffer))",
  "metadata": {
    "agent": "l7-architect",
    "phase": "review",
    "feedback": "needs-security-features",
    "continue": "YES"
  }
}
EOF
```

#### Step 4: L5 Implements Feedback
```bash
# L5 addresses TODOs
cat > ~/.emacs.d/efrit-queue/requests/l5-impl-002.json << 'EOF'
{
  "id": "l5-impl-002",
  "action": "eval",
  "content": "(progn (find-file \"~/project/src/api_gateway.py\") (goto-char (point-max)) (insert \"\\n    def add_rate_limit(self, limit=100):\\n        self.rate_limit = limit\\n\") (save-buffer))",
  "metadata": {
    "agent": "l5-engineer",
    "phase": "implementation",
    "addressing": "l7-review-001",
    "continue": "YES"
  }
}
EOF
```

#### Step 5: Both Agents Decide to Stop
```bash
# L7 completes design
cat > ~/.emacs.d/efrit-queue/requests/l7-complete.json << 'EOF'
{
  "id": "l7-complete",
  "action": "eval",
  "content": "(message \"Architecture complete and implemented\")",
  "metadata": {
    "agent": "l7-architect",
    "continue": "NO",
    "reason": "Design fully implemented"
  }
}
EOF

# L5 completes implementation
cat > ~/.emacs.d/efrit-queue/requests/l5-complete.json << 'EOF'
{
  "id": "l5-complete",
  "action": "eval",
  "content": "(message \"Implementation complete, all tests pass\")",
  "metadata": {
    "agent": "l5-engineer",
    "continue": "NO",
    "reason": "All tasks completed"
  }
}
EOF
```

## Example 2: Code Reviewer with Three Coding Agents

### Scenario
A code reviewer coordinates three specialized coding agents working on different aspects of a feature.

### Agent Definitions

**Code Reviewer**
```json
{
  "id": "code-reviewer",
  "role": "review-coordinator",
  "monitors": ["frontend-dev", "backend-dev", "test-engineer"]
}
```

**Frontend Developer**
```json
{
  "id": "frontend-dev",
  "role": "ui-implementation",
  "specialization": "React/TypeScript"
}
```

**Backend Developer**
```json
{
  "id": "backend-dev",
  "role": "api-implementation",
  "specialization": "Python/FastAPI"
}
```

**Test Engineer**
```json
{
  "id": "test-engineer",
  "role": "test-implementation",
  "specialization": "pytest/jest"
}
```

### Parallel Development Flow

#### Round 1: Initial Implementation

```bash
# Frontend starts UI
cat > ~/.emacs.d/efrit-queue/requests/frontend-001.json << 'EOF'
{
  "id": "frontend-001",
  "action": "eval",
  "content": "(progn (find-file \"~/project/src/UserForm.tsx\") (insert \"import React from 'react';\\n\\nexport const UserForm: React.FC = () => {\\n  return <form>...</form>;\\n};\") (save-buffer))",
  "metadata": {
    "agent": "frontend-dev",
    "component": "user-form",
    "status": "in-progress",
    "continue": "YES"
  }
}
EOF

# Backend starts API
cat > ~/.emacs.d/efrit-queue/requests/backend-001.json << 'EOF'
{
  "id": "backend-001",
  "action": "eval",
  "content": "(progn (find-file \"~/project/api/users.py\") (insert \"from fastapi import APIRouter\\n\\nrouter = APIRouter()\\n\\n@router.post('/users')\\nasync def create_user(user: dict):\\n    pass\") (save-buffer))",
  "metadata": {
    "agent": "backend-dev",
    "endpoint": "/users",
    "status": "in-progress",
    "continue": "YES"
  }
}
EOF

# Test engineer prepares tests
cat > ~/.emacs.d/efrit-queue/requests/test-001.json << 'EOF'
{
  "id": "test-001",
  "action": "eval",
  "content": "(progn (find-file \"~/project/tests/test_users.py\") (insert \"import pytest\\n\\ndef test_create_user():\\n    # TODO: Implement when API ready\\n    pass\") (save-buffer))",
  "metadata": {
    "agent": "test-engineer",
    "coverage": "users",
    "status": "waiting-for-implementation",
    "continue": "YES"
  }
}
EOF
```

#### Round 2: Code Review Coordination

```bash
# Reviewer checks all work
cat > ~/.emacs.d/efrit-queue/requests/review-001.json << 'EOF'
{
  "id": "review-001",
  "action": "eval",
  "content": "(progn (let ((issues '())) (find-file \"~/project/src/UserForm.tsx\") (when (not (search-forward \"useState\" nil t)) (push \"Frontend: Missing state management\" issues)) (find-file \"~/project/api/users.py\") (when (not (search-forward \"validate\" nil t)) (push \"Backend: Missing validation\" issues)) (message \"Review issues: %s\" issues)))",
  "metadata": {
    "agent": "code-reviewer",
    "review-round": 1,
    "issues-found": 2,
    "continue": "YES"
  }
}
EOF
```

#### Round 3: Agents Address Feedback

```bash
# Frontend adds state
cat > ~/.emacs.d/efrit-queue/requests/frontend-002.json << 'EOF'
{
  "id": "frontend-002",
  "action": "eval",
  "content": "(progn (find-file \"~/project/src/UserForm.tsx\") (goto-line 1) (insert \"import React, { useState } from 'react';\\n\") (save-buffer))",
  "metadata": {
    "agent": "frontend-dev",
    "addressing": "review-001",
    "change": "added-state-management",
    "continue": "YES"
  }
}
EOF

# Backend adds validation
cat > ~/.emacs.d/efrit-queue/requests/backend-002.json << 'EOF'
{
  "id": "backend-002",
  "action": "eval",
  "content": "(progn (find-file \"~/project/api/users.py\") (goto-char (point-max)) (insert \"\\nfrom pydantic import BaseModel, validator\\n\") (save-buffer))",
  "metadata": {
    "agent": "backend-dev",
    "addressing": "review-001",
    "change": "added-validation",
    "continue": "YES"
  }
}
EOF

# Test engineer writes integration tests
cat > ~/.emacs.d/efrit-queue/requests/test-002.json << 'EOF'
{
  "id": "test-002",
  "action": "eval",
  "content": "(progn (find-file \"~/project/tests/test_integration.py\") (insert \"def test_user_creation_flow():\\n    # Test frontend -> backend flow\\n    assert True\") (save-buffer))",
  "metadata": {
    "agent": "test-engineer",
    "test-type": "integration",
    "coverage": "end-to-end",
    "continue": "YES"
  }
}
EOF
```

#### Round 4: Final Review and Completion

```bash
# Reviewer approves all changes
cat > ~/.emacs.d/efrit-queue/requests/review-final.json << 'EOF'
{
  "id": "review-final",
  "action": "eval",
  "content": "(message \"Code review complete: All issues addressed, tests passing\")",
  "metadata": {
    "agent": "code-reviewer",
    "status": "approved",
    "continue": "NO",
    "reason": "All quality criteria met"
  }
}
EOF

# All agents complete
for agent in frontend-dev backend-dev test-engineer; do
cat > ~/.emacs.d/efrit-queue/requests/${agent}-complete.json << EOF
{
  "id": "${agent}-complete",
  "action": "eval",
  "content": "(message \"${agent}: Work complete\")",
  "metadata": {
    "agent": "${agent}",
    "continue": "NO",
    "reason": "Review approved"
  }
}
EOF
done
```

## Example 3: CONTINUE Pattern with Progressive Enhancement

### Scenario
A single agent progressively enhances a function with the CONTINUE pattern.

```bash
# Iteration 1: Basic function
cat > ~/.emacs.d/efrit-queue/requests/enhance-001.json << 'EOF'
{
  "id": "enhance-001",
  "action": "eval",
  "content": "(progn (find-file \"~/project/utils.el\") (insert \"(defun process-data (data)\\n  data)\\n\") (save-buffer))",
  "metadata": {
    "iteration": 1,
    "enhancement": "basic-structure",
    "continue": "YES"
  }
}
EOF

# Check response and continue
response=$(cat ~/.emacs.d/efrit-queue/responses/resp_enhance-001.json)

# Iteration 2: Add validation
cat > ~/.emacs.d/efrit-queue/requests/enhance-002.json << 'EOF'
{
  "id": "enhance-002",
  "action": "eval",
  "content": "(progn (find-file \"~/project/utils.el\") (goto-char (point-max)) (insert \"\\n(defun validate-data (data)\\n  (and (listp data) (not (null data))))\\n\") (save-buffer))",
  "metadata": {
    "iteration": 2,
    "enhancement": "add-validation",
    "continue": "YES"
  }
}
EOF

# Iteration 3: Add error handling
cat > ~/.emacs.d/efrit-queue/requests/enhance-003.json << 'EOF'
{
  "id": "enhance-003",
  "action": "eval",
  "content": "(progn (find-file \"~/project/utils.el\") (goto-char (point-max)) (insert \"\\n(defun safe-process-data (data)\\n  (condition-case err\\n      (process-data data)\\n    (error (message \\\"Error: %s\\\" err))))\\n\") (save-buffer))",
  "metadata": {
    "iteration": 3,
    "enhancement": "add-error-handling",
    "continue": "YES"
  }
}
EOF

# Iteration 4: Add tests
cat > ~/.emacs.d/efrit-queue/requests/enhance-004.json << 'EOF'
{
  "id": "enhance-004",
  "action": "eval",
  "content": "(progn (find-file \"~/project/utils-test.el\") (insert \"(ert-deftest test-process-data ()\\n  (should (equal (process-data '(1 2 3)) '(1 2 3))))\\n\") (save-buffer))",
  "metadata": {
    "iteration": 4,
    "enhancement": "add-tests",
    "continue": "NO",
    "reason": "Enhancement complete"
  }
}
EOF
```

## Monitoring Queue Activity

### Check Queue Status
```bash
# See pending requests
ls -la ~/.emacs.d/efrit-queue/requests/

# See available responses  
ls -la ~/.emacs.d/efrit-queue/responses/

# Monitor in real-time
watch -n 1 'echo "=== Requests ===" && ls ~/.emacs.d/efrit-queue/requests/ && echo -e "\\n=== Responses ===" && ls ~/.emacs.d/efrit-queue/responses/'
```

### Parse Agent Decisions
```bash
# Extract CONTINUE decisions from responses
for file in ~/.emacs.d/efrit-queue/responses/*.json; do
    agent=$(jq -r '.metadata.agent // "unknown"' "$file")
    continue=$(jq -r '.metadata.continue // "N/A"' "$file")
    echo "$agent: CONTINUE=$continue"
done
```

## Best Practices

1. **Unique IDs**: Always use unique request IDs (include timestamp or UUID)
2. **Metadata**: Include rich metadata for agent coordination
3. **CONTINUE Logic**: Make continue decisions explicit and reasoned
4. **Error Handling**: Check response status before proceeding
5. **Cleanup**: Archive old requests/responses periodically

## Troubleshooting

### Queue Not Processing
```bash
# Check if efrit daemon is running
bash bin/launch-autonomous-efrit.sh status

# Check logs
tail -f ~/.emacs.d/efrit-ai-workspace/daemon.log
```

### Malformed JSON
```bash
# Validate JSON before sending
cat request.json | jq '.' || echo "Invalid JSON"
```

### Response Timeout
```bash
# Add timeout to requests
{
  "id": "timeout-test",
  "action": "eval",
  "content": "...",
  "timeout": 30
}
```