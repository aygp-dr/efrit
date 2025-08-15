# Single-Line JSON Generator


# [[file:queue-system.org::*Single-Line JSON Generator][Single-Line JSON Generator:1]]
"""
Test harness for generating and validating queue messages.
Supports single-line JSON generation for easy testing.
"""
import json
import uuid
from datetime import datetime
from models import (
    QueueRequest, QueueResponse, AgentRequest, AgentResponse,
    MessageType, MessageStatus, AgentRole, AgentMetadata
)


class TestHarness:
    """Test harness for queue system."""
    
    def __init__(self):
        self.test_cases = []
    
    def generate_request_json(self, msg_type: MessageType, content: str, 
                            agent_id: str = None, role: AgentRole = None) -> str:
        """Generate a single-line JSON request."""
        req_id = f"req_{int(datetime.now().timestamp())}"
        
        if agent_id and role:
            # Agent request
            metadata = AgentMetadata(
                agent_id=agent_id,
                role=role,
                phase="implementation",
                continue_logic="Continue while tasks remain"
            )
            request = AgentRequest(
                id=req_id,
                type=msg_type,
                content=content,
                agent_metadata=metadata
            )
        else:
            # Basic request
            request = QueueRequest(
                id=req_id,
                type=msg_type,
                content=content
            )
        
        return request.json(separators=(',', ':'))
    
    def generate_response_json(self, request_id: str, status: MessageStatus,
                             result: str = None, error: str = None) -> str:
        """Generate a single-line JSON response."""
        response = QueueResponse(
            id=request_id,
            status=status,
            result=result,
            error=error,
            execution_time=0.001
        )
        return response.json(separators=(',', ':'))
    
    def create_l5_l7_workflow(self) -> List[str]:
        """Create L5/L7 collaboration workflow as JSON lines."""
        workflow = []
        
        # L7 creates architecture
        l7_design = self.generate_request_json(
            MessageType.EVAL,
            "(progn (find-file \"ARCHITECTURE.md\") (insert \"## System Design\\n\") (save-buffer))",
            "l7-architect",
            AgentRole.L7_ARCHITECT
        )
        workflow.append(l7_design)
        
        # L5 implements based on design
        l5_impl = self.generate_request_json(
            MessageType.EVAL,
            "(progn (find-file \"src/api.py\") (insert \"# API Implementation\\n\") (save-buffer))",
            "l5-engineer", 
            AgentRole.L5_ENGINEER
        )
        workflow.append(l5_impl)
        
        # Security review
        security_review = self.generate_request_json(
            MessageType.EVAL,
            "(progn (find-file \"SECURITY_REVIEW.md\") (insert \"## Security Analysis\\n\") (save-buffer))",
            "security-expert",
            AgentRole.SECURITY_EXPERT
        )
        workflow.append(security_review)
        
        return workflow
    
    def run_tests(self):
        """Run comprehensive test suite."""
        print("🧪 Running Efrit Queue Test Harness")
        print("=" * 50)
        
        # Test 1: Basic request generation
        print("\n📝 Test 1: Basic Request Generation")
        basic_req = self.generate_request_json(MessageType.EVAL, "(+ 40 2)")
        print(f"Request: {basic_req}")
        
        # Validate by parsing back
        try:
            parsed = QueueRequest.parse_raw(basic_req)
            print(f"✅ Valid: {parsed.type} request with ID {parsed.id}")
        except Exception as e:
            print(f"❌ Invalid: {e}")
        
        # Test 2: Agent workflow
        print("\n🤖 Test 2: L5/L7 Agent Workflow")
        workflow = self.create_l5_l7_workflow()
        for i, request in enumerate(workflow, 1):
            print(f"Step {i}: {request}")
            try:
                parsed = AgentRequest.parse_raw(request)
                print(f"✅ Valid agent request: {parsed.agent_metadata.role}")
            except Exception as e:
                print(f"❌ Invalid: {e}")
        
        # Test 3: Response generation
        print("\n📤 Test 3: Response Generation")
        response = self.generate_response_json("req_123", MessageStatus.SUCCESS, "42")
        print(f"Response: {response}")
        
        try:
            parsed = QueueResponse.parse_raw(response)
            print(f"✅ Valid response: {parsed.status} with result '{parsed.result}'")
        except Exception as e:
            print(f"❌ Invalid: {e}")
        
        print("\n🎉 Test harness complete!")


if __name__ == "__main__":
    harness = TestHarness()
    harness.run_tests()
# Single-Line JSON Generator:1 ends here
