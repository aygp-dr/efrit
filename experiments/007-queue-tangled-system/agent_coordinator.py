# Multi-Agent Workflow Management


# [[file:queue-system.org::*Multi-Agent Workflow Management][Multi-Agent Workflow Management:1]]
"""
Agent coordinator for managing multi-agent workflows.
"""
import json
import os
import time
from pathlib import Path
from typing import Dict, List
from models import AgentRequest, AgentResponse, AgentMetadata, AgentRole, MessageType
from test_harness import TestHarness


class AgentCoordinator:
    """Coordinates multiple agents working together."""
    
    def __init__(self, queue_dir: str = "~/.emacs.d/efrit-queue"):
        self.queue_dir = Path(queue_dir).expanduser()
        self.requests_dir = self.queue_dir / "requests"
        self.responses_dir = self.queue_dir / "responses"
        self.agents: Dict[str, AgentMetadata] = {}
        self.harness = TestHarness()
        
        # Ensure directories exist
        self.requests_dir.mkdir(parents=True, exist_ok=True)
        self.responses_dir.mkdir(parents=True, exist_ok=True)
    
    def register_agent(self, agent_id: str, role: AgentRole, 
                      continue_logic: str, dependencies: List[str] = None):
        """Register a new agent."""
        self.agents[agent_id] = AgentMetadata(
            agent_id=agent_id,
            role=role,
            phase="initialized",
            continue_logic=continue_logic,
            dependencies=dependencies or []
        )
        print(f"🤖 Registered agent: {agent_id} ({role})")
    
    def send_request(self, agent_id: str, msg_type: MessageType, content: str) -> str:
        """Send a request from an agent."""
        if agent_id not in self.agents:
            raise ValueError(f"Unknown agent: {agent_id}")
        
        agent_meta = self.agents[agent_id]
        request = AgentRequest(
            id=f"{agent_id}_{int(time.time())}",
            type=msg_type,
            content=content,
            agent_metadata=agent_meta
        )
        
        # Write to queue
        request_file = self.requests_dir / f"{request.id}.json"
        with open(request_file, 'w') as f:
            f.write(request.json(indent=2))
        
        print(f"📤 {agent_id} sent request: {request.id}")
        return request.id
    
    def wait_for_response(self, request_id: str, timeout: int = 30) -> AgentResponse:
        """Wait for a response to a request."""
        response_file = self.responses_dir / f"resp_{request_id[4:]}.json"
        
        for _ in range(timeout):
            if response_file.exists():
                with open(response_file, 'r') as f:
                    return AgentResponse.parse_raw(f.read())
            time.sleep(1)
        
        raise TimeoutError(f"No response for {request_id} within {timeout}s")
    
    def run_l5_l7_collaboration(self):
        """Run L5/L7 collaboration example."""
        print("🚀 Starting L5/L7 Collaboration Workflow")
        print("=" * 50)
        
        # Register agents
        self.register_agent("l7-architect", AgentRole.L7_ARCHITECT, 
                          "Continue while design is incomplete")
        self.register_agent("l5-engineer", AgentRole.L5_ENGINEER,
                          "Continue while implementation tasks remain",
                          dependencies=["l7-architect"])
        
        # Step 1: L7 creates architecture
        print("\n📐 Step 1: L7 Architect creates system design")
        arch_content = """(progn 
          (find-file "~/project/ARCHITECTURE.md")
          (insert "## System Architecture\\n\\n### Components\\n1. API Gateway\\n2. Service Layer\\n3. Data Store\\n")
          (save-buffer))"""
        
        arch_req_id = self.send_request("l7-architect", MessageType.EVAL, arch_content)
        
        # Step 2: L5 implements based on architecture  
        print("\n🔧 Step 2: L5 Engineer starts implementation")
        impl_content = """(progn
          (find-file "~/project/src/api.py") 
          (insert "# API Gateway Implementation\\nfrom flask import Flask\\napp = Flask(__name__)\\n")
          (save-buffer))"""
        
        impl_req_id = self.send_request("l5-engineer", MessageType.EVAL, impl_content)
        
        print(f"\n⏳ Waiting for Efrit to process requests...")
        print(f"   Architecture request: {arch_req_id}")  
        print(f"   Implementation request: {impl_req_id}")
        print(f"\n💡 Start Efrit queue system with: (efrit-remote-queue-start)")
    
    def generate_test_files(self):
        """Generate test files for manual testing."""
        print("📁 Generating test request files...")
        
        # Generate L5/L7 workflow
        workflow = self.harness.create_l5_l7_workflow()
        
        for i, request_json in enumerate(workflow, 1):
            filename = f"test_workflow_{i:02d}.json"
            filepath = self.requests_dir / filename
            
            # Parse and reformat for readability
            request_data = json.loads(request_json)
            with open(filepath, 'w') as f:
                json.dump(request_data, f, indent=2)
            
            print(f"   Generated: {filename}")
        
        print(f"🎯 Test files written to: {self.requests_dir}")


if __name__ == "__main__":
    coordinator = AgentCoordinator()
    
    print("🎮 Efrit Agent Coordinator")
    print("=" * 30)
    print("1. Generate test files")  
    print("2. Run L5/L7 collaboration")
    print("3. Both")
    
    choice = input("\nChoose option (1-3): ").strip()
    
    if choice in ["1", "3"]:
        coordinator.generate_test_files()
    
    if choice in ["2", "3"]:
        coordinator.run_l5_l7_collaboration()
# Multi-Agent Workflow Management:1 ends here
