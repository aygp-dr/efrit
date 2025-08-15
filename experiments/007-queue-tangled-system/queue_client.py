# Python Client for Queue Interaction


# [[file:queue-system.org::*Python Client for Queue Interaction][Python Client for Queue Interaction:1]]
"""
Python client for interacting with Efrit queue system.
"""
import json
import time
from pathlib import Path
from typing import Optional
from models import QueueRequest, QueueResponse, MessageType, MessageStatus


class EfritQueueClient:
    """Client for sending requests to Efrit queue system."""
    
    def __init__(self, queue_dir: str = "~/.emacs.d/efrit-queue"):
        self.queue_dir = Path(queue_dir).expanduser()
        self.requests_dir = self.queue_dir / "requests" 
        self.responses_dir = self.queue_dir / "responses"
        
        # Ensure directories exist
        self.requests_dir.mkdir(parents=True, exist_ok=True)
        self.responses_dir.mkdir(parents=True, exist_ok=True)
    
    def send_eval(self, elisp_code: str, request_id: str = None) -> str:
        """Send Elisp evaluation request."""
        if not request_id:
            request_id = f"eval_{int(time.time())}"
        
        request = QueueRequest(
            id=request_id,
            type=MessageType.EVAL,
            content=elisp_code
        )
        
        request_file = self.requests_dir / f"{request_id}.json"
        with open(request_file, 'w') as f:
            f.write(request.json(indent=2))
        
        return request_id
    
    def send_shell(self, command: str, request_id: str = None) -> str:
        """Send shell command request."""
        if not request_id:
            request_id = f"shell_{int(time.time())}"
        
        request = QueueRequest(
            id=request_id,
            type=MessageType.SHELL,
            content=command
        )
        
        request_file = self.requests_dir / f"{request_id}.json"
        with open(request_file, 'w') as f:
            f.write(request.json(indent=2))
        
        return request_id
    
    def wait_for_response(self, request_id: str, timeout: int = 30) -> Optional[QueueResponse]:
        """Wait for response to a request."""
        response_file = self.responses_dir / f"resp_{request_id[5:]}.json"
        
        for _ in range(timeout):
            if response_file.exists():
                with open(response_file, 'r') as f:
                    return QueueResponse.parse_raw(f.read())
            time.sleep(1)
        
        return None
    
    def eval_and_wait(self, elisp_code: str, timeout: int = 30) -> Optional[str]:
        """Send eval request and wait for result."""
        request_id = self.send_eval(elisp_code)
        response = self.wait_for_response(request_id, timeout)
        
        if response and response.status == MessageStatus.SUCCESS:
            return response.result
        elif response:
            print(f"❌ Error: {response.error}")
            return None
        else:
            print(f"⏰ Timeout waiting for response")
            return None


def demo_queue_client():
    """Demonstrate queue client usage."""
    print("🔌 Efrit Queue Client Demo")
    print("=" * 30)
    
    client = EfritQueueClient()
    
    # Test 1: Simple arithmetic
    print("\n➕ Test 1: Simple arithmetic")
    result = client.eval_and_wait("(+ 40 2)")
    if result:
        print(f"Result: {result}")
    
    # Test 2: Get current time
    print("\n🕐 Test 2: Current time")
    result = client.eval_and_wait("(current-time-string)")
    if result:
        print(f"Time: {result}")
    
    # Test 3: Buffer operations
    print("\n📄 Test 3: Buffer operations")
    elisp_code = """(progn
      (with-temp-buffer
        (insert "Hello from Python!")
        (buffer-string)))"""
    result = client.eval_and_wait(elisp_code)
    if result:
        print(f"Buffer content: {result}")
    
    print("\n✅ Demo complete!")


if __name__ == "__main__":
    demo_queue_client()
# Python Client for Queue Interaction:1 ends here
