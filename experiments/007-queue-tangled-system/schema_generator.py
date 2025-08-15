# Schema Generation


# [[file:queue-system.org::*Schema Generation][Schema Generation:1]]
"""
Generate JSON schemas from Pydantic models.
"""
import json
from models import QueueRequest, QueueResponse, AgentRequest, AgentResponse


def generate_schemas():
    """Generate JSON schemas for all models."""
    schemas = {
        "QueueRequest": QueueRequest.schema(),
        "QueueResponse": QueueResponse.schema(), 
        "AgentRequest": AgentRequest.schema(),
        "AgentResponse": AgentResponse.schema()
    }
    
    # Write schemas to files
    for name, schema in schemas.items():
        with open(f"{name.lower()}_schema.json", "w") as f:
            json.dump(schema, f, indent=2)
    
    return schemas


if __name__ == "__main__":
    schemas = generate_schemas()
    print("Generated schemas:")
    for name in schemas:
        print(f"  - {name.lower()}_schema.json")
# Schema Generation:1 ends here
