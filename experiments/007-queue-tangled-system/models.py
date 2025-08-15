# Base Queue Message Model


# [[file:queue-system.org::*Base Queue Message Model][Base Queue Message Model:1]]
"""
Pydantic models for Efrit queue system.
Provides type safety and JSON schema generation.
"""
from datetime import datetime
from enum import Enum
from typing import Optional, Dict, Any, List
from pydantic import BaseModel, Field


class MessageType(str, Enum):
    """Queue message types."""
    EVAL = "eval"
    SHELL = "shell" 
    FILE_READ = "file_read"
    FILE_WRITE = "file_write"
    NATURAL = "natural"


class MessageStatus(str, Enum):
    """Queue message status."""
    PENDING = "pending"
    PROCESSING = "processing"
    SUCCESS = "success"
    ERROR = "error"
    TIMEOUT = "timeout"


class AgentRole(str, Enum):
    """Agent role types."""
    L5_ENGINEER = "l5-engineer"
    L7_ARCHITECT = "l7-architect"
    SECURITY_EXPERT = "security-expert"
    PERFORMANCE_ENGINEER = "performance-engineer"
    QA_ENGINEER = "qa-engineer"


class QueueRequest(BaseModel):
    """Base queue request message."""
    id: str = Field(..., description="Unique request identifier")
    timestamp: datetime = Field(default_factory=datetime.now, description="Request timestamp")
    type: MessageType = Field(..., description="Type of operation to perform")
    content: str = Field(..., description="Content to execute or process")
    metadata: Optional[Dict[str, Any]] = Field(default=None, description="Additional metadata")
    
    class Config:
        use_enum_values = True
        json_encoders = {
            datetime: lambda v: v.isoformat()
        }


class QueueResponse(BaseModel):
    """Base queue response message."""
    id: str = Field(..., description="Request ID this responds to")
    timestamp: datetime = Field(default_factory=datetime.now, description="Response timestamp")
    status: MessageStatus = Field(..., description="Execution status")
    result: Optional[str] = Field(default=None, description="Execution result")
    error: Optional[str] = Field(default=None, description="Error message if failed")
    execution_time: Optional[float] = Field(default=None, description="Execution time in seconds")
    
    class Config:
        use_enum_values = True
        json_encoders = {
            datetime: lambda v: v.isoformat()
        }


class AgentMetadata(BaseModel):
    """Agent-specific metadata."""
    agent_id: str = Field(..., description="Agent identifier")
    role: AgentRole = Field(..., description="Agent role")
    phase: str = Field(..., description="Current phase of work")
    continue_logic: str = Field(..., description="Logic for when to continue")
    dependencies: List[str] = Field(default=[], description="Dependencies on other agents")
    
    class Config:
        use_enum_values = True


class AgentRequest(QueueRequest):
    """Agent-specific queue request."""
    agent_metadata: AgentMetadata = Field(..., description="Agent coordination metadata")


class AgentResponse(QueueResponse):
    """Agent-specific queue response."""
    agent_metadata: Optional[AgentMetadata] = Field(default=None, description="Updated agent metadata")
    next_actions: List[str] = Field(default=[], description="Suggested next actions")
    blocking_issues: List[str] = Field(default=[], description="Issues blocking progress")
# Base Queue Message Model:1 ends here
