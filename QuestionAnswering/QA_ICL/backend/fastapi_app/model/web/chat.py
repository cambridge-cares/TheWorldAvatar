from pydantic import BaseModel


class ChatRequest(BaseModel):
    qa_request_id: str
