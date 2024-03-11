from functools import cache
import os
from typing import Literal, Optional
from pydantic_settings import BaseSettings


class TextEmbeddingSettings(BaseSettings):
    server: Literal["openai", "triton"]
    url: Optional[str] = None
    model: Optional[str] = None


@cache
def get_text_embedding_settings():
    return TextEmbeddingSettings(
        server=os.getenv("TEXT_EMBEDDING_SERVER"),
        url=os.getenv("TEXT_EMBEDDING_URL"),
        model=os.getenv("TEXT_EMBEDDING_MODEL"),
    )


class FunctionCallingSettings(BaseSettings):
    server: Literal["openai"] = "openai"
    url: Optional[str] = None
    model: Optional[str] = None


@cache
def get_function_calling_settings():
    return FunctionCallingSettings(
        server=os.getenv("FUNCTION_CALLING_SERVER", "openai"),
        url=os.getenv("FUNCTION_CALLING_URL"),
        model=os.getenv("FUNCTION_CALLING_MODEL"),
    )
