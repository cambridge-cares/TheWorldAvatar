from functools import cache
import os
from typing import Literal
from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    embedding_service: Literal["openai", "triton"]
    function_calling_service: Literal["openai"]


@cache
def get_settings():
    return Settings(
        embedding_service=os.getenv("EMBEDDING_SERVICE"),
        function_calling_service=os.getenv("FUNCTION_CALLING_SERVICE", "openai"),
    )
