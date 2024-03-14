import logging
import os
from typing import Literal, Optional
from pydantic_settings import BaseSettings


logger = logging.getLogger()


class TextEmbeddingSettings(BaseSettings):
    server: Literal["openai", "triton"] = "triton"
    url: Optional[str] = None
    model: Optional[str] = None

    class Config:
        env_prefix = "TEXT_EMBEDDING_"


class FunctionCallingSettings(BaseSettings):
    url: Optional[str] = None
    model: Optional[str] = None

    class Config:
        env_prefix = "FUNCTION_CALLING_"


QA_SUPERDOMAIN = os.getenv("QA_SUPERDOMAIN", "chemistry")
TEXT_EMBEDDING_SETTINGS = TextEmbeddingSettings()
FUNCTION_CALLING_SETTINGS = FunctionCallingSettings()

logger.info("QA_SUPERDOMAIN: " + QA_SUPERDOMAIN)
logger.info("TEXT_EMBEDDING_SETTINGS: " + str(TEXT_EMBEDDING_SETTINGS))
logger.info("FUNCTION_CALLING_SETTINGS: " + str(FUNCTION_CALLING_SETTINGS))
