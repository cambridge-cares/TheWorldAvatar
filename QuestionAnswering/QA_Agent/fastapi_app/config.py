import logging
import os
from typing import Literal, Optional
from pydantic import ConfigDict
from pydantic_settings import BaseSettings


logger = logging.getLogger()


class TextEmbeddingSettings(BaseSettings):
    model_config = ConfigDict(env_prefix="TEXT_EMBEDDING_")

    server: Literal["openai", "triton"] = "triton"
    url: Optional[str] = None
    model: Optional[str] = None


class FunctionCallingSettings(BaseSettings):
    model_config = ConfigDict(env_prefix="FUNCTION_CALLING_")

    url: Optional[str] = None
    model: Optional[str] = None


QA_SUPERDOMAIN = os.getenv("QA_SUPERDOMAIN", "chemistry")
TEXT_EMBEDDING_SETTINGS = TextEmbeddingSettings()
FUNCTION_CALLING_SETTINGS = FunctionCallingSettings()

logger.info("QA_SUPERDOMAIN: " + QA_SUPERDOMAIN)
logger.info("TEXT_EMBEDDING_SETTINGS: " + str(TEXT_EMBEDDING_SETTINGS))
logger.info("FUNCTION_CALLING_SETTINGS: " + str(FUNCTION_CALLING_SETTINGS))
