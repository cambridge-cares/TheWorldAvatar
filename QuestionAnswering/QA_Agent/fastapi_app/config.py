from enum import Enum
from functools import cache
import logging
import os
from typing import Literal, Optional

from pydantic import ConfigDict
from pydantic_settings import BaseSettings


logger = logging.getLogger()


class TextEmbeddingSettings(BaseSettings):
    model_config = ConfigDict(env_prefix="TEXT_EMBEDDING_", frozen=True)

    server: Literal["openai", "triton"] = "triton"
    url: Optional[str] = None
    model: Optional[str] = None


class FunctionCallingSettings(BaseSettings):
    model_config = ConfigDict(env_prefix="FUNCTION_CALLING_", frozen=True)

    url: Optional[str] = None
    model: Optional[str] = None


class QAEngine(Enum):
    MARIE = "marie"
    ZAHA = "zaha"


@cache
def get_qa_engine():
    engine = QAEngine(os.getenv("QA_ENGINE"))
    logger.info("QA_ENGINE: " + engine.value)
    return engine


@cache
def get_text_embedding_settings():
    settings = TextEmbeddingSettings()
    logger.info("TEXT_EMBEDDING_SETTINGS: " + str(settings))
    return settings


@cache
def get_function_calling_settings():
    settings = FunctionCallingSettings()
    logger.info("FUNCTION_CALLING_SETTINGS: " + str(settings))
    return settings
