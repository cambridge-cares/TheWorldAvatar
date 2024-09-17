from typing import Literal
from pydantic import BaseModel, ConfigDict


ENTITIES_KEY_PREFIX = "entities:"
ENTITIES_INDEX_NAME = "idx:entities"


class LexiconEntry(BaseModel):
    iri: str
    cls: str
    label: str
    surface_forms: list[str] = []


class LexiconEntryProcessed(LexiconEntry):
    surface_forms_embedding: list[float] | None = None


ELStrategy = Literal["fuzzy", "semantic"]


class ELConfig(BaseModel):
    model_config = ConfigDict(frozen=True)

    strategy: ELStrategy = "fuzzy"
    k: int = 3


class ELConfigEntry(BaseModel):
    model_config = ConfigDict(frozen=True)

    cls: str
    el_config: ELConfig
