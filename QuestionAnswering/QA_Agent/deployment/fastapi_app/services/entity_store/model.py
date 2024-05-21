from typing import List, Literal
from pydantic import BaseModel, ConfigDict


class LexiconEntry(BaseModel):
    iri: str
    clsname: str
    label: str
    surface_forms: List[str] = []


class LexiconEntryProcessed(LexiconEntry):
    surface_forms_embedding: List[float]


ELStrategy = Literal["fuzzy", "semantic"]


class ELConfig(BaseModel):
    model_config = ConfigDict(frozen=True)
    
    strategy: ELStrategy = "fuzzy"
    k: int = 3


class ELConfigEntry(BaseModel):
    model_config = ConfigDict(frozen=True)
    
    clsname: str
    el_config: ELConfig
