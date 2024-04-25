from functools import cache
from importlib.resources import files
from typing import Annotated, List, Literal, Tuple

from fastapi import Depends
from pydantic import TypeAdapter
from pydantic.dataclasses import dataclass
from redis import Redis

from config import QAEngineName, get_qa_engine_name
from services.core.redis import get_redis_client
from services.core.embed import IEmbedder, get_embedder
from .fuzzy import FuzzyEntityLinker
from .semantic import LexiconEntry, SemanticEntityLinker


@dataclass(frozen=True)
class ELConfigEntry:
    entity_type: str
    el_strategy: Literal["semantic", "fuzzy"]


class ELMediator:
    LEXICON_FILE_SUFFIX = "_lexicon.json"

    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
        config: Tuple[ELConfigEntry, ...],
    ):
        adapter = TypeAdapter(List[LexiconEntry])

        def load_linker(config_entry: ELConfigEntry):
            path = files("resources.lexicon").joinpath(
                config_entry.entity_type + self.LEXICON_FILE_SUFFIX
            )
            lexicon = adapter.validate_json(path.read_text())

            if config_entry.el_strategy == "fuzzy":
                return FuzzyEntityLinker(
                    redis_client=redis_client,
                    key=config_entry.entity_type,
                    lexicon=lexicon,
                )
            else:
                return SemanticEntityLinker(
                    redis_client=redis_client,
                    embedder=embedder,
                    key=config_entry.entity_type,
                    lexicon=lexicon,
                )

        self.type2linker = {
            config.entity_type: load_linker(config) for config in config
        }

    def link(self, entity_type: str, surface_form: str):
        if entity_type not in self.type2linker:
            raise ValueError("Unexpected `entity_type`: " + entity_type)

        return self.type2linker[entity_type].link(surface_form)


@cache
def get_el_config(qa_engine: Annotated[QAEngineName, Depends(get_qa_engine_name)]):
    adapter = TypeAdapter(Tuple[ELConfigEntry, ...])
    return adapter.validate_json(
        files("resources." + qa_engine.value).joinpath("el_config.json").read_text()
    )


@cache
def get_el_mediator(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    config: Annotated[Tuple[ELConfigEntry, ...], Depends(get_el_config)],
):
    return ELMediator(redis_client=redis_client, embedder=embedder, config=config)
