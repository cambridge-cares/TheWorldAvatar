from importlib.resources import files
import json
from typing import Annotated, Dict

from fastapi import Depends
from pydantic import TypeAdapter
from redis import Redis

from services.core.redis import get_redis_client
from services.core.embed import IEmbedder, get_embedder
from services.nlq2action.link_entity.semantic import LexiconEntry, SemanticEntityLinker


class ELMediator:
    LEXICON_FILE_SUFFIX = "_lexicon.json"

    def __init__(self, redis_client: Redis, embedder: IEmbedder):
        type2lexicon: Dict[str, SemanticEntityLinker] = dict()

        adapter = TypeAdapter(LexiconEntry)
        for file in files("resources.lexicon").iterdir():
            if file.is_file() and file.name.lower().endswith(self.LEXICON_FILE_SUFFIX):
                entity_type = file.name[: -len(self.LEXICON_FILE_SUFFIX)]
                linker = SemanticEntityLinker(
                    redis_client=redis_client,
                    embedder=embedder,
                    key=entity_type,
                    entries=[adapter.validate_json(obj) for obj in file.read_text()],
                )
                type2lexicon[entity_type] = linker

        self.type2lexicon = type2lexicon

    def link(self, entity_type: str, surface_form: str):
        if entity_type not in self.type2lexicon:
            raise ValueError("Unexpected `entity_type`: " + entity_type)

        return self.type2lexicon[entity_type].link(surface_form)


def get_elMediator(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
):
    return ELMediator(redis_client=redis_client, embedder=embedder)
