from typing import Iterable, List

from redis import Redis

from services.core.label_store import IRIWithLabels, LabelStore
from services.nlq2action.link_entity.semantic import LexiconEntry
from .link import IEntityLinker


class FuzzyEntityLinker(IEntityLinker):
    def __init__(self, redis_client: Redis, key: str, lexicon: List[LexiconEntry]):
        bindings = [
            IRIWithLabels(IRI=entry.iri, labels=[entry.label] + entry.surface_forms)
            for entry in lexicon
        ]
        self.label_store = LabelStore(
            redis_client=redis_client, key=key, bindings=bindings
        )

    def link(self, surface_form: str):
        return self.label_store.link_entity(surface_form)
