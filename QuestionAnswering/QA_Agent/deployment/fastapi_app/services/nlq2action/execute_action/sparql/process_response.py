from functools import cache
from typing import Annotated, Dict, List

from fastapi import Depends

from services.entity_store import EntityStore, get_entity_store


class SparqlResponseProcessor:
    TWA_PREFIXES = [
        "http://www.theworldavatar.com/kg/",
        "https://www.theworldavatar.com/kg/",
    ]

    def __init__(self, entity_store: EntityStore):
        self.entity_store = entity_store

    def add_labels(self, vars: List[str], bindings: List[Dict[str, str]]):
        vars_set = set(vars)

        for binding in bindings:
            new_kvs = dict()
            for k, v in binding.items():
                if any(v.startswith(prefix) for prefix in self.TWA_PREFIXES):
                    label = self.entity_store.lookup_label(v)
                    if label:
                        new_key = k + "Name"
                        if new_key not in vars_set:
                            idx = vars.index(k)
                            vars.insert(idx + 1, new_key)
                            vars_set.add(new_key)
                        new_kvs[new_key] = label
            binding.update(new_kvs)

    def process(self, vars: List[str], bindings: List[Dict[str, str]]):
        self.add_labels(vars=vars, bindings=bindings)


@cache
def get_sparqlRes_processor(
    entity_store: Annotated[EntityStore, Depends(get_entity_store)]
):
    return SparqlResponseProcessor(entity_store=entity_store)
