from functools import cache
from typing import Annotated, Dict, List, Union

from fastapi import Depends

from constants.prefixes import TWA_ABOX_PREFIXES
from services.entity_store import EntityStore, get_entity_store
from services.example_store.model import TypedVarNode
from services.model import TableDataItem
from services.processs_response.augment_table import TableAugmenter, get_table_augmenter


class SparqlResponseProcessor:
    WKT_LITERAL_PREFIX = "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> "

    def __init__(self, entity_store: EntityStore, table_augmenter: TableAugmenter):
        self.entity_store = entity_store
        self.table_augmenter = table_augmenter

    def add_labels(
        self, vars: List[str], bindings: List[Dict[str, Union[str, float, dict]]]
    ):
        vars_set = set(vars)

        for binding in bindings:
            new_kvs = dict()
            for k, v in binding.items():
                if not isinstance(v, str) or not any(
                    v.startswith(prefix) for prefix in TWA_ABOX_PREFIXES
                ):
                    continue

                label_key = k + "Label"
                if label_key in binding.keys():
                    continue

                label = self.entity_store.lookup_label(v)
                if not label:
                    continue

                if label_key not in vars_set:
                    idx = vars.index(k)
                    vars.insert(idx + 1, label_key)
                    vars_set.add(label_key)
                new_kvs[label_key] = label

            binding.update(new_kvs)

    def process(
        self,
        nodes_to_augment: List[TypedVarNode],
        vars: List[str],
        bindings: List[Dict[str, Union[str, float, dict]]],
    ):
        self.table_augmenter.augment(
            nodes_to_augment=nodes_to_augment, vars=vars, bindings=bindings
        )
        self.add_labels(vars=vars, bindings=bindings)


@cache
def get_sparqlRes_processor(
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    table_augmenter: Annotated[TableAugmenter, Depends(get_table_augmenter)],
):
    return SparqlResponseProcessor(
        entity_store=entity_store, table_augmenter=table_augmenter
    )
