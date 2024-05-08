from functools import cache
from typing import Annotated, Dict, List, Union

from fastapi import Depends

from services.entity_store import EntityStore, get_entity_store
from controllers.qa.support_data import DataItem, TableDataItem, WktDataItem


class SparqlResponseProcessor:
    TWA_PREFIXES = [
        "http://www.theworldavatar.com/kg/",
        "https://www.theworldavatar.com/kg/",
    ]
    WKT_LITERAL_PREFIX = "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> "

    def __init__(self, entity_store: EntityStore):
        self.entity_store = entity_store

    def add_labels(self, items: List[DataItem]):
        for item in items:
            if not isinstance(item, TableDataItem):
                continue

            vars_set = set(item.vars)

            for binding in item.bindings:
                new_kvs = dict()
                for k, v in binding.items():
                    if not isinstance(v, str) or not any(
                        v.startswith(prefix) for prefix in self.TWA_PREFIXES
                    ):
                        continue

                    label = self.entity_store.lookup_label(v)
                    if not label:
                        continue

                    new_key = k + "Name"
                    if new_key not in vars_set:
                        idx = item.vars.index(k)
                        item.vars.insert(idx + 1, new_key)
                        vars_set.add(new_key)
                    new_kvs[new_key] = label

                binding.update(new_kvs)

    def extract_wkt(self, vars: List[str], bindings: List[Dict[str, Dict[str, str]]]):
        wkt_vars = [
            var
            for var in vars
            if any(
                binding.get(var, {}).get("datatype")
                == "http://www.opengis.net/ont/geosparql#wktLiteral"
                for binding in bindings
            )
        ]

        if not wkt_vars:
            return [
                TableDataItem(
                    vars=vars,
                    bindings=[
                        {k: v["value"] for k, v in binding.items()}
                        for binding in bindings
                    ],
                )
            ]

        not_wkt_vars = [var for var in vars if var not in wkt_vars]

        items: List[Union[TableDataItem, WktDataItem]] = []
        for binding in bindings:
            items.append(
                TableDataItem(
                    vars=not_wkt_vars,
                    bindings=[
                        {k: v["value"] for k, v in binding.items() if k in not_wkt_vars}
                    ],
                )
            )
            items.extend(
                WktDataItem.from_literal(binding[var]["value"]) for var in wkt_vars
            )

        return items

    def process(self, vars: List[str], bindings: List[Dict[str, Dict[str, str]]]):
        items = self.extract_wkt(vars, bindings)
        self.add_labels(items)
        return items


@cache
def get_sparqlRes_processor(
    entity_store: Annotated[EntityStore, Depends(get_entity_store)]
):
    return SparqlResponseProcessor(entity_store=entity_store)
