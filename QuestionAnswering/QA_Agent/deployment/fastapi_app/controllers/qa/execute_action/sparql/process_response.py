from functools import cache
from typing import Annotated, Dict, List, Union

from fastapi import Depends

from services.entity_store import EntityStore, get_entity_store
from services.entity_store.species import SpeciesStore, get_species_store
from services.wkt import CRS84_URI, WKTTextSRS
from controllers.qa.model import DataItem, TableDataItem, WktCrs84DataItem


class SparqlResponseProcessor:
    TWA_PREFIXES = [
        "http://www.theworldavatar.com/kg/",
        "https://www.theworldavatar.com/kg/",
        "http://www.theworldavatar.com/kb/",
    ]
    WKT_LITERAL_PREFIX = "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> "

    def __init__(self, entity_store: EntityStore, species_store: SpeciesStore):
        self.entity_store = entity_store
        self.species_store = species_store

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

        items: List[Union[TableDataItem, WktCrs84DataItem]] = []
        for binding in bindings:
            table_vars = list(not_wkt_vars)
            table_binding = {
                k: v["value"] for k, v in binding.items() if k in not_wkt_vars
            }

            wkt_items: List[WktCrs84DataItem] = []
            for var in wkt_vars:
                wkt_text_srs = WKTTextSRS.from_literal(binding[var]["value"])
                if wkt_text_srs.srs_uri == CRS84_URI:
                    wkt_items.append(WktCrs84DataItem(wkt_text=wkt_text_srs.wkt_text))
                else:
                    table_vars.append(var)
                    table_binding[var] = binding[var]["value"]

            items.append(TableDataItem(vars=table_vars, bindings=[table_binding]))
            items.extend(wkt_items)

        return items

    def add_labels(self, item: TableDataItem):
        vars_set = set(item.vars)

        for binding in item.bindings:
            new_kvs = dict()
            for k, v in binding.items():
                if not isinstance(v, str) or not any(
                    v.startswith(prefix) for prefix in self.TWA_PREFIXES
                ):
                    continue

                label_key = k + "Label"
                if label_key in binding.keys():
                    continue

                label = self.entity_store.lookup_label(v)
                if not label:
                    continue

                if label_key not in vars_set:
                    idx = item.vars.index(k)
                    item.vars.insert(idx + 1, label_key)
                    vars_set.add(label_key)
                new_kvs[label_key] = label

            binding.update(new_kvs)

    def add_iupac_names(self, item: TableDataItem):
        vars_set = set(item.vars)

        for binding in item.bindings:
            new_kvs = dict()
            for k, v in binding.items():
                if (
                    not isinstance(v, str)
                    or not any(v.startswith(prefix) for prefix in self.TWA_PREFIXES)
                    or self.entity_store.lookup_clsname(v) != "Species"
                ):
                    continue

                iupac_key = k + "IUPACName"
                if iupac_key in binding.keys():
                    continue

                name = self.species_store.lookup_iupac_name(v)
                if not name:
                    continue

                if iupac_key not in vars_set:
                    idx = item.vars.index(k)
                    item.vars.insert(idx + 1, iupac_key)
                    vars_set.add(iupac_key)
                new_kvs[iupac_key] = name

            binding.update(new_kvs)

    def process(self, vars: List[str], bindings: List[Dict[str, Dict[str, str]]]):
        items = self.extract_wkt(vars, bindings)

        for item in items:
            if isinstance(item, DataItem):
                self.add_labels(item)
                self.add_iupac_names(item)

        return items


@cache
def get_sparqlRes_processor(
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    species_store: Annotated[SpeciesStore, Depends(get_species_store)],
):
    return SparqlResponseProcessor(
        entity_store=entity_store, species_store=species_store
    )
