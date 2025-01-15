from collections import defaultdict
from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends

from constants.prefixes import TWA_ABOX_PREFIXES
from model.structured_answer import DocumentCollection, TableData
from services.rdf_stores import get_rdfStores
from services.rdf_stores.base import Cls2NodeGetter
from utils.collections import FrozenDict
from utils.json import deep_pd_json_normalize_list
from utils.rdf import filter_deep_remove_iris_from_list


class FallbackDataReqExecutor:
    def __init__(
        self,
        stores: Sequence[Cls2NodeGetter],
    ):
        self.cls2getter = {
            cls: getter for store in stores for cls, getter in store.cls2getter.items()
        }

    def exec(
        self,
        var2cls: dict[str, str],
        entity_bindings: dict[str, list[str]],
        const_bindings: dict[str, object],
        vis_vars: list[str],
    ):
        cls2varIrisLst: defaultdict[str, list[tuple[str, list[str]]]] = defaultdict(
            list
        )
        for var, iris in entity_bindings.items():
            cls2varIrisLst[var2cls[var]].append((var, iris))

        docs = [
            {
                "Variable": var,
                "Data": [
                    FrozenDict.from_dict(x.model_dump(exclude_none=True))
                    for x in self.cls2getter[cls](iris=iris)
                    if x
                ],
            }
            for cls, varIrisLst in cls2varIrisLst.items()
            for var, iris in varIrisLst
            if cls in self.cls2getter
        ]
        vis_var2iris: dict[str, list[str]] = {
            var: entity_bindings[var] for var in vis_vars
        }

        if not docs:
            return [], None, vis_var2iris

        docs_collection = DocumentCollection(data=docs)

        docs_no_iris = filter_deep_remove_iris_from_list(
            lst=docs, iri_prefixes=TWA_ABOX_PREFIXES
        )
        flattened_docs = deep_pd_json_normalize_list(docs_no_iris)
        table_data = TableData.from_data(flattened_docs)

        data = [docs_collection, table_data]
        artifact = docs
        
        return data, artifact, vis_var2iris


@cache
def get_fallback_executor(
    stores: Annotated[Sequence[Cls2NodeGetter], Depends(get_rdfStores)]
):
    return FallbackDataReqExecutor(stores)
