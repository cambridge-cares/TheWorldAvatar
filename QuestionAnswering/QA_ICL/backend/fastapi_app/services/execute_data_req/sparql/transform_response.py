from collections import defaultdict
from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends

from model.nlq2datareq import SparqlNodeMappingConfig
from services.processs_response.augment_node import NodeDataRetriever
from services.processs_response.ontocompchem import get_ontocompchem_nodeDataRetriever
from services.processs_response.ontokin import get_ontokin_nodeDataRetriever
from services.processs_response.ontospecies import get_ontospecies_nodeDataRetriever
from utils.collections import listofdict2dictoflist


class SparqlResponseTransformer:
    WKT_LITERAL_PREFIX = "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> "

    def __init__(
        self,
        retrievers: Sequence[NodeDataRetriever],
    ):
        self.type2retriever = {
            type: retriever
            for retriever in retrievers
            for type in retriever.type2getter
        }

    def transform(
        self,
        vars: list[str],
        bindings: list[dict[str, str | float]],
        res_map: dict[str, SparqlNodeMappingConfig],
    ):
        # TODO: perform aggregate before transform to reduce the complexity of 
        # calling __hash__ on FrozenDict
        for var in vars:
            transform_config = res_map.get(var)
            if not transform_config:
                continue

            node_data_retriever = self.type2retriever.get(transform_config.cls)
            if not node_data_retriever:
                continue

            retrieved_data = node_data_retriever.retrieve(
                type=transform_config.cls,
                iris=[binding.get(var) for binding in bindings],
            )
            for binding, datum in zip(bindings, retrieved_data):
                binding[var] = datum

        pkeys = [var for var, config in res_map.items() if config.pkey]
        non_pkeys = [var for var, config in res_map.items() if not config.pkey]

        pkeys2data: defaultdict[str, list[dict]] = defaultdict(list)
        for binding in bindings:
            pkeys2data[tuple(binding.get(k) for k in pkeys)].append(
                {k: binding.get(k) for k in non_pkeys}
            )

        return [
            {
                **{pkey: val for pkey, val in zip(pkeys, pkeyvalues)},
                **listofdict2dictoflist(data),
            }
            for pkeyvalues, data in pkeys2data.items()
        ]


@cache
def get_sparqlRes_transformer(
    ontospecies_retriever: Annotated[
        NodeDataRetriever, Depends(get_ontospecies_nodeDataRetriever)
    ],
    ontokin_retriever: Annotated[
        NodeDataRetriever, Depends(get_ontokin_nodeDataRetriever)
    ],
    ontocompchem_retriever: Annotated[
        NodeDataRetriever, Depends(get_ontocompchem_nodeDataRetriever)
    ],
):
    return SparqlResponseTransformer(
        retrievers=(ontospecies_retriever, ontokin_retriever, ontocompchem_retriever)
    )
