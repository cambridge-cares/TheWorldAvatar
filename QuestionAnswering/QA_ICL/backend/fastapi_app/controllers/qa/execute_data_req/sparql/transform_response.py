from functools import cache
from typing import Annotated, Dict, List, Sequence, Union

from fastapi import Depends
import pandas as pd

from services.example_store.model import SparqlNodeMappingConfig
from services.processs_response.augment_node import NodeDataRetriever
from services.processs_response.ontocompchem import get_ontocompchem_nodeDataRetriever
from services.processs_response.ontokin import get_ontokin_nodeDataRetriever
from services.processs_response.ontospecies import get_ontospecies_nodeDataRetriever


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
        vars: List[str],
        bindings: List[Dict[str, Union[str, float]]],
        res_map: Dict[str, SparqlNodeMappingConfig],
    ):
        df = pd.DataFrame(bindings, columns=vars)

        for var in vars:
            transform_config = res_map.get(var)
            if not transform_config:
                continue

            node_data_retriever = self.type2retriever.get(transform_config.cls)
            if not node_data_retriever:
                continue

            df[var] = node_data_retriever.retrieve(
                type=transform_config.cls, iris=df[var]
            )

        pkeys = [var for var, config in res_map.items() if config.pkey]
        non_pkeys = [var for var, config in res_map.items() if not config.pkey]

        return (
            df.groupby(pkeys)[non_pkeys]
            .apply(lambda gp: pd.Series({col: gp[col].dropna().to_list() for col in gp.columns}))
            .reset_index()
            .to_dict("records")
        )


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
