from collections import defaultdict
from functools import cache
import logging
from typing import Annotated, Any, Dict, List, Optional, Tuple

from fastapi import Depends

from services.connectors.sg_land_lots.land_use import LandUseTypeStore, get_landUseType_store
from services.utils.rdf import extract_name, flatten_sparql_response
from model.aggregate import AggregateOperator
from model.qa import QAData
from services.core.kg import KgClient
from services.connectors.sg import get_sg_ontopClient
from ..model import PlotAttrKey, PlotNumAttrKey
from ..kg import get_sgLandLots_bgClient
from .make_sparql import SGLandLotsSPARQLMaker, get_sgLandLots_sparqlMaker

logger = logging.getLogger(__name__)


class SGLandLotsAgent:
    def __init__(
        self,
        ontop_client: KgClient,
        land_use_type_store: LandUseTypeStore,
        sparql_maker: SGLandLotsSPARQLMaker,
    ):
        # self.ontop_client = ontop_client
        self.ontop_client = KgClient("http://174.138.23.221:3838/ontop/ui/sparql")
        self.land_use_type_store = land_use_type_store
        self.sparql_maker = sparql_maker

    def _add_landUseType(self, vars: List[str], bindings: List[dict]):
        try:
            idx = vars.index("LandUseTypeIRI")
            vars.insert(idx + 1, "LandUseType")
            for binding in bindings:
                binding["LandUseType"] = ", ".join(
                    self.land_use_type_store.get_labels(binding["LandUseTypeIRI"])
                )
        except:
            pass

    def _process_unit_iri(self, bindings: List[Dict[str, Any]]):
        for binding in bindings:
            for k, v in binding.items():
                if k.endswith("Unit") and isinstance(v, str):
                    binding[k] = extract_name(v)

    def count_plots(self, land_use_types: List[str] = []):
        landUseType_iris = [
            iri
            for clsname in land_use_types
            for iri in self.land_use_type_store.get_iris(clsname)
        ]

        query = self.sparql_maker.count_plots(landUseType_iris)

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        self._add_landUseType(vars, bindings)
        self._process_unit_iri(bindings)

        return QAData(vars=vars, bindings=bindings)

    def compute_aggregate_plot_attribute(
        self,
        attr_agg: Tuple[PlotNumAttrKey, AggregateOperator],
        land_use_types: List[str] = [],
    ):
        landUseType_iris = [
            iri
            for clsname in land_use_types
            for iri in self.land_use_type_store.get_iris(clsname)
        ]
        query = self.sparql_maker.compute_aggregate_plot_attribute(
            attr_agg, landUseType_iris
        )

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        self._add_landUseType(vars, bindings)
        self._process_unit_iri(bindings)

        return QAData(vars=vars, bindings=bindings)


def get_sgLandLots_agent(
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
    land_use_type_store: Annotated[LandUseTypeStore, Depends(get_landUseType_store)],
    sparql_maker: Annotated[SGLandLotsSPARQLMaker, Depends(get_sgLandLots_sparqlMaker)],
):
    return SGLandLotsAgent(
        ontop_client=ontop_client,
        land_use_type_store=land_use_type_store,
        sparql_maker=sparql_maker,
    )
