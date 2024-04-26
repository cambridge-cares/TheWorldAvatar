import logging
from typing import Annotated, Any, Dict, List, Tuple

from fastapi import Depends

from services.entity_store import EntityStore, get_entity_linker
from utils.rdf import extract_name, flatten_sparql_response
from model.aggregate import AggregateOperator
from model.qa import QAData
from core.kg import KgClient
from services.kg import get_sg_ontopClient, get_sgPlot_bgClient
from ..model import PlotNumAttrKey
from .make_sparql import SGLandLotsSPARQLMaker, get_sgLandLots_sparqlMaker

logger = logging.getLogger(__name__)


class SGLandLotsAgent:
    def __init__(
        self,
        ontop_client: KgClient,
        bg_client: KgClient,
        entity_linker: EntityStore,
        sparql_maker: SGLandLotsSPARQLMaker,
    ):
        self.ontop_client = ontop_client
        self.bg_client = bg_client
        self.entity_linker = entity_linker
        self.sparql_maker = sparql_maker

    def _add_landUseType(self, vars: List[str], bindings: List[dict]):
        try:
            idx = vars.index("LandUseTypeIRI")
            vars.insert(idx + 1, "LandUseType")
            for binding in bindings:
                binding["LandUseType"] = self.entity_linker.lookup_label(
                    binding["LandUseTypeIRI"]
                )
        except:
            pass

    def _process_unit_iri(self, bindings: List[Dict[str, Any]]):
        for binding in bindings:
            for k, v in binding.items():
                if k.endswith("Unit") and isinstance(v, str):
                    binding[k] = extract_name(v)

    def explain_landUses(self, land_use_types: List[str]):
        query = self.sparql_maker.explain_landUses(land_use_types)
        res = self.bg_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        return QAData(vars=vars, bindings=bindings)

    def count_plots(self, land_use_types: List[str] = []):
        query = self.sparql_maker.count_plots(land_use_types)

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
        query = self.sparql_maker.compute_aggregate_plot_attribute(
            attr_agg, land_use_types
        )

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        self._add_landUseType(vars, bindings)
        self._process_unit_iri(bindings)

        return QAData(vars=vars, bindings=bindings)


def get_sgLandLots_agent(
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
    bg_client: Annotated[KgClient, Depends(get_sgPlot_bgClient)],
    entity_linker: Annotated[EntityStore, Depends(get_entity_linker)],
    sparql_maker: Annotated[SGLandLotsSPARQLMaker, Depends(get_sgLandLots_sparqlMaker)],
):
    return SGLandLotsAgent(
        ontop_client=ontop_client,
        bg_client=bg_client,
        entity_linker=entity_linker,
        sparql_maker=sparql_maker,
    )
