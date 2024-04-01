from functools import cache
import logging
from typing import Annotated, List, Optional, Tuple

from fastapi import Depends

from services.utils.rdf import flatten_sparql_response
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
        bg_client: KgClient,
        ontop_client: KgClient,
        sparql_maker: SGLandLotsSPARQLMaker,
    ):
        self.bg_client = bg_client
        # self.ontop_client = ontop_client
        self.ontop_client = KgClient("http://174.138.23.221:3838/ontop/ui/sparql")
        self.sparql_maker = sparql_maker

    @cache
    def _landUse_clsname2iris(self, land_use_type: Optional[str]):
        if not land_use_type:
            return []
        
        query = """PREFIX ontozoning: <https://www.theworldavatar.com/kg/ontozoning/>

SELECT ?IRI WHERE {{
?IRI a ontozoning:{land_use} .        
}}""".format(
            land_use=land_use_type
        )

        return [
            binding["IRI"]["value"]
            for binding in self.bg_client.query(query)["results"]["bindings"]
        ]
    
    def _add_landUseType(self, vars: List[str], bindings: List[dict], land_use_type: Optional[str]):
        if not land_use_type:
            return
        
        try:
            idx = vars.index("LandUseTypeIRI")
            vars.insert(idx + 1, "LandUseType")
            for binding in bindings:
                binding["LandUseType"] = land_use_type
        except:
            pass
    
    def lookup_plot_attribute(
        self,
        attr_key: PlotAttrKey,
        land_use_type: Optional[str] = None,
    ):
        land_use_type_iris = self._landUse_clsname2iris(land_use_type)
        query = self.sparql_maker.lookup_plot_attribute(attr_key, land_use_type_iris)

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)
        self._add_landUseType(vars, bindings, land_use_type)

        return QAData(vars=vars, bindings=bindings)

    def count_plots(self, land_use_type: Optional[str] = None):
        land_use_type_iris = self._landUse_clsname2iris(land_use_type)
        query = self.sparql_maker.count_plots(land_use_type_iris)
        print(query)
        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)
        self._add_landUseType(vars, bindings, land_use_type)

        return QAData(vars=vars, bindings=bindings)

    def compute_aggregate_plot_attribute(
        self,
        attr_agg: Tuple[PlotNumAttrKey, AggregateOperator],
        land_use_type: Optional[str] = None,
    ):
        land_use_type_iris = self._landUse_clsname2iris(land_use_type)
        query = self.sparql_maker.compute_aggregate_plot_attribute(attr_agg, land_use_type_iris)

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)
        self._add_landUseType(vars, bindings, land_use_type)

        return QAData(vars=vars, bindings=bindings)


def get_sgLandLots_agent(
    bg_client: Annotated[KgClient, Depends(get_sgLandLots_bgClient)],
    sparql_maker: Annotated[SGLandLotsSPARQLMaker, Depends(get_sgLandLots_sparqlMaker)],
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
):
    return SGLandLotsAgent(
        bg_client=bg_client,
        sparql_maker=sparql_maker,
        ontop_client=ontop_client,
    )
