from functools import cache
import logging
from typing import Annotated, Optional, Tuple

from fastapi import Depends

from services.utils.rdf import flatten_sparql_response
from model.aggregate import AggregateOperator
from model.constraint import (
    CompoundComparativeConstraint,
    NumericalConstraint,
    ExtremeValueConstraint,
)
from model.qa import QAData
from services.core.kg import KgClient
from services.connectors.sg import get_sg_ontopClient
from .constants import LAND_USE_TYPE_PREDICATE, PLOT_NUM_ATTR_PREDICATES
from .model import PlotAttrKey, PlotCatAttrKey, PlotNumAttrKey
from .kg import get_sgLandLots_bgClient

logger = logging.getLogger(__name__)


class SGLandLotsAgent:
    def __init__(
        self,
        bg_client: KgClient,
        ontop_client: KgClient,
    ):
        self.bg_client = bg_client
        # self.ontop_client = ontop_client
        self.ontop_client = KgClient("http://174.138.23.221:3838/ontop/ui/sparql")

    def _make_clauses_for_constraint(
        self, key: PlotNumAttrKey, constraint: NumericalConstraint
    ):
        where_patterns = []
        orderby = None

        valuenode = "?{key}NumericalValue".format(key=key.value)
        where_patterns.append(
            "?IRI {pred}/om:hasNumericalValue {valuenode} .".format(
                pred=PLOT_NUM_ATTR_PREDICATES[key], valuenode=valuenode
            )
        )

        if isinstance(constraint, CompoundComparativeConstraint):
            atomic_constraints = [
                "{valuenode} {operator} {operand}".format(
                    valuenode=valuenode, operator=x.operator.value, operand=x.operand
                )
                for x in constraint.constraints
            ]
            if constraint.logical_operator:
                delimiter = constraint.logical_operator.value
            else:
                delimiter = "&&"
            exprn = delimiter.join(atomic_constraints)
            filter_pattern = "FILTER ( {exprn} )".format(exprn=exprn)
            where_patterns.append(filter_pattern)
        elif constraint is ExtremeValueConstraint.MAX:
            orderby = "DESC({var})".format(var=valuenode)
        else:
            orderby = valuenode

        return where_patterns, orderby

    @cache
    def _landUse_clsname2iri(self, land_use_type: str):
        query = """PREFIX ontozoning: <https://www.theworldavatar.com/kg/ontozoning/>

SELECT ?IRI WHERE {{
?IRI a ontozoning:{land_use} .        
}}""".format(
            land_use=land_use_type
        )
        print(query)
        return [
            binding["IRI"]["value"]
            for binding in self.bg_client.query(query)["results"]["bindings"]
        ]

    def _make_clauses_to_locate_plots(self, land_use_type: Optional[str] = None):
        ontop_patterns = ["?IRI rdf:type ontoplot:Plot ."]
        if land_use_type:
            ontop_patterns.extend(
                [
                    "VALUES ?LandUseType {{ {values} }}".format(
                        values=" ".join(
                            [
                                "<{iri}>".format(iri=iri)
                                for iri in self._landUse_clsname2iri(land_use_type)
                            ]
                        )
                    ),
                    "?IRI {pred} ?LandUseType .".format(pred=LAND_USE_TYPE_PREDICATE),
                ]
            )
        return ontop_patterns

    def lookup_plot_attribute(
        self,
        attr_key: PlotAttrKey,
        land_use_type: Optional[str] = None,
    ):
        ontop_patterns = self._make_clauses_to_locate_plots(land_use_type)
        vars = ["?IRI"]
        if attr_key is PlotCatAttrKey.LAND_USE_TYPE:
            ontop_patterns.append("?IRI ontozoning:hasLandUseType ?LandUseType .")
            vars.append("?LandUseType")
        elif attr_key is PlotNumAttrKey.GROSS_PLOT_RATIO:
            ontop_patterns.append(
                """OPTIONAL {{
    ?IRI {pred} ?gpr . 
}}
OPTIONAL {{
    ?IRI opr:isAwaitingDetailedGPREvaluation ?awaiting_detailed_evaluation .
}}
BIND(IF(BOUND(?gpr), ?gpr, IF(?awaiting_detailed_evaluation = true, "Awaiting detailed evaluation", "")) AS ?{key})""".format(
                    pred=PLOT_NUM_ATTR_PREDICATES[attr_key], key=attr_key.value
                )
            )
            vars.append("?" + attr_key.value)
        else:
            ontop_patterns.append(
                "?IRI {pred} [ om:hasNumericalValue ?{key} ; om:hasUnit ?{key}Unit ] .".format(
                    pred=PLOT_NUM_ATTR_PREDICATES[attr_key], key=attr_key.value
                )
            )
            vars.append("?" + attr_key.value)

        query = """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
PREFIX opr: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX ontozoning: <https://www.theworldavatar.com/kg/ontozoning/>
PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT {vars} WHERE {{
{patterns}
}}""".format(
            vars=" ".join(vars), patterns="\n".join(ontop_patterns)
        )

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        return QAData(vars=vars, bindings=bindings)

    def count_plots(self, land_use_type: Optional[str] = None):
        ontop_patterns = self._make_clauses_to_locate_plots(land_use_type)
        vars = ["(COUNT(?IRI) as ?Count)"]

        query = """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
PREFIX opr: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX ontozoning:<https://www.theworldavatar.com/kg/ontozoning/>
PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT {vars} WHERE {{
{patterns}
}}""".format(
            vars=" ".join(vars), patterns="\n".join(ontop_patterns)
        )
        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        return QAData(vars=vars, bindings=bindings)

    def compute_aggregate_plot_attribute(
        self,
        attr_agg: Tuple[PlotNumAttrKey, AggregateOperator],
        land_use_type: Optional[str] = None,
    ):
        ontop_patterns = self._make_clauses_to_locate_plots(land_use_type)
        vars = []

        key, agg = attr_agg
        func = agg.value
        valuenode = "?{key}NumericalValue".format(key=key.value)

        vars.append(
            "({func}({valuenode}) AS {valuenode}{func})".format(
                func=func, valuenode=valuenode
            )
        )
        ontop_patterns.append(
            "?IRI {pred}/om:hasNumericalValue {valuenode} .".format(
                pred=PLOT_NUM_ATTR_PREDICATES[key], valuenode=valuenode
            )
        )

        query = """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
PREFIX opr: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX ontozoning:<https://www.theworldavatar.com/kg/ontozoning/>
PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT {vars} WHERE {{
{patterns} 
}}""".format(
            vars=" ".join(vars), patterns="\n".join(ontop_patterns)
        )
        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        return QAData(vars=vars, bindings=bindings)


def get_sgLandLots_agent(
    bg_client: Annotated[KgClient, Depends(get_sgLandLots_bgClient)],
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
):
    return SGLandLotsAgent(
        bg_client=bg_client,
        ontop_client=ontop_client,
    )
