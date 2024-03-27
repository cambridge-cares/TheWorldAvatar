import logging
from typing import Annotated, Dict, Optional, Tuple

from fastapi import Depends

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
        self.ontop_client = ontop_client

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
        else:
            if constraint is ExtremeValueConstraint.MAX:
                orderby = "DESC({var})".format(var=valuenode)
            else:
                orderby = valuenode

        return where_patterns, orderby

    def find_plot_iris(
        self,
        land_use_type_iri: Optional[str] = None,
        num_constraints: Dict[PlotNumAttrKey, NumericalConstraint] = dict(),
        limit: Optional[int] = None,
    ):
        patterns = ["?IRI rdf:type ontoplot:Plot ."]
        orderbys = []

        if land_use_type_iri:
            patterns.append(
                "?IRI {pred} <{land_use}> .".format(
                    pred=LAND_USE_TYPE_PREDICATE,
                    land_use=land_use_type_iri,
                )
            )
        for key, constraint in num_constraints.items():
            where_patterns, orderby = self._make_clauses_for_constraint(key, constraint)
            patterns.extend(where_patterns)
            if orderby:
                orderbys.append(orderby)
            else:
                pass
        query = """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
PREFIX opr: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX ontozoning:<https://www.theworldavatar.com/kg/ontozoning/>
PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?IRI WHERE {{
{patterns}  
}}{orderby}{limit}""".format(
            patterns="\n".join(patterns),
            orderby="\nORDER BY " + " ".join(orderbys) if orderbys else "",
            limit="\nLIMIT " + str(limit) if limit else "",
        )

        logger.info("SPARQL query:\n" + query)

        return [
            x["IRI"]["value"]
            for x in self.ontop_client.query(query)["results"]["bindings"]
        ]

    def lookup_plot_attribute(
        self,
        attr_key: PlotAttrKey,
        land_use_type_iri: Optional[str] = None,
        num_constraints: Dict[PlotNumAttrKey, NumericalConstraint] = dict(),
    ):
        iris = self.find_plot_iris(
            land_use_type_iri=land_use_type_iri, num_constraints=num_constraints
        )
        if not iris:
            return QAData()

        patterns = [
            "VALUES ?IRI {{ {values} }}".format(
                values=" ".join(["<{iri}>".format(iri=iri) for iri in iris])
            )
        ]
        vars = ["?IRI"]
        if attr_key is PlotCatAttrKey.LAND_USE_TYPE:
            patterns.append("?IRI ontozoning:hasLandUseType ?LandUseType .")
            patterns.append(
                "SERVICE <{bg}> {{ ?LandUseType rdfs:label ?LandUseTypeLabel }} ".format(
                    bg=self.bg_client.sparql.endpoint
                )
            )
            vars.append("?LandUseTypeLabel")
        elif attr_key is PlotNumAttrKey.GROSS_PLOT_RATIO:
            patterns.append(
                """
OPTIONAL {{
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
            patterns.append(
                "?IRI {pred} [ om:hasNumericalValue ?{key} ; om:hasUnit ?{key}Unit ] .".format(
                    pred=PLOT_NUM_ATTR_PREDICATES[attr_key], key=attr_key.value
                )
            )
            vars.append("?" + attr_key.value)

        query = """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
PREFIX opr: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX ontozoning:<https://www.theworldavatar.com/kg/ontozoning/>
PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT {vars} WHERE {{
{patterns}
}}
""".format(
            vars=" ".join(vars), patterns="\n".join(patterns)
        )

        res = self.ontop_client.query(query)
        vars = res["head"]["vars"]
        bindings = [
            {k: v["value"] for k, v in binding.items()}
            for binding in res["results"]["bindings"]
        ]
        return QAData(vars=vars, bindings=bindings)

    def count_plots(
        self,
        land_use_type_iri: Optional[str] = None,
        num_constraints: Dict[PlotNumAttrKey, NumericalConstraint] = dict(),
    ):
        iris = self.find_plot_iris(land_use_type_iri, num_constraints)
        return QAData(vars=["count"], bindings=[dict(count=len(iris))])

    def compute_aggregate_plot_attribute(
        self,
        attr_agg: Tuple[PlotNumAttrKey, AggregateOperator],
        land_use_type_iri: Optional[str] = None,
        num_constraints: Dict[PlotNumAttrKey, NumericalConstraint] = dict(),
    ):
        iris = self.find_plot_iris(land_use_type_iri, num_constraints)
        vars = []
        patterns = [
            "VALUES ?IRI {{ {values} }}".format(
                values=" ".join(["<{iri}>".format(iri=iri) for iri in iris])
            )
        ]
        key, agg = attr_agg
        func = agg.value
        valuenode = "?{key}NumericalValue".format(key=key.value)

        vars.append(
            "({func}({valuenode}) AS {valuenode}{func})".format(
                func=func, valuenode=valuenode
            )
        )
        patterns.append(
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
            vars=" ".join(vars), patterns="\n".join(patterns)
        )

        res = self.ontop_client.query(query)
        vars = res["head"]["vars"]
        bindings = [
            {k: v["value"] for k, v in binding.items()}
            for binding in res["results"]["bindings"]
        ]

        return QAData(vars=vars, bindings=bindings)


def get_sgLandLots_agent(
    bg_client: Annotated[KgClient, Depends(get_sgLandLots_bgClient)],
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
):
    return SGLandLotsAgent(
        bg_client=bg_client,
        ontop_client=ontop_client,
    )
