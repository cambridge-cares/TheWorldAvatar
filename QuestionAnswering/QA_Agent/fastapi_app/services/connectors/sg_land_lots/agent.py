import logging
from typing import Annotated, Iterator, List, Optional, Tuple

from fastapi import Depends
from pydantic.dataclasses import dataclass

from model.aggregate import AggregateOperator
from model.constraint import (
    CompoundNumericalConstraint,
    NumericalArgConstraint,
    ExtremeValueConstraint,
)
from model.qa import QAData
from services.core.kg import KgClient
from services.connectors.sg import get_sg_ontopClient
from .constants import PlotAttrKey
from .kg import get_sgLandLots_bgClient

logger = logging.getLogger(__name__)


@dataclass
class PlotConstraints:
    land_use_type_iri: Optional[str] = None
    gross_plot_ratio: Optional[NumericalArgConstraint] = None
    plot_area: Optional[NumericalArgConstraint] = None
    gross_floor_area: Optional[NumericalArgConstraint] = None
    num: Optional[int] = None

    def __str__(self):
        agg = []
        if self.land_use_type_iri:
            agg.append("land_use_type_iri='{iri}'".format(iri=self.land_use_type_iri))
        for field in ["gross_plot_ratio", "plot_area", "gross_floor_area"]:
            if getattr(self, field):
                agg.append(str(getattr(self, field)))
        return ", ".join(agg)


class SGLandLotsAgent:
    _ATTRKEY2PRED = {
        PlotAttrKey.LAND_USE_TYPE: "ontozoning:hasLandUseType",
        PlotAttrKey.GROSS_PLOT_RATIO: "^opr:appliesTo/opr:allowsGrossPlotRatio/om:hasValue",
        PlotAttrKey.PLOT_AREA: "ontoplot:hasPlotArea/om:hasValue",
        PlotAttrKey.GROSS_FLOOR_AREA: "ontoplot:hasMaximumPermittedGPR/om:hasValue",
    }

    def __init__(
        self,
        bg_client: KgClient,
        ontop_client: KgClient,
    ):
        self.bg_client = bg_client
        self.ontop_client = ontop_client

    def _make_clauses_for_constraint(
        self, key: PlotAttrKey, constraint: NumericalArgConstraint
    ):
        where_patterns = []
        orderby = None

        valuenode = "?{key}NumericalValue".format(key=key.value)
        where_patterns.append(
            "?IRI {pred}/om:hasNumericalValue {valuenode} .".format(
                pred=self._ATTRKEY2PRED[key], valuenode=valuenode
            )
        )

        if isinstance(constraint, CompoundNumericalConstraint):
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

    def find_plot_iris(self, plot_constraints: PlotConstraints):
        patterns = ["?IRI rdf:type ontoplot:Plot ."]
        orderbys = []

        if plot_constraints.land_use_type_iri:
            patterns.append(
                "?IRI {pred} <{land_use}> .".format(
                    pred=self._ATTRKEY2PRED[PlotAttrKey.LAND_USE_TYPE],
                    land_use=plot_constraints.land_use_type_iri,
                )
            )
        for fieldname, key in [
            ("gross_plot_ratio", PlotAttrKey.GROSS_PLOT_RATIO),
            ("plot_area", PlotAttrKey.PLOT_AREA),
            ("gross_floor_area", PlotAttrKey.GROSS_FLOOR_AREA),
        ]:
            field = getattr(plot_constraints, fieldname)
            where_patterns, orderby = self._make_clauses_for_constraint(key, field)
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
}}""".format(
            patterns="\n".join(patterns)
        )

        if orderbys:
            query += "\nORDER BY " + " ".join(orderbys)

        if plot_constraints.num:
            query += "\nLIMIT " + str(plot_constraints.num)

        logger.info("SPARQL query:\n" + query)

        return [
            x["IRI"]["value"]
            for x in self.ontop_client.query(query)["results"]["bindings"]
        ]

    def lookup_plot_attribute(
        self, plot_constraints: PlotConstraints, attr_key: PlotAttrKey
    ):
        iris = self.find_plot_iris(plot_constraints)
        if not iris:
            return QAData()

        patterns = [
            "VALUES ?IRI {{ {values} }}".format(
                values=" ".join(["<{iri}>".format(iri=iri) for iri in iris])
            )
        ]
        vars = ["?IRI"]
        if attr_key is PlotAttrKey.LAND_USE_TYPE:
            patterns.append("?IRI ontozoning:hasLandUseType ?LandUseType .")
            patterns.append(
                "SERVICE <{bg}> {{ ?LandUseType rdfs:label ?LandUseTypeLabel }} ".format(
                    bg=self.bg_client.sparql.endpoint
                )
            )
            vars.append("?LandUseTypeLabel")
        elif attr_key is PlotAttrKey.GROSS_PLOT_RATIO:
            patterns.append(
                """
OPTIONAL {{
    ?IRI {pred} ?gpr . 
}}
OPTIONAL {{
    ?IRI opr:isAwaitingDetailedGPREvaluation ?awaiting_detailed_evaluation .
}}
BIND(IF(BOUND(?gpr), ?gpr, IF(?awaiting_detailed_evaluation = true, "Awaiting detailed evaluation", "")) AS ?{key})""".format(
                    pred=self._ATTRKEY2PRED[attr_key], key=attr_key.value
                )
            )
            vars.append("?" + attr_key.value)
        else:
            patterns.append(
                "?IRI {pred} [ om:hasNumericalValue ?{key} ; om:hasUnit ?{key}Unit ] .".format(
                    pred=self._ATTRKEY2PRED[attr_key], key=attr_key.value
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

    def count_plots(self, plot_args: PlotConstraints):
        iris = self.find_plot_iris(plot_args)
        return QAData(vars=["count"], bindings=[dict(count=len(iris))])

    def compute_aggregate_plot_attribute(
        self,
        plot_constraints: PlotConstraints,
        attr_agg: Tuple[PlotAttrKey, AggregateOperator],
    ):
        iris = self.find_plot_iris(plot_constraints)
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
                pred=self._ATTRKEY2PRED[key], valuenode=valuenode
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
