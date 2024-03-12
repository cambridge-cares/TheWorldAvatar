import logging
from typing import List, Optional

from pydantic.dataclasses import dataclass

from services.kg_client import KgClient
from model.constraint import CompoundNumericalConstraint
from model.qa import QAData
from .constants import LAND_USE_CLS2INSTANCE, LandUseType, PlotAttrKey

logger = logging.getLogger(__name__)


@dataclass
class PlotArgs:
    land_use_type: Optional[LandUseType] = None
    gross_plot_ratio: Optional[CompoundNumericalConstraint] = None
    plot_area: Optional[CompoundNumericalConstraint] = None
    gross_floor_area: Optional[CompoundNumericalConstraint] = None


class SingporeLandLotAgent:
    _ATTRKEY2PRED = {
        PlotAttrKey.LAND_USE_TYPE: "ontozoning:hasLandUseType",
        PlotAttrKey.GROSS_PLOT_RATIO: "^opr:appliesTo/opr:allowsGrossPlotRatio/om:hasValue",
        PlotAttrKey.PLOT_AREA: "ontoplot:hasPlotArea/om:hasValue",
        PlotAttrKey.GROSS_FLOOR_AREA: "ontoplot:hasMaximumPermittedGPR/om:hasValue",
    }

    def __init__(self, ontop_client: KgClient, bg_endpoint: str):
        self.ontop_client = ontop_client
        self.bg_endpoint = bg_endpoint

    def _make_patterns_for_constraint(
        self, key: PlotAttrKey, constraint: CompoundNumericalConstraint
    ):
        triple = "?IRI {pred}/om:hasNumericalValue ?{key} .".format(
            pred=self._ATTRKEY2PRED[key], key=key.value
        )

        atomic_constraints = [
            "?{key} {operator} {operand}".format(
                key=key.value, operator=x.operator.value, operand=x.operand
            )
            for x in constraint.constraints
        ]
        if constraint.logical_operator:
            delimiter = constraint.logical_operator.value
        else:
            delimiter = "&&"
        exprn = delimiter.join(atomic_constraints)
        filter_pattern = "FILTER ( {exprn} )".format(exprn=exprn)

        return [triple, filter_pattern]

    def find_plot_iris(self, plot_args: PlotArgs):
        patterns = []

        if plot_args.land_use_type:
            patterns.append(
                "?IRI {pred} <{land_use}> .".format(
                    pred=self._ATTRKEY2PRED[PlotAttrKey.LAND_USE_TYPE],
                    land_use=LAND_USE_CLS2INSTANCE[plot_args.land_use_type],
                )
            )
        for fieldname, key in [
            ("gross_plot_ratio", PlotAttrKey.GROSS_PLOT_RATIO),
            ("plot_area", PlotAttrKey.PLOT_AREA),
            ("gross_floor_area", PlotAttrKey.GROSS_FLOOR_AREA),
        ]:
            field = getattr(plot_args, fieldname)
            if isinstance(field, CompoundNumericalConstraint):
                _patterns = self._make_patterns_for_constraint(key, field)
                patterns.extend(_patterns)

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

        logger.info("SPARQL query:\n" + query)

        return [
            x["IRI"]["value"]
            for x in self.ontop_client.query(query)["results"]["bindings"]
        ]

    def lookup_plot_attributes(self, plot_args: PlotArgs, attr_keys: List[PlotAttrKey]):
        iris = self.find_plot_iris(plot_args)
        if not iris:
            return QAData()
        
        patterns = [
            "VALUES ?IRI {{ {values} }}".format(
                values=" ".join(["<{iri}>".format(iri=iri) for iri in iris])
            )
        ]
        vars = ["?IRI"]
        for key in attr_keys:
            if key is PlotAttrKey.LAND_USE_TYPE:
                patterns.append("?IRI ontozoning:hasLandUseType ?LandUseType .")
                patterns.append(
                    "SERVICE <{bg}> {{ ?LandUseType rdfs:label ?LandUseTypeLabel }} ".format(
                        bg=self.bg_endpoint
                    )
                )
                vars.append("?LandUseTypeLabel")
            elif key is PlotAttrKey.GROSS_PLOT_RATIO:
                patterns.append(
                    """
OPTIONAL {{
    ?IRI {pred} ?gpr . 
}}
OPTIONAL {{
    ?IRI opr:isAwaitingDetailedGPREvaluation ?awaiting_detailed_evaluation .
}}
BIND(IF(BOUND(?gpr), ?gpr, IF(?awaiting_detailed_evaluation = true, "Awaiting detailed evaluation", "")) AS ?{key})""".format(
                        pred=self._ATTRKEY2PRED[key], key=key.value
                    )
                )
                vars.append("?" + key.value)
            elif key is PlotAttrKey.PLOT_AREA or key is PlotAttrKey.GROSS_FLOOR_AREA:
                patterns.append(
                    "?IRI {pred} [ om:hasNumericalValue ?{key} ; om:hasUnit ?{key}Unit ] .".format(
                        pred=self._ATTRKEY2PRED[key], key=key.value
                    )
                )
                vars.append("?" + key.value)
            else:
                pass

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
        print(query)

        res = self.ontop_client.query(query)
        vars = res["head"]["vars"]
        bindings = [
            {k: v["value"] for k, v in binding.items()}
            for binding in res["results"]["bindings"]
        ]
        return QAData(vars=vars, bindings=bindings)

    def compute_plot_statistics(self, plot_args: PlotArgs):
        # numercal arguments: either a range, or argmin/argmax
        pass
