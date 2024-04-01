from functools import cache
from typing import Optional, Tuple

from model.aggregate import AggregateOperator
from services.core.kg import KgClient
from ..model import PlotAttrKey, PlotCatAttrKey, PlotNumAttrKey
from ..constants import LAND_USE_TYPE_PREDICATE, PLOT_NUM_ATTR_PREDICATES


class SGLandLotsSPARQLMaker:
    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client

    @cache
    def _landUse_clsname2iri(self, land_use_type: str):
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
        self, attr_key: PlotAttrKey, land_use_type: Optional[str] = None
    ):
        ontop_patterns = self._make_clauses_to_locate_plots(land_use_type)
        vars = ["?IRI"]
        if attr_key is PlotCatAttrKey.LAND_USE_TYPE:
            ontop_patterns.append("?IRI ontozoning:hasLandUseType ?LandUseType .")
            vars.append("?LandUseType")
        elif attr_key is PlotNumAttrKey.GROSS_PLOT_RATIO:
            varnode = "?" + attr_key.value
            ontop_patterns.extend(
                [
                    "OPTIONAL {{ ?IRI {pred} ?gpr . }}".format(
                        pred=PLOT_NUM_ATTR_PREDICATES[attr_key]
                    ),
                    "OPTIONAL { ?IRI opr:isAwaitingDetailedGPREvaluation ?awaiting_detailed_evaluation . }",
                    'BIND(IF(BOUND(?gpr), ?gpr, IF(?awaiting_detailed_evaluation = true, "Awaiting detailed evaluation", "")) AS {varnode})'.format(
                        varnode=varnode
                    ),
                ]
            )
            vars.append(varnode)
        else:
            ontop_patterns.append(
                "?IRI {pred} [ om:hasNumericalValue ?{key} ; om:hasUnit ?{key}Unit ] .".format(
                    pred=PLOT_NUM_ATTR_PREDICATES[attr_key], key=attr_key.value
                )
            )
            vars.append("?" + attr_key.value)

        return """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
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

    def count_plots(self, land_use_type: Optional[str]):
        ontop_patterns = self._make_clauses_to_locate_plots(land_use_type)
        vars = ["(COUNT(?IRI) as ?Count)"]

        return """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
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

        return """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
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
