from functools import cache
from typing import List, Tuple

from model.aggregate import AggregateOperator
from ..model import PlotAttrKey, PlotCatAttrKey, PlotNumAttrKey
from ..constants import LAND_USE_TYPE_PREDICATE, PLOT_NUM_ATTR_PREDICATES


class SGLandLotsSPARQLMaker:
    def _make_clauses_to_locate_plots(self, land_use_type_iris: List[str] = []):
        select_vars = []
        ontop_patterns = ["?IRI rdf:type ontoplot:Plot ."]
        groupby_vars = []

        if land_use_type_iris:
            node = "?LandUseTypeIRI"
            select_vars.append(node)
            groupby_vars.append(node)

            ontop_patterns.extend(
                [
                    "VALUES {node} {{ {values} }}".format(
                        node=node,
                        values=" ".join(
                            ["<{iri}>".format(iri=iri) for iri in land_use_type_iris]
                        ),
                    ),
                    "?IRI {pred} ?LandUseTypeIRI .".format(
                        pred=LAND_USE_TYPE_PREDICATE
                    ),
                ]
            )
        return select_vars, ontop_patterns, groupby_vars

    def lookup_plot_attribute(
        self, attr_key: PlotAttrKey, land_use_type_iris: List[str] = []
    ):
        select_vars, ontop_patterns, groupby_vars = self._make_clauses_to_locate_plots(
            land_use_type_iris
        )
        select_vars.append("?IRI")

        if attr_key is PlotCatAttrKey.LAND_USE_TYPE:
            ontop_patterns.append("?IRI ontozoning:hasLandUseType ?LandUseType .")
            select_vars.append("?LandUseType")
        else:
            value_node = "?{key}Value".format(key=attr_key.value)
            unit_node = "?{key}Unit".format(key=attr_key.value)
            select_vars.extend([value_node, unit_node])

            if attr_key is PlotNumAttrKey.GROSS_PLOT_RATIO:
                optional_valuenode = "?OptionalGPR"
                awaiting_node = "?Awaiting"
                ontop_patterns.extend(
                    [
                        "OPTIONAL {{ ?IRI {pred} [ om:hasNumericalValue {optional} ; om:hasUnit {unit} ] . }}".format(
                            pred=PLOT_NUM_ATTR_PREDICATES[attr_key],
                            optional=optional_valuenode,
                            unit=unit_node,
                        ),
                        "OPTIONAL {{ ?IRI opr:isAwaitingDetailedGPREvaluation {awaiting} . }}".format(
                            awaiting=awaiting_node
                        ),
                        'BIND(IF(BOUND({optional}), {optional}, IF({awaiting} = true, "Awaiting detailed evaluation", "")) AS {value})'.format(
                            optional=optional_valuenode,
                            awaiting=awaiting_node,
                            value=value_node,
                        ),
                    ]
                )
            else:
                ontop_patterns.append(
                    "?IRI {pred} [ om:hasNumericalValue {value} ; om:hasUnit {unit} ] .".format(
                        pred=PLOT_NUM_ATTR_PREDICATES[attr_key],
                        value=value_node,
                        unit=unit_node,
                    )
                )

        return """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
PREFIX opr: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX ontozoning: <https://www.theworldavatar.com/kg/ontozoning/>
PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT {vars} WHERE {{
{patterns}
}}{groupby}""".format(
            vars=" ".join(select_vars),
            patterns="\n".join(ontop_patterns),
            groupby="\nGROUP BY " + " ".join(groupby_vars) if groupby_vars else "",
        )

    def count_plots(self, land_use_type_iris: List[str] = []):
        select_vars, ontop_patterns, groupby_vars = self._make_clauses_to_locate_plots(
            land_use_type_iris
        )
        select_vars.append("(COUNT(?IRI) as ?Count)")

        return """PREFIX ontoplot:<https://www.theworldavatar.com/kg/ontoplot/>
PREFIX opr: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX ontozoning:<https://www.theworldavatar.com/kg/ontozoning/>
PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT {vars} WHERE {{
{patterns}
}}{groupby}""".format(
            vars=" ".join(select_vars),
            patterns="\n".join(ontop_patterns),
            groupby="\nGROUP BY " + " ".join(groupby_vars) if groupby_vars else "",
        )

    def compute_aggregate_plot_attribute(
        self,
        attr_agg: Tuple[PlotNumAttrKey, AggregateOperator],
        land_use_type_iris: List[str] = [],
    ):
        select_vars, ontop_patterns, groupby_vars = self._make_clauses_to_locate_plots(
            land_use_type_iris
        )

        key, agg = attr_agg
        func = agg.value
        valuenode = "?{key}NumericalValue".format(key=key.value)

        select_vars.append(
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
}}{groupby}""".format(
            vars=" ".join(select_vars),
            patterns="\n".join(ontop_patterns),
            groupby="\nGROUP BY " + " ".join(groupby_vars) if groupby_vars else "",
        )


@cache
def get_sgLandLots_sparqlMaker():
    return SGLandLotsSPARQLMaker()
