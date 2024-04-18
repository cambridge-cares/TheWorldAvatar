from functools import cache
from typing import Dict, List, Optional, Tuple

from model.aggregate import AggregateOperator
from model.constraint import ExtremeValueConstraint
from services.connectors.sg_data_centres.model import DataCentreAttrKey


class SGDataCentresSPARQLMaker:
    def lookup_dataCentre_attribute(self, iris: List[str], attr_key: DataCentreAttrKey):
        select_vars = ["?IRI"]
        ontop_patterns = [
            # Use FILTER instead of VALUES because a VALUES clause with 2 or more UUID
            # values results in an error at the ONTOP endpoint
            # "VALUES ?IRI {{ {values} }}".format(
            #     values=" ".join(["<{iri}>".format(iri=iri) for iri in iris])
            # )
            "FILTER ( ?IRI IN ({values}) )".format(
                values=", ".join(["<{iri}>".format(iri=iri) for iri in iris])
            )
        ]

        value_var = "?{key}Value".format(key=attr_key.value)
        unit_var = "?{key}Unit".format(key=attr_key.value)

        select_vars.extend([value_var, unit_var])
        ontop_patterns.extend(
            [
                "?IRI ontocompany:has{key}/om:hasValue ?{key} .".format(
                    key=attr_key.value
                ),
                "?{key} om:hasNumericalValue {value_node} .".format(
                    key=attr_key.value, value_node=value_var
                ),
                "OPTIONAL {{ ?{key} om:hasUnit/skos:notation {unit_node} . }}".format(
                    key=attr_key.value, unit_node=unit_var
                ),
            ]
        )

        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>

SELECT DISTINCT {vars} WHERE {{
{patterns}
}}""".format(
            vars=" ".join(select_vars),
            patterns="\n".join(ontop_patterns),
        )

    def _make_dataCentre_class_pattern(self):
        return "?IRI rdf:type ontocompany:DataCentre ."

    def _make_clauses_for_constraint(
        self, key: DataCentreAttrKey, constraint: ExtremeValueConstraint
    ):
        value_var = "?{key}Value".format(key=key.value)
        unit_var = "?{key}Unit".format(key=key.value)

        where_patterns = [
            "?IRI ontocompany:has{key}/om:hasValue ?{key} .".format(key=key.value),
            "?{key} om:hasNumericalValue {value_node} .".format(
                key=key.value, value_node=value_var
            ),
            "OPTIONAL {{ ?{key} om:hasUnit/skos:notation {unit_node} . }}".format(
                key=key.value, unit_node=unit_var
            ),
        ]

        if constraint is ExtremeValueConstraint.MAX:
            value_var_orderby = "DESC({var})".format(var=value_var)
        else:
            value_var_orderby = value_var

        return value_var, value_var_orderby, unit_var, where_patterns

    def find_dataCentres(
        self,
        constraints: Dict[DataCentreAttrKey, ExtremeValueConstraint] = dict(),
        limit: Optional[int] = None,
    ):
        select_vars = ["?IRI"]
        ontop_patterns = [self._make_dataCentre_class_pattern()]
        orderby_vars = []

        for key, constraint in constraints.items():
            value_var, value_var_orderby, unit_var, where_patterns = (
                self._make_clauses_for_constraint(key=key, constraint=constraint)
            )
            select_vars.extend([value_var, unit_var])
            ontop_patterns.extend(where_patterns)
            orderby_vars.append(value_var_orderby)

        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>

SELECT DISTINCT {vars} WHERE {{
{patterns}
}}{orderby}{limit}""".format(
            vars=" ".join(select_vars),
            patterns="\n".join(ontop_patterns),
            orderby="\nORDER BY " + " ".join(orderby_vars) if orderby_vars else "",
            limit="\nLIMIT " + str(limit) if limit else "",
        )

    def count_dataCentres(self):
        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>

SELECT (COUNT(?IRI) AS ?Count) WHERE {{
{patterns}
}}""".format(
            patterns=self._make_dataCentre_class_pattern()
        )

    def compute_aggregate_dataCentre_attribute(
        self, attr_agg: Tuple[DataCentreAttrKey, AggregateOperator]
    ):
        select_vars = []
        ontop_patterns = [self._make_dataCentre_class_pattern()]
        groupby_vars = []

        attr_key, agg_op = attr_agg

        unit_var = "?{key}Unit".format(key=attr_key.value)
        agg_var = "?{key}Value".format(key=attr_key.value)
        ontop_patterns.extend(
            [
                "?IRI ontocompany:has{key}/om:hasValue ?{key} .".format(
                    key=attr_key.value
                ),
                "?{key} om:hasNumericalValue ?{key}Value .".format(key=attr_key.value),
                "OPTIONAL {{ ?{key} om:hasUnit/skos:notation {unit_node} . }}".format(
                    key=attr_key.value, unit_node=unit_var
                ),
            ]
        )
        groupby_vars.append(unit_var)

        select_vars.extend(
            [
                "({func}({var}) AS {var}{func})".format(func=agg_op.value, var=agg_var),
                unit_var,
            ]
        )

        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>

SELECT {select_vars} WHERE {{
{patterns}
}}{groupby}""".format(
            select_vars=" ".join(select_vars),
            patterns="\n".join(ontop_patterns),
            groupby="\nGROUP BY " + " ".join(groupby_vars) if groupby_vars else "",
        )


@cache
def get_sgDataCentres_sparqlMaker():
    return SGDataCentresSPARQLMaker()
