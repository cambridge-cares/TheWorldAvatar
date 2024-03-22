from functools import cache
from typing import Annotated, Dict, List, Optional, Tuple

from fastapi import Depends

from model.aggregate import AggregateOperator
from model.constraint import ExtremeValueConstraint
from ..model import (
    FactoryAttrKey,
    FactoryIndustryKey,
    FactoryNumAttrKey,
    Industry,
)
from .labels_store import get_factory_subclasses


class SGFactoriesSPARQLMaker:
    def __init__(self, factory_subclasses: List[str]):
        self.factory_subclasses = factory_subclasses
        self.factory_subclass_patterns = [
            "VALUES ?Type {{ {types} }}".format(
                types=" ".join(
                    ["<{iri}>".format(iri=iri) for iri in self.factory_subclasses]
                )
            ),
            "?IRI rdf:type ?Type .",
        ]

    def _make_factory_subclass_patterns(self):
        return list(self.factory_subclass_patterns)

    def lookup_factory_attribute(self, iris: List[str], attr_key: FactoryAttrKey):
        select_vars = ["?IRI"]
        patterns = [
            "VALUES ?IRI {{ {iris} }}".format(
                iris=" ".join(["<{iri}>".format(iri=iri) for iri in iris])
            )
        ]

        if isinstance(attr_key, FactoryIndustryKey):
            select_vars.append("?Industry")
            patterns.append("?IRI ontocompany:belongsToIndustry/rdf:type ?Industry .")
        else:
            value_var = "?{key}Value".format(key=attr_key.value)
            unit_var = "?{key}Unit".format(key=attr_key.value)

            select_vars.extend([value_var, unit_var])
            patterns.extend(
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
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT {vars} WHERE {{
{patterns}
}}""".format(
            vars=" ".join(select_vars),
            patterns="\n".join(patterns),
        )

    def _make_clauses_for_constraint(
        self,
        key: FactoryNumAttrKey,
        constraint: ExtremeValueConstraint,
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

    def _make_industry_pattern(self, industry: Industry):
        return "?IRI ontocompany:belongsToIndustry/rdf:type ontocompany:{industry} .".format(
            industry=industry.value
        )

    def find_factories(
        self,
        industry: Industry,
        numattr_constraints: Dict[FactoryNumAttrKey, ExtremeValueConstraint] = dict(),
        limit: Optional[int] = None,
    ):
        select_vars = ["?IRI"]
        ontop_patterns = self._make_factory_subclass_patterns()
        ontop_patterns.append(self._make_industry_pattern(industry))
        orderby_vars = []

        for key, constraint in numattr_constraints.items():
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
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT {vars} WHERE {{
{patterns}
}}{orderby}{limit}""".format(
            vars=" ".join(select_vars),
            patterns="\n".join(ontop_patterns),
            orderby="\nORDER BY " + " ".join(orderby_vars),
            limit="\nLIMIT " + str(limit) if limit else "",
        )

    def count_factories(self, industry: Industry):
        select_vars = []
        ontop_patterns = self._make_factory_subclass_patterns()
        ontop_patterns.append(self._make_industry_pattern(industry))
        select_vars.append("(COUNT(?IRI) AS ?Count)")

        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT {select_vars} WHERE {{
{patterns}
}}""".format(
            select_vars=" ".join(select_vars), patterns="\n".join(ontop_patterns)
        )

    def compute_aggregate_factory_attribute(
        self,
        industry: Industry,
        attr_agg: Tuple[FactoryNumAttrKey, AggregateOperator],
    ):
        select_vars = []
        ontop_patterns = self._make_factory_subclass_patterns()
        ontop_patterns.append(self._make_industry_pattern(industry))
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
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant#>

SELECT {select_vars} WHERE {{
{patterns}
}}{groupby}""".format(
            select_vars=" ".join(select_vars),
            patterns="\n".join(ontop_patterns),
            groupby="\nGROUP BY " + " ".join(groupby_vars) if groupby_vars else "",
        )


@cache
def get_sgFactories_sparqlmaker(
    factory_subclasses: Annotated[Tuple[str, ...], Depends(get_factory_subclasses)]
):
    return SGFactoriesSPARQLMaker(factory_subclasses)
