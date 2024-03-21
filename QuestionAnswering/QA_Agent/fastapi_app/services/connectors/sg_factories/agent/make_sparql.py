from functools import cache
from typing import List, Literal, Optional

from model.constraint import ExtremeValueConstraint
from services.connectors.sg_factories.model import (
    FACTORYATTR2UNIT,
    FactoryAttrKey,
    FactoryConstraints,
    Industry,
)


class SGFactoriesSPARQLMaker:
    def __init__(self, factory_subclasses: List[str]):
        self.factory_subclasses = factory_subclasses

    @cache
    def _make_patterns_for_entity_type(self):
        return [
            "VALUES ?Type {{ {types} }}".format(
                types=" ".join(
                    ["<{iri}>".format(iri=iri) for iri in self.factory_subclasses]
                )
            ),
            "?IRI rdf:type ?Type .",
        ]

    def lookup_factory_attribute(self, iris: List[str], attr_key: FactoryAttrKey):
        vars = ["?IRI"]

        if attr_key is FactoryAttrKey.INDUSTRY:
            vars.append("?Industry")
            pattern = "?IRI ontocompany:belongsToIndustry/rdf:type ?Industry ."
        elif attr_key is FactoryAttrKey.THERMAL_EFFICIENCY:
            vars.append("?{key}NumericalValue".format(key=attr_key.value))
            pattern = "?IRI ontocompany:has{key}/om:hasValue/om:hasNumericalValue ?{key}NumericalValue .".format(
                key=attr_key.value
            )
        else:
            vars.extend(
                [
                    "?{key}NumericalValue".format(key=attr_key.value),
                    "?{key}Unit".format(key=attr_key.value),
                ]
            )
            pattern = "?IRI ontocompany:has{key}/om:hasValue [ om:hasNumericalValue ?{key}NumericalValue ; om:hasUnit/skos:notation ?{key}Unit ] .".format(
                key=attr_key.value
            )

        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT {vars} WHERE {{
VALUES ?IRI {{ {iris} }}
{pattern}
}}""".format(
            iris=" ".join(["<{iri}>".format(iri=iri) for iri in iris]),
            vars=" ".join(vars),
            pattern=pattern,
        )

    def _make_clauses_for_constraint(
        self,
        key: Literal[
            FactoryAttrKey.DESIGN_CAPACITY,
            FactoryAttrKey.GENERATED_HEAT,
            FactoryAttrKey.SPECIFIC_ENERGY_CONSUMPTION,
            FactoryAttrKey.THERMAL_EFFICIENCY,
        ],
        constraint: ExtremeValueConstraint,
    ):
        unit = FACTORYATTR2UNIT[key]
        value_var = "?{key}NumericalValue".format(key=key.value)

        if unit:
            where_pattern = '?IRI ontocompany:has{key}/om:hasValue [ om:hasNumericalValue {value_node} ; om:hasUnit/skos:notation "{unit}" ] .'.format(
                key=key.value, value_node=value_var, unit=unit
            )
        else:
            where_pattern = "?IRI ontocompany:has{key}/om:hasValue/om:hasNumericalValue {value_node} .".format(
                key=key.value, value_node=value_var
            )

        if constraint is ExtremeValueConstraint.MAX:
            orderby = "DESC({var})".format(var=value_var)
        else:
            orderby = value_var

        return value_var, where_pattern, orderby

    def find_factories(
        self,
        constraints: Optional[FactoryConstraints] = None,
    ):
        vars = ["?IRI"]
        ontop_patterns = []
        orderby_vars = []

        # Match subclasses of ontocompany:Factory
        ontop_patterns.extend(self._make_patterns_for_entity_type())

        if constraints:
            if constraints.industry:
                ontop_patterns.append(
                    "?IRI ontocompany:belongsToIndustry/rdf:type ontocompany:{industry} .".format(
                        industry=constraints.industry.value
                    )
                )
            for field, key in (
                ("generated_heat", FactoryAttrKey.GENERATED_HEAT),
                ("design_capacity", FactoryAttrKey.DESIGN_CAPACITY),
                (
                    "specific_energy_consumption",
                    FactoryAttrKey.SPECIFIC_ENERGY_CONSUMPTION,
                ),
                ("thermal_efficiency", FactoryAttrKey.THERMAL_EFFICIENCY),
            ):
                constraint = getattr(constraints, field)
                if constraint:
                    value_var, where_pattern, orderby = (
                        self._make_clauses_for_constraint(
                            key=key, constraint=constraint
                        )
                    )
                    vars.append(value_var)
                    ontop_patterns.append(where_pattern)
                    orderby_vars.append(orderby)

        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT {vars} WHERE {{
{patterns}
}}{orderby}""".format(
            vars=" ".join(vars),
            patterns="\n".join(ontop_patterns),
            orderby="\nORDER BY " + " ".join(orderby_vars),
        )

    def count_factories(
        self, industry: Optional[Industry] = None, groupby_industry: bool = False
    ):
        select_vars = ["(COUNT(?IRI) AS ?Count)"]
        ontop_patterns = []
        groupby_vars = []

        # Match subclasses of ontocompany:Factory
        ontop_patterns.extend(self._make_patterns_for_entity_type())

        if industry:
            ontop_patterns.append(
                "?IRI ontocompany:belongsToIndustry/rdf:type ontocompany:{industry} .".format(
                    industry=industry.value
                )
            )

        if groupby_industry:
            select_vars.append("?Industry")
            ontop_patterns.append(
                "?IRI ontocompany:belongsToIndustry/rdf:type ?Industry ."
            )
            groupby_vars.append("?Industry")

        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT {select_vars} WHERE {{
{patterns}
}}{groupby}""".format(
            select_vars=" ".join(select_vars),
            patterns="\n".join(ontop_patterns),
            groupby="\nGROUP BY " + " ".join(groupby_vars) if groupby_vars else "",
        )
