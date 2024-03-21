from functools import cache
from typing import Annotated, List, Literal, Optional, Tuple

from fastapi import Depends

from model.aggregate import AggregateOperator
from model.constraint import ExtremeValueConstraint
from ..model import (
    FACTORYATTR2UNIT,
    FactoryAttrKey,
    FactoryConstraints,
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
        limit: Optional[int] = None,
    ):
        vars = ["?IRI"]
        ontop_patterns = self._make_factory_subclass_patterns()
        orderby_vars = []

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
}}{orderby}{limit}""".format(
            vars=" ".join(vars),
            patterns="\n".join(ontop_patterns),
            orderby="\nORDER BY " + " ".join(orderby_vars),
            limit="\nLIMIT " + str(limit) if limit else "",
        )

    def _init_clauses_for_agg(
        self, industry: Optional[Industry] = None, groupby_industry: bool = False
    ):
        select_vars = []
        ontop_patterns = self._make_factory_subclass_patterns()
        groupby_vars = []

        if industry:
            ontop_patterns.append(
                "VALUES ?Industry {{ ontocompany:{industry} }}".format(
                    industry=industry.value
                )
            )

        if industry or groupby_industry:
            select_vars.append("?Industry")
            ontop_patterns.append(
                "?IRI ontocompany:belongsToIndustry/rdf:type ?Industry ."
            )
            groupby_vars.append("?Industry")

        return select_vars, ontop_patterns, groupby_vars

    def count_factories(
        self, industry: Optional[Industry] = None, groupby_industry: bool = False
    ):
        select_vars, ontop_patterns, groupby_vars = self._init_clauses_for_agg(
            industry=industry, groupby_industry=groupby_industry
        )
        select_vars.append("(COUNT(?IRI) AS ?Count)")

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

    def compute_aggregate_factory_attribute(
        self,
        attr_agg: Tuple[FactoryAttrKey, AggregateOperator],
        industry: Optional[Industry] = None,
        groupby_industry: bool = False,
    ):
        select_vars, ontop_patterns, groupby_vars = self._init_clauses_for_agg(
            industry=industry, groupby_industry=groupby_industry
        )

        attr_key, agg_op = attr_agg
        unit = None
        if attr_key is FactoryAttrKey.INDUSTRY:
            pass
        else:
            agg_var = "?{key}NumericalValue".format(key=attr_key.value)
            unit = FACTORYATTR2UNIT[attr_key]
            if unit:
                pattern = '?IRI ontocompany:has{key}/om:hasValue [ om:hasNumericalValue ?{key}NumericalValue ; om:hasUnit/skos:notation "{unit}" ] .'.format(
                    key=attr_key.value, unit=unit
                )
            else:
                pattern = "?IRI ontocompany:has{key}/om:hasValue/om:hasNumericalValue ?{key}NumericalValue .".format(
                    key=attr_key.value
                )
            ontop_patterns.append(pattern)
        select_vars.append(
            "({func}({var}) AS {var}{func})".format(func=agg_op.value, var=agg_var)
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
def get_sg_factories_sparql_maker(
    factory_subclasses: Annotated[Tuple[str, ...], Depends(get_factory_subclasses)]
):
    return SGFactoriesSPARQLMaker(factory_subclasses)
