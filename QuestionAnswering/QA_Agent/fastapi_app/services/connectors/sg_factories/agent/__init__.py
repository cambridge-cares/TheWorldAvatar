from functools import cache
from typing import Annotated, Optional, Tuple

from fastapi import Depends

from model.aggregate import AggregateOperator
from model.qa import QAData
from services.kg_client import KgClient
from services.labels_store import LabelsStore
from ..constants import FACTORYATTR2UNIT, FactoryAttrKey, FactoryConcept
from ..kg_client import get_sg_factories_ontop_client
from .labels_store import get_sg_factories_labels_store


class SGFactoriesAgent:
    def __init__(
        self,
        ontop_client: KgClient,
        labels_store: LabelsStore,
    ):
        self.ontop_client = ontop_client
        self.labels_store = labels_store

    def lookup_factory_attribute(self, plant_name: str, attr_key: FactoryAttrKey):
        vars = ["?IRI"]
        iris = self.labels_store.link_entity(plant_name)

        if attr_key is FactoryAttrKey.INDUSTRY:
            vars.append("?Industry")
            pattern = "?IRI ontocompany:belongsToIndustry/rdfs:label ?Industry ."
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

        query = """PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant/>

SELECT {vars} WHERE {{
    VALUES ?IRI {{ {iris} }}
    {pattern}
}}
""".format(
            iris=" ".join(["<{iri}>".format(iri=iri) for iri in iris]),
            vars=" ".join(vars),
            pattern=pattern,
        )

        res = self.ontop_client.query(query)
        return QAData(
            vars=res["head"]["vars"],
            bindings=[
                {k: v["value"] for k, v in binding.items()}
                for binding in res["results"]["bindings"]
            ],
        )

    def count_factories(
        self, factory_type: Optional[FactoryConcept] = None, group_bytype: bool = False
    ):
        vars = ["(COUNT(?IRI) AS ?Count)"]
        ontop_patterns = []
        groupby_vars = []

        if group_bytype:
            vars.append("?Type")
            ontop_patterns.append("?IRI rdf:type ?Type .")
            groupby_vars.append("?Type")

        if factory_type is None:
            ontop_patterns.extend(
                [
                    "VALUES ?Type {{ {types} }}".format(
                        types=" ".join(
                            [
                                "<{iri}>".format(iri=concept.value)
                                for concept in FactoryConcept
                            ]
                        )
                    ),
                    "?IRI rdf:type ?Type .",
                ]
            )
        else:
            if factory_type is FactoryConcept.CHEMICAL_PLANT:
                concept = "ontochemplant:ChemicalPlant"
            else:
                concept = "ontocompany:" + factory_type.value
            ontop_patterns.append("?IRI rdf:type {type} .".format(type=concept))

        query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant/>

SELECT DISTINCT {vars} WHERE {{
{patterns}
}}{groupby}""".format(
            vars=" ".join(vars),
            patterns="\n".join(ontop_patterns),
            groupby="\nGROUP BY " + " ".join(groupby_vars) if groupby_vars else "",
        )
        res = self.ontop_client.query(query)

        return QAData(
            vars=res["head"]["vars"],
            bindings=[
                {k: v["value"] for k, v in binding.items()}
                for binding in res["results"]["bindings"]
            ],
        )

    def compute_aggregate_factory_attribute(
        self,
        factory_type: Optional[FactoryConcept],
        attr_agg: Tuple[FactoryAttrKey, AggregateOperator],
        group_bytype: bool = False,
    ):
        vars = []
        ontop_patterns = []
        groupby_vars = []

        if group_bytype:
            vars.append("?Type")
            ontop_patterns.append("?IRI rdf:type ?Type .")
            groupby_vars.append("?Type")

        if factory_type is None:
            ontop_patterns.extend(
                [
                    "VALUES ?Type {{ {types} }}".format(
                        types=" ".join(
                            [
                                "<{iri}>".format(iri=concept.value)
                                for concept in FactoryConcept
                            ]
                        )
                    ),
                    "?IRI rdf:type ?Type .",
                ]
            )
        else:
            ontop_patterns.append(
                "?IRI rdf:type <{type}> .".format(type=factory_type.value)
            )

        attr_key, agg_op = attr_agg
        if attr_key is FactoryAttrKey.INDUSTRY:
            agg_var = "?Industry"
            ontop_patterns.append(
                "?IRI ontocompany:belongsToIndustry/rdfs:label ?Industry ."
            )
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
        vars.append(
            "({func}({var}) AS {var}{func})".format(func=agg_op.value, var=agg_var)
        )

        query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant/>

SELECT {vars} WHERE {{
{patterns}
}}{groupby}""".format(
            vars=" ".join(vars),
            patterns="\n".join(ontop_patterns),
            groupby="\nGROUP BY " + " ".join(groupby_vars) if groupby_vars else "",
        )
        res = self.ontop_client.query(query)
        return QAData(
            vars=res["head"]["vars"],
            bindings=[
                {k: v["value"] for k, v in binding.items()}
                for binding in res["results"]["bindings"]
            ],
        )


def get_sg_factories_agent(
    ontop_client: Annotated[KgClient, Depends(get_sg_factories_ontop_client)],
    labels_store: Annotated[LabelsStore, Depends(get_sg_factories_labels_store)],
):
    return SGFactoriesAgent(
        ontop_client=ontop_client,
        labels_store=labels_store,
    )
