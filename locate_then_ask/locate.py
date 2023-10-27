import copy
import random
from typing import Dict, Iterable, List, Optional
from collections import defaultdict

import networkx as nx

from constants.functions import (
    COMPARATIVE_LABELS,
    COMPARATIVES,
    AROUND,
    EQUAL,
    GREATER_THAN,
    GREATER_THAN_EQUAL,
    INSIDE,
    LESS_THAN,
    LESS_THAN_EQUAL,
)
from constants.ontospecies_keys import (
    KEY2LABELS,
    PROPERTY_KEYS,
    USE_KEY,
    CHEMCLASS_KEY,
)
from locate_then_ask.kg_client import KgClient
from locate_then_ask.utils import get_attribute_keys, get_gt, get_lt


class Locator:
    def __init__(
        self,
        kg_endpoint: str = "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql",
    ):
        self.kg_client = KgClient(kg_endpoint)
        self.entity2identifiers: Dict[
            str, Dict[str, List[str]]
        ] = dict()  # entity_iri -> IdentifierName -> names
        self.entity2propertykeys = dict()
        self.entity2uses: Dict[str, List[str]] = dict()
        self.entity2chemclasses: Dict[str, List[str]] = dict()

    def get_identifiers(self, entity_iri: str):
        if entity_iri not in self.entity2identifiers:
            query_template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    SELECT DISTINCT ?IdentifierNameValue ?hasIdentifierName WHERE {{
        VALUES ?hasIdentifierName {{ os:hasInChI os:hasIUPACName os:hasMolecularFormula os:hasSMILES }}
        <{SpeciesIri}> ?hasIdentifierName [ os:value ?IdentifierNameValue ] .
    }}"""
            query = query_template.format(SpeciesIri=entity_iri)

            response_bindings = self.kg_client.query(query)
            value_bindings = [
                {k: v["value"] for k, v in x.items()} for x in response_bindings
            ]

            accum = defaultdict(list)
            for binding in value_bindings:
                accum[binding["hasIdentifierName"]].append(
                    binding["IdentifierNameValue"]
                )
            self.entity2identifiers[entity_iri] = {
                k.split("#has", maxsplit=1)[-1]: v for k, v in accum.items()
            }
        return self.entity2identifiers[entity_iri]

    def locate_entity_name(self, entity_iris: Iterable[str]):
        entity_names = []
        for entity_iri in entity_iris:
            identifier_name = random.choice(
                list(self.get_identifiers(entity_iri).keys())
            )
            entity_name = random.choice(
                self.get_identifiers(entity_iri)[identifier_name]
            )
            entity_names.append("[{name}]".format(name=entity_name))

        query_graph = nx.DiGraph()
        query_graph.add_node(
            "Species", iri=entity_iris, label=entity_names, template_node=True
        )

        verbalization = " and ".join(entity_names)
        return query_graph, verbalization

    def locate_concept_name(self, entity_iri: str):
        query_graph = nx.DiGraph()
        query_graph.add_node(
            "Species", iri=entity_iri, rdf_type="os:Species", label="os:Species"
        )
        return query_graph, "chemical species"

    def get_operator_value_qualifier_property(self, entity_iri: str, key: str):
        predicate = "os:has{PropertyName}/os:value".format(PropertyName=key)

        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT * WHERE {{
    <{SpeciesIri}> os:has{PropertyName} ?PropertyName .
    ?PropertyName os:value ?PropertyNameValue ; os:unit/rdfs:label ?PropertyNameUnitLabel .
    OPTIONAL {{
        ?PropertyName os:hasReferenceState [ os:value ?ReferenceStateValue ; os:unit/rdfs:label ?ReferenceStateUnitLabel ] .
    }}
}}"""
        query = query_template.format(SpeciesIri=entity_iri, PropertyName=key)

        response_bindings = self.kg_client.query(query)
        if len(response_bindings) == 0:
            return None, None, None, None, None

        response_binding = random.choice(response_bindings)
        if (
            response_binding["PropertyNameValue"]["datatype"]
            != "http://www.w3.org/2001/XMLSchema#float"
        ):
            raise ValueError("Unexpected datatype: " + str(response_binding))
        property_value = float(response_binding["PropertyNameValue"]["value"])

        operator = random.choice(COMPARATIVES)
        if operator in [LESS_THAN, LESS_THAN_EQUAL]:
            value = get_gt(property_value)
        elif operator in [GREATER_THAN, GREATER_THAN_EQUAL]:
            value = get_lt(property_value)
        elif operator in [EQUAL, AROUND]:
            value = property_value
        elif operator == INSIDE:
            value = (get_lt(property_value), get_gt(property_value))
        else:
            raise ValueError("Unrecognised comparative: " + operator)

        qualifier_key = "reference state"
        qualifier_value = response_binding.get("ReferenceStateValue", dict()).get(
            "value"
        )

        return predicate, operator, value, qualifier_key, qualifier_value

    def get_property_keys(self, entity_iri: str):
        if entity_iri not in self.entity2propertykeys:
            query_template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT DISTINCT * WHERE {{
  VALUES (?p) {{{bindings}
  }}
  <{SpeciesIri}> ?p ?o .
}}"""
            hasproperty_values = "\n    ".join(
                ["(os:has{p})".format(p=p) for p in PROPERTY_KEYS]
            )
            query = query_template.format(
                bindings=hasproperty_values, SpeciesIri=entity_iri
            )
            response_bindings = self.kg_client.query(query)
            self.entity2propertykeys[entity_iri] = set(
                [
                    binding["p"]["value"].split("#has", maxsplit=1)[-1]
                    for binding in response_bindings
                ]
            )
        return self.entity2propertykeys[entity_iri]

    def get_uses(self, entity_iri: str):
        if entity_iri not in self.entity2uses:
            query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    SELECT DISTINCT ?UseLabel WHERE {{
        <{SpeciesIri}> os:hasUse [ rdfs:label ?UseLabel ] .
    }}"""
            query = query_template.format(SpeciesIri=entity_iri)
            response_bindings = self.kg_client.query(query)
            uses = [x["UseLabel"]["value"] for x in response_bindings]
            self.entity2uses[entity_iri] = uses
        return self.entity2uses[entity_iri]

    def get_chemclasses(self, entity_iri: str):
        if entity_iri not in self.entity2chemclasses:
            query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    SELECT ?ChemicalClassLabel WHERE {{
        <{SpeciesIri}> os:hasChemicalClass* ?x .
        ?x ?y ?z .
        ?z rdfs:subClassOf* [ rdf:type os:ChemicalClass ; 
                            rdfs:label ?ChemicalClassLabel ] .
    }}"""
            query = query_template.format(SpeciesIri=entity_iri)
            response_bindings = self.kg_client.query(query)
            chemclasses = [x["ChemicalClassLabel"]["value"] for x in response_bindings]
            self.entity2chemclasses[entity_iri] = chemclasses
        return self.entity2chemclasses[entity_iri]

    def locate_concept_and_literal(
        self, entity_iri: str, query_graph: Optional[nx.DiGraph] = None
    ):
        if query_graph is None:
            query_graph, _ = self.locate_concept_name(entity_iri)
        else:
            query_graph = copy.deepcopy(query_graph)

        sampled_keys = get_attribute_keys(query_graph)
        unsampled_keys = [
            x
            for x in self.get_property_keys(entity_iri)
            if x not in sampled_keys
        ]
        key_sampling_frame = unsampled_keys + (
            [USE_KEY] * (len(self.get_uses(entity_iri)) > 0)
            + [CHEMCLASS_KEY] * (len(self.get_chemclasses(entity_iri)) > 0)
        ) * (len(unsampled_keys) // 4)
        key = random.choice(key_sampling_frame)
        key_label = random.choice(KEY2LABELS[key])

        predicate, operator, value, qualifier_key, qualifier_value = None, None, None, None, None

        if key in PROPERTY_KEYS:
            (
                predicate,
                operator,
                value,
                qualifier_key,
                qualifier_value,
            ) = self.get_operator_value_qualifier_property(entity_iri, key)
        elif key == USE_KEY:
            predicate = "os:hasUse/rdfs:label"
            sampled_uses = [
                v
                for u, v, label in query_graph.edges(data="label")
                if label == predicate
            ]
            sampling_frame = [
                x for x in self.get_uses(entity_iri) if x not in sampled_uses
            ]
            if len(sampling_frame) > 0:
                value = random.choice(sampling_frame)
        elif key == CHEMCLASS_KEY:
            predicate = "os:hasChemicalClass/rdfs:label"
            sampled_chemclasses = [
                v
                for u, v, label in query_graph.edges(data="label")
                if label == predicate
            ]
            sampling_frame = [
                x
                for x in self.get_chemclasses(entity_iri)
                if x not in sampled_chemclasses
            ]
            if len(sampling_frame) > 0:
                value = random.choice(sampling_frame)

        if value is None:
            return None, None

        literal_num = len([n for n in query_graph.nodes() if n.startswith("literal")])
        literal_node = "literal_" + str(literal_num)

        query_graph.add_node(
            literal_node, label=value, literal=True, template_node=True
        )
        query_graph.add_edge("Species", literal_node, label=predicate)

        if operator is None:
            verbalization = "{K} is {V}".format(
                K=key_label, V="[{x}]".format(x=value)
            )
        else:
            operator_label = random.choice(COMPARATIVE_LABELS[operator])

            func_node = literal_node + "_func"
            query_graph.add_node(
                func_node, label=operator, func=True, template_node=True
            )
            query_graph.add_edge(literal_node, func_node, label="func")

            verbalization = "{K} is {OP} {V}".format(
                K=key_label, OP=operator_label, V=value
            )

        if (
            qualifier_key is not None
            and qualifier_value is not None
            and random.getrandbits(1)
        ):
            query_graph.nodes[literal_node]["qualifier_key"] = qualifier_key
            query_graph.nodes[literal_node]["qualifier_value"] = qualifier_value

            # qualifier_template = " ({QK} is {QV})"
            # verbalization += qualifier_template.format(
            #     QK=qualifier_key, QV=qualifier_value
            # )

        return query_graph, verbalization

    def locate_intersection(self, entity_iri: str, cond_num: int = 2):
        verbalized_conds = []
        query_graph = None

        for _ in range(cond_num):
            query_graph, verbalized_cond = self.locate_concept_and_literal(
                entity_iri, query_graph
            )
            if verbalized_cond is not None:
                verbalized_conds.append(verbalized_cond)

        verbalization = "the chemical species whose {conds}".format(
            conds=" and ".join(verbalized_conds)
        )

        return query_graph, verbalization
