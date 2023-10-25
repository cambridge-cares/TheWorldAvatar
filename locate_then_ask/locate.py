import random
import random
import math
from typing import Optional
from collections import defaultdict

from SPARQLWrapper import SPARQLWrapper, JSON
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
    IDENTIFIER_KEYS,
    SPECIES_ATTRIBUTE_KEYS,
    USE_KEY,
    CHEMCLASS_KEY,
)
from constants.predicates import RDF_TYPE


def get_lt(value: float):
    if value == 0:
        lt = -1.0
    elif value > 0:
        lt = value * 0.9
    else:
        lt = value * 1.1

    if random.getrandbits(1):
        lt_int = math.floor(lt)
        if lt_int < value:
            lt = lt_int

    return lt


def get_gt(value: float):
    if value == 0:
        gt = 1.0
    elif value > 0:
        gt = value * 1.1
    else:
        gt = value * 0.9

    if random.getrandbits(1):
        gt_int = math.ceil(gt)
        if gt_int > value:
            gt = gt_int

    return gt


class KgClient:
    def __init__(self, kg_endpoint: str):
        client = SPARQLWrapper(endpoint=kg_endpoint)
        client.setReturnFormat(JSON)
        self.client = client

    def query(self, query: str):
        self.client.setQuery(query)
        return self.client.queryAndConvert()["results"]["bindings"]


class Locator:
    SPECIES_ATTR_VALUES = "\n    ".join(
        ["(os:has{p})".format(p=p) for p in SPECIES_ATTRIBUTE_KEYS]
    )

    def __init__(
        self,
        kg_endpoint: str = "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql",
        seed_entities_filepath: Optional[str] = None,
    ):
        self.kg_client = KgClient(kg_endpoint)

        if seed_entities_filepath is None:
            seed_entities = self.sample_entities()
        else:
            with open(seed_entities_filepath, "r") as f:
                seed_entities = [x.strip() for x in f.readlines()]
                seed_entities = [x for x in seed_entities if x]
        self.seed_entities = seed_entities
        self.entity2attrs = dict()

    def sample_entities(self):
        query = """"PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT DISTINCT ?x (COUNT(DISTINCT ?p) as ?degree) WHERE {{
  ?x a os:Species .
  VALUES (?p) {{{bindings}
  }}
  ?x ?p ?o .
}}
GROUP BY ?x
ORDER BY DESC(?degree)
LIMIT 100""".format(
            bindings=self.SPECIES_ATTR_VALUES
        )
        bindings = self.kg_client.query(query)
        return [x["x"]["value"] for x in bindings]

    def locate_entity_name(self, entity_iri: str):
        query_template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT DISTINCT ?IdentifierNameValue ?hasIdentifierName WHERE {{
    VALUES ( ?hasIdentifierName ) {{ ( os:hasInChI ) ( os:hasIUPACName ) ( os:hasMolecularFormula ) ( os:hasSMILES ) }}
    <{SpeciesIri}> ?hasIdentifierName [ os:value ?IdentifierNameValue ] .
}}"""
        query = query_template.format(SpeciesIri=entity_iri)

        response_bindings = self.kg_client.query(query)
        value_bindings = [
            {k: v["value"] for k, v in x.items()} for x in response_bindings
        ]

        hasIdentifierName2IdentifierNameValues = defaultdict(list)
        for binding in value_bindings:
            hasIdentifierName2IdentifierNameValues[binding["hasIdentifierName"]].append(
                binding["IdentifierNameValue"]
            )

        values = random.choice(
            [v for _, v in hasIdentifierName2IdentifierNameValues.items()]
        )
        entity_name = random.choice(values)

        query_graph = nx.DiGraph()
        query_graph.add_node(
            "Species", iri=entity_iri, label=entity_name, template_node=True
        )
        return query_graph, entity_name

    def locate_concept_name(self, entity_iri: str):
        query_graph = nx.DiGraph()
        query_graph.add_node(
            "Species", iri=entity_iri, rdf_type="os:Species", label="os:Species"
        )
        return query_graph, "chemical species"

    def get_operator_value_qualifier_property(self, entity_iri: str, key: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?PropertyNameValue ?PropertyNameUnitLabel ?ReferenceStateValue ?ReferenceStateUnitLabel WHERE {{
    <{SpeciesIri}> os:has{PropertyName} ?PropertyName .
    ?PropertyName os:value ?PropertyNameValue ; os:unit ?PropertyNameUnit . 
    ?PropertyNameUnit rdfs:label ?PropertyNameUnitLabel .
    OPTIONAL {{
        ?PropertyName os:hasReferenceState ?ReferenceState .
        ?ReferenceState os:value ?ReferenceStateValue ; os:unit ?ReferenceStateUnit .
        ?ReferenceStateUnit rdfs:label ?ReferenceStateUnitLabel .
    }}
}}"""
        query = query_template.format(SpeciesIri=entity_iri, PropertyName=key)

        response_bindings = self.kg_client.query(query)
        if len(response_bindings) == 0:
            return None, None, None, None

        response_binding = random.choice(response_bindings)
        if (
            response_binding["PropertyNameValue"]["datatype"]
            != "http://www.w3.org/2001/XMLSchema#float"
        ):
            raise ValueError("Unexpected datatype: " + str(response_binding))
        property_value = float(response_binding["PropertyNameValue"]["value"])

        operator = random.choice(COMPARATIVES)
        if operator in [LESS_THAN, LESS_THAN_EQUAL]:
            value = str(get_lt(property_value))
        elif operator in [GREATER_THAN, GREATER_THAN_EQUAL]:
            value = str(get_gt(property_value))
        elif operator in [EQUAL, AROUND]:
            value = str(property_value)
        elif operator == INSIDE:
            value = "between {low} and {high}".format(
                low=get_lt(property_value), high=get_gt(property_value)
            )
        else:
            raise ValueError("Unrecognised comparative: " + operator)

        qualifier_key = "reference state"
        qualifier_value = response_binding.get("ReferenceStateValue", dict()).get(
            "value"
        )

        return operator, value, qualifier_key, qualifier_value

    def get_value_identifier(self, entity_iri: str, key: str):
        query_template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT DISTINCT ?IdentifierNameValue WHERE {{
    <{SpeciesIri}> os:has{IdentifierName} [ os:value ?IdentifierNameValue ] .
}}"""
        query = query_template.format(SpeciesIri=entity_iri, IdentifierName=key)
        response_bindings = self.kg_client.query(query)
        if len(response_bindings) == 0:
            return None
        return random.choice(response_bindings)["IdentifierNameValue"]["value"]

    def get_value_use(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT ?UseLabel WHERE {{
    <{SpeciesIri}> os:hasUse [ rdfs:label ?UseLabel ] .
}}"""
        query = query_template.format(SpeciesIri=entity_iri)
        response_bindings = self.kg_client.query(query)
        if len(response_bindings) == 0:
            return None
        return random.choice(response_bindings)["UseLabel"]["value"]

    def get_value_chemclass(self, entity_iri: str):
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
        if len(response_bindings) == 0:
            return None
        return random.choice(response_bindings)["ChemicalClassLabel"]["value"]

    def get_operator_value_qualifier(self, entity_iri: str, key: str):
        operator, value, qualifier_key, qualifier_value = None, None, None, None

        if key in PROPERTY_KEYS:
            (
                operator,
                value,
                qualifier_key,
                qualifier_value,
            ) = self.get_operator_value_qualifier_property(entity_iri, key)
        elif key in IDENTIFIER_KEYS:
            value = self.get_value_identifier(entity_iri, key)
        elif key == USE_KEY:
            value = self.get_value_use(entity_iri)
        elif key == CHEMCLASS_KEY:
            value = self.get_value_chemclass(entity_iri)

        return operator, value, qualifier_key, qualifier_value

    def get_attrs(self, entity_iri: str):
        if entity_iri not in self.entity2attrs:
            query_template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT DISTINCT * WHERE {{
  VALUES (?p) {{{bindings}
  }}
  <{SpeciesIri}> ?p ?o .
}}"""
            query = query_template.format(
                bindings=self.SPECIES_ATTR_VALUES, SpeciesIri=entity_iri
            )
            response_bindings = self.kg_client.query(query)
            self.entity2attrs[entity_iri] = set(
                [
                    binding["p"]["value"].split("#has", maxsplit=1)[-1]
                    for binding in response_bindings
                ]
            )
        return self.entity2attrs[entity_iri]

    def locate_concept_and_literal(
        self, entity_iri: str, query_graph: Optional[nx.DiGraph] = None
    ):
        if query_graph is None:
            query_graph, _ = self.locate_concept_name(entity_iri)
        else:
            query_graph = query_graph.copy()

        key_sampling_frame = [
            x for x in self.get_attrs(entity_iri) if x not in query_graph.nodes()
        ]
        key = random.choice(key_sampling_frame)
        key_label = random.choice(KEY2LABELS[key])

        (
            operator,
            value,
            qualifier_key,
            qualifier_value,
        ) = self.get_operator_value_qualifier(entity_iri, key)
        if value is None:
            return None, None

        query_graph.add_node(key, label=value, literal=True, template_node=True)
        query_graph.add_edge("Species", key, label="os:has" + key)

        if operator is None:
            verbalization = "{K} is {V}".format(K=key_label, V=value)
        else:
            operator_label = random.choice(COMPARATIVE_LABELS[operator])

            func_node = key + "_func"
            query_graph.add_node(
                func_node, label=operator, func=True, template_node=True
            )
            query_graph.add_edge(key, func_node)

            verbalization = "{K} is {OP} {V}".format(
                K=key_label, OP=operator_label, V=value
            )

        if (
            qualifier_key is not None
            and qualifier_value is not None
            and random.getrandbits(1)
        ):
            query_graph.nodes[key]["qualifier_key"] = qualifier_key
            query_graph.nodes[key]["qualifier_value"] = qualifier_value

            # qualifier_template = " ({QK} is {QV})"
            # verbalization += qualifier_template.format(
            #     QK=qualifier_key, QV=qualifier_value
            # )

        return query_graph, verbalization

    def locate_intersection(self, entity_iri: str):
        verbalized_conds = []
        query_graph, verbalized_cond = self.locate_concept_and_literal(entity_iri)
        verbalized_conds.append(verbalized_cond)

        query_graph, verbalized_cond = self.locate_concept_and_literal(
            entity_iri, query_graph
        )
        verbalized_conds.append(verbalized_cond)

        verbalization = "the chemical species whose {conds}".format(
            conds=" and ".join(verbalized_conds)
        )

        return query_graph, verbalization
