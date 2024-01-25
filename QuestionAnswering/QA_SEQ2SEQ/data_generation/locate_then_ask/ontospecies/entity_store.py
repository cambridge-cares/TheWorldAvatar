from collections import defaultdict
from typing import Dict, List
from constants.ontospecies import IDENTIFIER_KEYS, PROPERTY_KEYS
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontospecies.model import OSProperty, OSSpecies


class OSEntityStore:
    def __init__(self, kg_endpoint: str):
        self.kg_client = KgClient(kg_endpoint)
        self.iri2entity: Dict[str, OSSpecies] = dict()

    def get(self, entity_iri: str):
        if entity_iri not in self.iri2entity:
            self.iri2entity[entity_iri] = self.create(entity_iri)
        return self.iri2entity[entity_iri]

    def create(self, entity_iri: str):
        identifiers = self.retrieve_identifiers(entity_iri)
        properties = self.retrieve_properties(entity_iri)
        chemclasses = self.retrieve_chemclasses(entity_iri)
        uses = self.retrieve_uses(entity_iri)

        return OSSpecies(
            iri=entity_iri,
            key2identifier=identifiers,
            key2property=properties,
            chemclasses=chemclasses,
            uses=uses,
        )

    def retrieve_identifiers(self, entity_iri: str):
        query_template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT DISTINCT ?IdentifierNameValue ?hasIdentifierName WHERE {{
    VALUES ?hasIdentifierName {{ {hasIdentifierNameValues} }}
    <{SpeciesIRI}> ?hasIdentifierName [ os:value ?IdentifierNameValue ] .
}}"""
        query = query_template.format(
            SpeciesIRI=entity_iri,
            hasIdentifierNameValues=" ".join(["os:has" + x for x in IDENTIFIER_KEYS]),
        )

        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        value_bindings = [
            {k: v["value"] for k, v in x.items()} for x in response_bindings
        ]
        accum: Dict[List[Dict[str, str]]] = defaultdict(list)
        for binding in value_bindings:
            key = binding["hasIdentifierName"].rsplit("#has", maxsplit=1)[-1]
            accum[key].append(binding["IdentifierNameValue"])
        return dict(accum)

    def retrieve_properties(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT * WHERE {{
    VALUES ?hasPropertyName {{ {hasPropertyNameValues} }}
    <{SpeciesIri}> ?hasPropertyName ?PropertyName .
    ?PropertyName os:value ?PropertyNameValue ; 
                  os:unit/rdfs:label ?PropertyNameUnitLabel .
    OPTIONAL {{
        ?PropertyName os:hasReferenceState [ 
            os:value ?ReferenceStateValue ; 
            os:unit/rdfs:label ?ReferenceStateUnitLabel 
        ] .
    }}
}}"""
        query = query_template.format(
            SpeciesIri=entity_iri,
            hasPropertyNameValues=" ".join(["os:has" + key for key in PROPERTY_KEYS]),
        )
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        value_bindings = [
            {k: v["value"] for k, v in binding.items()} for binding in response_bindings
        ]
        accum: Dict[str, List[OSProperty]] = defaultdict(list)
        for binding in value_bindings:
            key = binding["hasPropertyName"].rsplit("#has", maxsplit=1)[-1]
            accum[key].append(
                OSProperty(
                    value=float(binding["PropertyNameValue"]),
                    unit=binding["PropertyNameUnitLabel"],
                    reference_state_value=binding.get("ReferenceStateValue"),
                    reference_state_unit=binding.get("ReferenceStateUnitLabel"),
                )
            )
        return dict(accum)

    def retrieve_chemclasses(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT DISTINCT ?ChemicalClassLabel WHERE {{
    <{SpeciesIri}> (rdf:|!rdf:)+ [ 
        a os:ChemicalClass ; 
        rdfs:label ?ChemicalClassLabel 
    ] .
}}"""
        query = query_template.format(SpeciesIri=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [x["ChemicalClassLabel"]["value"] for x in response_bindings]

    def retrieve_uses(self, entity_iri: str):
        use_blacklist = [
            "Other",
            "Other (specify)",
            "Not Known or Reasonably Ascertainable",
        ]

        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT DISTINCT ?UseLabel WHERE {{
    <{SpeciesIri}> os:hasUse/rdfs:label ?UseLabel .
}}"""
        query = query_template.format(SpeciesIri=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        uses = [x["UseLabel"]["value"] for x in response_bindings]
        return [x for x in uses if x not in use_blacklist]
