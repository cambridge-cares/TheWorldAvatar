import random

from data_generation.constants import IDENTIFIER_LABELS, PROPERTY_LABELS, SPECIES
from data_generation.create_training_data_from_kg.constants import INTERROGATIVE


class ExampleMakerFromTemplate:
    def make_h2t_property(self, PropertyName: str):
        species = random.choice(SPECIES)
        verbalization = f"What is the {random.choice(PROPERTY_LABELS[PropertyName])} of {{species}}?"
        bindings = dict(species=species)
        sparql_query = f"""SELECT DISTINCT ?label ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    VALUES ( ?species ) {{ ( "{species}" ) }}
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL {{
        ?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
        ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
        ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .
    }}
}}"""
        sparql_query_compact = f"""SELECT ?{PropertyName}Value
WHERE {{
    VALUES ( ?species ) {{ ( "{species}") }}
    ?SpeciesIRI ?hasIdentifier ?species .
    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}Value .
}}"""
        return dict(
            verbalization=verbalization,
            verbalization_type=INTERROGATIVE,
            bindings=bindings,
            sparql_query=sparql_query,
            sparql_query_compact=sparql_query_compact,
            query_path="h2t",
            subgraph_type="1h_1t",
        )

    def make_h2t_identifier(self, IdentifierName: str):
        species = random.choice(SPECIES)
        verbalization = f"What is the {random.choice(IDENTIFIER_LABELS[IdentifierName])} of {{species}}?"
        bindings = dict(species=species)
        sparql_query = f"""SELECT DISTINCT ?label ?{IdentifierName}Value
WHERE {{
    VALUES ( ?species ) {{ ( "{species}" ) }}
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    ?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}IRI .
    ?{IdentifierName}IRI os:value ?{IdentifierName}Value .
}}"""
        sparql_query_compact = f"""SELECT ?{IdentifierName}Value
WHERE {{
    VALUES ( ?species ) {{ ( "{species}" ) }}
    ?SpeciesIRI ?hasIdentifier ?species .
    ?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}Value .
}}"""
        return dict(
            verbalization=verbalization,
            verbalization_type=INTERROGATIVE,
            bindings=bindings,
            sparql_query=sparql_query,
            sparql_query_compact=sparql_query_compact,
            query_path="h2t",
            subgraph_type="1h_1t",
        )

    def make_1h_propertytails(self):
        species = random.choice(SPECIES)
        verbalization = "What are the properties of {species}?"
        bindings = dict(species=species)
        sparql_query = f"""SELECT DISTINCT ?label ?PropertyLabel ?PropertyNameValue ?PropertyNameUnitValue ?PropertyNameReferenceStateValue ?PropertyNameReferenceStateUnitValue
WHERE {{
    VALUES ( ?species ) {{ ( "{species}" ) }}
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    ?SpeciesIRI ?hasPropertyName ?PropertyNameIRI .
    ?PropertyNameIRI rdf:type ?PropertyName .
    ?PropertyName rdfs:subClassOf os:Property .
    ?PropertyNameIRI os:value ?PropertyNameValue ; os:unit ?PropertyNameUnitIRI ; os:hasProvenance ?PropertyNameProvenanceIRI .
    ?PropertyNameUnitIRI rdfs:label ?PropertyNameUnitValue .
    OPTIONAL {{
        ?PropertyNameIRI os:hasReferenceState ?PropertyNameReferenceStateIRI .
        ?PropertyNameReferenceStateIRI os:value ?PropertyNameReferenceStateValue ; os:unit ?PropertyNameReferenceStateUnitIRI .
        ?PropertyNameReferenceStateUnitIRI rdfs:label ?PropertyNameReferenceStateUnitValue .
    }}

    BIND(strafter(str(?PropertyName),'#') AS ?PropertyLabel)
}}"""
        sparql_query_compact = f"""SELECT ?PropertyNameValue
WHERE {{
    VALUES ( ?species ) {{ ( "{species}") }}
    ?SpeciesIRI ?hasIdentifier ?species .
    ?SpeciesIRI ?hasPropertyName ?PropertyNameValue .
}}"""
        return dict(
            verbalization=verbalization,
            verbalization_type=INTERROGATIVE,
            bindings=bindings,
            sparql_query=sparql_query,
            sparql_query_compact=sparql_query_compact,
            query_path="h2t",
            subgraph_type="1h_nt",
        )

    def make_1h_identifiertails(self):
        species = random.choice(SPECIES)
        verbalization = "What are the identifiers of {species}?"
        bindings = dict(species=species)
        sparql_query = f"""SELECT DISTINCT ?label ?IdentifierLabel ?IdentifierNameValue
WHERE {{
    VALUES ( ?species ) {{ ( "{species}" ) }}
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    ?SpeciesIRI ?hasIdentifierName ?IdentifierNameIRI .
    ?IdentifierNameIRI rdf:type ?IdentifierName .
    ?IdentifierName rdfs:subClassOf os:Identifier .
    ?IdentifierNameIRI os:value ?IdentifierNameValue .

    BIND(strafter(str(?IdentifierName),'#') AS ?IdentifierLabel)
}}"""
        sparql_query_compact = f"""SELECT ?IdentifierNameValue
WHERE {{
    VALUES ( ?species ) {{ ( "{species}") }}
    ?SpeciesIRI ?hasIdentifier ?species .
    ?SpeciesIRI ?hasIdentifierName ?IdentifierNameValue . 
}}"""
        return dict(
            verbalization=verbalization,
            verbalization_type=INTERROGATIVE,
            bindings=bindings,
            sparql_query=sparql_query,
            sparql_query_compact=sparql_query_compact,
            query_path="h2t",
            subgraph_type="1h_nt",
        )
