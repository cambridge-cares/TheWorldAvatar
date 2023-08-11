import random
from generate_data.template_utils import add_space_and_lower

### 

def get_property_from_chemclass(PropertyName, ChemClass):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue  ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label rdfs:label ?ChemicalClassValue .

    FILTER( ?ChemicalClassValue = "{ChemClass}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
    FILTER( ?ChemicalClassValue = "{ChemClass}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Can you provide the {PropertyName} of species classified as {ChemClass}? 
Can you provide the {PropertyName} of {ChemClass} species?
Can you furnish the {PropertyName} of chemical species categorized as {ChemClass}?
Could you offer the {PropertyName} related to molecules grouped as {ChemClass}?
What is the {PropertyName} of compounds designated under {ChemClass}?
Please share the {PropertyName} pertaining to chemical species sorted as {ChemClass}.
I'm interested in knowing the {PropertyName} for molecules classified as {ChemClass}.
Provide the {PropertyName} for compounds that fall into the {ChemClass} category.
What's the {PropertyName} of chemical entities belonging to {ChemClass}?
Tell me the {PropertyName} associated with molecules that are part of {ChemClass}.
I'd like to learn the {PropertyName} of compounds characterized by {ChemClass}.
Share the {PropertyName} for chemical species grouped under {ChemClass}.
What would be the {PropertyName} of molecules assigned to {ChemClass}?
Inform me about the {PropertyName} corresponding to compounds in the {ChemClass} classification.
What's the {PropertyName} for chemical entities falling into the {ChemClass} category?
Could you disclose the {PropertyName} related to molecules sorted into {ChemClass}?
Provide information on the {PropertyName} of compounds identified as {ChemClass}.
I'm curious about the {PropertyName} of chemical species organized under {ChemClass}.
What's the {PropertyName} for molecules categorized as {ChemClass}?
Tell me the {PropertyName} of compounds labeled within the {ChemClass} group.
Please elaborate on the {PropertyName} associated with chemical species attributed to {ChemClass}.
Give me details about the {PropertyName} of molecules classed as {ChemClass}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact,
    )

####

def get_two_property_from_chemclass(PropertyName1, PropertyName2, ChemClass):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .

    FILTER( ?ChemicalClassValue = "{ChemClass}")

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
    FILTER( ?ChemicalClassValue = "{ChemClass}")
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
}}"""

    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""What are the {PropertyName1} and {PropertyName2} of species classified as {ChemClass}?
What are the {PropertyName1} and {PropertyName2} of {ChemClass} species?
What are the {PropertyName1} and {PropertyName2} of chemical species grouped as {ChemClass}?
What are the {PropertyName1} and {PropertyName2} of molecules categorized under {ChemClass}?
What are the {PropertyName1} and {PropertyName2} of compounds designated as {ChemClass}?
What {PropertyName1} and {PropertyName2} do species of the {ChemClass} classification possess?
What {PropertyName1} and {PropertyName2} are associated with chemical species classified as {ChemClass}?
Which {PropertyName1} and {PropertyName2} characterize molecules falling into the {ChemClass} category?
Identify the {PropertyName1} and {PropertyName2} of compounds belonging to the {ChemClass} classification.
Enumerate the {PropertyName1} and {PropertyName2} traits of species that fit the {ChemClass} category.
Provide details about the {PropertyName1} and {PropertyName2} of chemical species that fall under {ChemClass}.
Elaborate on the {PropertyName1} and {PropertyName2} of molecules attributed to the {ChemClass} grouping.
Explain the {PropertyName1} and {PropertyName2} inherent to compounds classified as {ChemClass}.
Outline the {PropertyName1} and {PropertyName2} features exhibited by species characterized as {ChemClass}.
Delve into the {PropertyName1} and {PropertyName2} of chemical species placed in the {ChemClass} category.
Elucidate the {PropertyName1} and {PropertyName2} associated with molecules that pertain to {ChemClass}.
Describe the {PropertyName1} and {PropertyName2} traits of compounds falling within the {ChemClass} classification.
What are the {PropertyName1} and {PropertyName2} attributes of species labeled as {ChemClass}?
What are the {PropertyName1} and {PropertyName2} qualities of molecules sorted into the {ChemClass} category?
What are the {PropertyName1} and {PropertyName2} elements of compounds grouped as {ChemClass}?
Mention the {PropertyName1} and {PropertyName2} that define species recognized as {ChemClass}.
Point out the {PropertyName1} and {PropertyName2} traits of chemical species identified under {ChemClass}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact,
    )

###

def get_three_property_from_chemclass(PropertyName1, PropertyName2, PropertyName3, ChemClass):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue ?{PropertyName3}Value ?{PropertyName3}UnitValue ?{PropertyName3}ReferenceStateValue ?{PropertyName3}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label rdfs:label ?ChemicalClassValue .

    FILTER( ?ChemicalClassValue = "{ChemClass}")

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    ?SpeciesIRI os:has{PropertyName3} ?{PropertyName3}IRI .
    ?{PropertyName3}IRI os:value ?{PropertyName3}Value ; os:unit ?{PropertyName3}UnitIRI ; os:hasProvenance ?{PropertyName3}ProvenanceIRI . 
    ?{PropertyName3}UnitIRI rdfs:label ?{PropertyName3}UnitValue .
    OPTIONAL{{?{PropertyName3}IRI os:hasReferenceState ?{PropertyName3}ReferenceStateIRI .
    ?{PropertyName3}ReferenceStateIRI os:value ?{PropertyName3}ReferenceStateValue ; os:unit ?{PropertyName3}ReferenceStateUnitIRI .
    ?{PropertyName3}ReferenceStateUnitIRI rdfs:label ?{PropertyName3}ReferenceStateUnitValue .}}
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value  ?{PropertyName3}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
    FILTER( ?ChemicalClassValue = "{ChemClass}")
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    ?SpeciesIRI os:hasProperty{PropertyName3} ?{PropertyName3}Value .
}}"""

    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)
    PropertyName3 = add_space_and_lower(PropertyName3)

    question_text = f"""What are the {PropertyName1}, {PropertyName2} and {PropertyName3} of species classified as {ChemClass}?
What are the {PropertyName1}, {PropertyName2} and {PropertyName3} of {ChemClass} species
What are the {PropertyName1}, {PropertyName2}, and {PropertyName3} for chemical species grouped under {ChemClass}?
Provide the {PropertyName1}, {PropertyName2}, and {PropertyName3} details for molecules categorized as {ChemClass}.
What are the {PropertyName1}, {PropertyName2}, and {PropertyName3} attributes associated with compounds belonging to {ChemClass}?
Share the {PropertyName1}, {PropertyName2}, and {PropertyName3} characteristics of species identified as {ChemClass}.
Give insights into the {PropertyName1}, {PropertyName2}, and {PropertyName3} traits linked to chemical species falling under {ChemClass}.
What do you know about the {PropertyName1}, {PropertyName2}, and {PropertyName3} features of molecules grouped as {ChemClass}?
Can you list the {PropertyName1}, {PropertyName2}, and {PropertyName3} attributes of compounds that are part of {ChemClass}?
Discuss the {PropertyName1}, {PropertyName2}, and {PropertyName3} properties of species classified under {ChemClass}.
Elaborate on the {PropertyName1}, {PropertyName2}, and {PropertyName3} particulars of chemical species categorized as {ChemClass}.
Highlight the {PropertyName1}, {PropertyName2}, and {PropertyName3} characteristics for molecules falling into the {ChemClass} group.
What are the {PropertyName1}, {PropertyName2}, and {PropertyName3} traits associated with compounds classified within {ChemClass}?
Explain the {PropertyName1}, {PropertyName2}, and {PropertyName3} attributes pertaining to species designated as {ChemClass}.
Could you provide information on the {PropertyName1}, {PropertyName2}, and {PropertyName3} qualities of chemical species sorted under {ChemClass}?
Please outline the {PropertyName1}, {PropertyName2}, and {PropertyName3} features of molecules labeled as {ChemClass}.
Detail the {PropertyName1}, {PropertyName2}, and {PropertyName3} aspects of compounds categorized in the {ChemClass} class.
What can you tell me about the {PropertyName1}, {PropertyName2}, and {PropertyName3} attributes of species recognized as {ChemClass}?
I'm interested in learning about the {PropertyName1}, {PropertyName2}, and {PropertyName3} traits belonging to chemical species characterized under {ChemClass}.
Provide insights into the {PropertyName1}, {PropertyName2}, and {PropertyName3} properties of molecules organized within the {ChemClass} category.
What are the {PropertyName1}, {PropertyName2}, and {PropertyName3} specifics concerning compounds classified as {ChemClass} species?
Discuss the {PropertyName1}, {PropertyName2}, and {PropertyName3} particulars of species belonging to the {ChemClass} classification."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

