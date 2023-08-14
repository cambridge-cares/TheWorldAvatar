import random
from generate_data.template_utils import add_space_and_lower

### 

def get_property_from_species(PropertyName, species):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}
}}"""

    query_text_compact = f"""SELECT DISTINCT ?{PropertyName}Value
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
}}"""
  
    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Can you provide the {PropertyName} of {species}? 
What {PropertyName} does {species} have?
What is the {PropertyName} of {species}?
Could you tell me the {PropertyName} of {species}?
Can you provide the {PropertyName} of {species}?
Can you share the {PropertyName} of {species} with me?
Could you inform me of the {PropertyName} of {species}?
What is the exact {PropertyName} of {species}?
What is the {PropertyName} for {species}?
Can you let me know the {PropertyName} of {species}?
Do you know the {PropertyName} of {species}?
What is the {species} {PropertyName}?
{PropertyName} of {species}
I'm curious about the {PropertyName} of {species}. Can you provide that information?
Do you happen to know the {PropertyName} of {species}?
What can you tell me about the {PropertyName} of {species}?
I'd like to learn more about the {PropertyName} of {species}. Can you help?
I'm interested in finding out the {PropertyName} of {species}. Any insights?
What details do you have regarding the {PropertyName} of {species}?
Tell me about the {PropertyName} of {species}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

####

def get_two_property_from_species(PropertyName1, PropertyName2, species):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species}")

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

    query_text_compact = f"""SELECT DISTINCT ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species}")
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""What are the {PropertyName1} and {PropertyName2} of {species}?
{PropertyName1} and the {PropertyName2} of {species}
Could you list the {PropertyName1} and {PropertyName2} pertaining to {species}?
What do we know about the {PropertyName1} and {PropertyName2} of {species}?
Tell me about {species}'s {PropertyName1} and {PropertyName2}.
What are the {PropertyName1} and {PropertyName2} of {species}?
Could you describe the {PropertyName1} and {PropertyName2} of {species}?
I'm interested in learning about {species}'s {PropertyName1} and {PropertyName2}. Can you share?
Provide information on the {PropertyName1} and {PropertyName2} of {species}.
What can you tell me regarding {species}'s {PropertyName1} and {PropertyName2}?
Delve into the details of {species}'s {PropertyName1} and {PropertyName2}, please.
Discuss the {PropertyName1} and {PropertyName2} associated with {species}.
I'd like to know more about the {PropertyName1} and {PropertyName2} of {species}.
Can you elaborate on {species}'s {PropertyName1} and {PropertyName2}?
Tell me how the {PropertyName1} and {PropertyName2} of {species} manifest.
What are the distinguishing {PropertyName1} and {PropertyName2} of {species}?
Let's talk about {species}'s {PropertyName1} and {PropertyName2} qualities.
Enlighten me on the subject of {species}'s {PropertyName1} and {PropertyName2}.
What's the scoop on {species}'s {PropertyName1} and {PropertyName2} attributes?
I'm curious about {species}. What can you share about its {PropertyName1} and {PropertyName2}?
Could you give me an overview of {species}'s {PropertyName1} and {PropertyName2}?
Share some insights into the {PropertyName1} and {PropertyName2} of {species}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_three_property_from_species(PropertyName1, PropertyName2, PropertyName3, species):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue ?{PropertyName3}Value ?{PropertyName3}UnitValue ?{PropertyName3}ReferenceStateValue ?{PropertyName3}ReferenceStateUnitValue 
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species}")

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

    query_text_compact = f"""SELECT DISTINCT ?{PropertyName1}Value ?{PropertyName2}Value ?{PropertyName3}Value 
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species}")
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    ?SpeciesIRI os:hasProperty{PropertyName3} ?{PropertyName3}Value .
}}"""

    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)
    PropertyName3 = add_space_and_lower(PropertyName3)

    question_text = f"""What are the {PropertyName1}, {PropertyName2} and {PropertyName3} of {species}?
Can you list the {PropertyName1}, {PropertyName2}, and {PropertyName3} associated with {species}?
Provide the {PropertyName1}, {PropertyName2}, and {PropertyName3} of {species}.
What are the {PropertyName1}, {PropertyName2}, and {PropertyName3} of {species}?
Enumerate the {PropertyName1}, {PropertyName2}, and {PropertyName3} of {species}.
I'm curious about the {PropertyName1}, {PropertyName2}, and {PropertyName3} of {species}. Can you elaborate?
Could you tell me about the {PropertyName1}, {PropertyName2}, and {PropertyName3} that define {species}?
{PropertyName1}, {PropertyName2}, and {PropertyName3} of {species}.
I'd like to know the {PropertyName1}, {PropertyName2}, and {PropertyName3} that pertain to {species}.
What are {species}'s {PropertyName1}, {PropertyName2}, and {PropertyName3}?
Tell me about the {PropertyName1}, {PropertyName2}, and {PropertyName3} exhibited by {species}.
Discuss the {PropertyName1}, {PropertyName2}, and {PropertyName3} attributed to {species}.
I'm interested in the {PropertyName1}, {PropertyName2}, and {PropertyName3} that define {species}.
Can you shed light on the {PropertyName1}, {PropertyName2}, and {PropertyName3} observed in {species}?
What can you tell me about {species}'s {PropertyName1}, {PropertyName2}, and {PropertyName3}?
Let's delve into the {PropertyName1}, {PropertyName2}, and {PropertyName3} that characterize {species}.
Describe the {PropertyName1}, {PropertyName2}, and {PropertyName3} associated with {species}.
Enlighten me about the {PropertyName1}, {PropertyName2}, and {PropertyName3} specific to {species}.
Share insights into the {PropertyName1}, {PropertyName2}, and {PropertyName3} of {species}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_identifier_from_species(IdentifierName, species):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?{IdentifierName}Value
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species}")

    ?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}IRI .
    ?{IdentifierName}IRI os:value ?{IdentifierName}Value .
}}"""

    query_text_compact = f"""SELECT DISTINCT ?{IdentifierName}Value
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species}")
    ?SpeciesIRI os:hasIdentifier{IdentifierName} ?{IdentifierName}Value .
}}"""
  
    IdentifierName = add_space_and_lower(IdentifierName)

    question_text = f"""What is the {IdentifierName} of {species}?
Can you provide the {IdentifierName} for {species}?
Give me the {IdentifierName} that corresponds to {species}.
What's the {IdentifierName} that defines {species}?
What's the {IdentifierName} assigned to {species}?
What's {species}'s {IdentifierName}?
Tell me about the {IdentifierName} in relation to {species}.
Provide information about the {IdentifierName} of {species}.
I'm curious about the {IdentifierName} of {species}. what is it?
What's the official term for {species}'s {IdentifierName}?
I'd like to know the {IdentifierName} attributed to {species}.
Can you enlighten me about {species}'s {IdentifierName}?
What's the {IdentifierName} denoting {species}?
Please elaborate on the {IdentifierName} of {species}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_chemclass_from_species(species):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?ChemicalClassValue 
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species}")

	?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .
}}"""

    query_text_compact = f"""SELECT DISTINCT ?ChemicalClassValue 
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species}")
	?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
}}"""


    question_text = f"""What are the chemical classes of {species}?
Could you list the chemical classes that {species} belongs to?
What are the categorizations of {species} in terms of chemical classes?
Which chemical classes is {species} grouped into?
Can you provide information about the chemical classes attributed to {species}?
What are the identified chemical classes of {species}?
How is {species} classified in relation to chemical classes?
What are the distinct chemical classes associated with {species}?
Could you elaborate on the chemical classes that define {species}?
What groupings into chemical classes can be made for {species}?
What is the breakdown of {species} in terms of chemical classes?
What are the chemical class assignments for {species}?
What are the established chemical classes that {species} falls under?
How can {species} be categorized based on its chemical classes?
What are the specific chemical classes that characterize {species}?
What are the primary chemical classes to which {species} belongs?
What are the known chemical classes that pertain to {species}?
How would you describe the chemical classes of {species}?
What do we know about the chemical classes that {species} is part of?
What can you tell me about the chemical classes categorization for {species}?"""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_use_from_species(species):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?UseValue 
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species}")

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI os:value ?UseValue .
}}"""

    query_text_compact = f"""SELECT DISTINCT ?UseValue 
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species}")
    ?SpeciesIRI os:hasUse ?UseValue .
}}"""
  
    question_text = f"""What are the uses of {species}?
How can {species} be utilized?
What purposes can {species} serve?
In what ways is {species} used?
What are the practical applications of {species}?
How are {species} employed?
What functions does {species} have?
What are the different ways to use {species}?
What are the potential uses for {species}?
How do people make use of {species}?
What are {species} typically used for?
What are some applications for {species}?
How do various industries utilize {species}?
What roles does {species} play in various contexts?
What are the industrial and commercial uses of {species}?
What are the practical benefits of {species}?
How does {species} find its use in different fields?
What are the functionalities of {species}?
What are the diverse applications of {species}?
What are the ways people harness {species}?
How can {species} contribute to different endeavors?"""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )
###

def get_use_and_chemclass_from_species(species):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?UseValue ?ChemicalClassValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species}")

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI os:value ?UseValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .
}}"""

    query_text_compact = f"""SELECT DISTINCT ?UseValue ?ChemicalClassValue
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species}")
    ?SpeciesIRI os:hasUse ?UseValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
}}"""
  
    question_text = f"""What are the chemical classes and uses of {species}?
What are the various chemical categories and applications of {species}?
Could you outline the chemical groupings and practical uses of {species}?
What are the classifications of chemicals and how is {species} employed?
Please detail the chemical families and purposes of {species}.
What chemical classes does {species} belong to, and how are they utilized?
I'm curious about the chemical groupings and functions of {species}, could you explain?
Could you elucidate the chemical categories and real-world applications of {species}?
What can you tell me about {species} in terms of chemical classes and its various uses?
Please provide information on the chemical classifications and utility of {species}.
How is {species} categorized chemically, and what are its common applications?
What are the chemical groupings that {species} falls into, and what are its uses?
I'd like to know about the chemical classes and practical uses associated with {species}.
Can you give me insights into the chemical categories and usage scenarios for {species}?
What are the chemical families and applications attributed to {species}?
Tell me more about the chemical classes of {species} and the ways it is utilized.
What chemical groups does {species} encompass, and what are its typical uses?
I'm interested in learning about {species}—its chemical categories and applications.
Could you break down the chemical classifications and functions of {species}?
What do we know about the chemical classes and potential uses of {species}?
Share some information about {species} and its chemical classes, along with its uses."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_all_properties(species):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?PropertyLabel ?PropertyNameValue ?PropertyNameUnitValue ?PropertyNameReferenceStateValue ?PropertyNameReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species}")

    ?SpeciesIRI ?hasPropertyName ?PropertyNameIRI .
    ?PropertyNameIRI  rdf:type ?PropertyName .
    ?PropertyName rdfs:subClassOf os:Property .
    ?PropertyNameIRI os:value ?PropertyNameValue ; os:unit ?PropertyNameUnitIRI ; os:hasProvenance ?PropertyNameProvenanceIRI . 
    ?PropertyNameUnitIRI rdfs:label ?PropertyNameUnitValue .
    OPTIONAL{{?PropertyNameIRI os:hasReferenceState ?PropertyNameReferenceStateIRI . 
    ?PropertyNameReferenceStateIRI os:value ?PropertyNameReferenceStateValue ; os:unit ?PropertyNameReferenceStateUnitIRI .
    ?PropertyNameReferenceStateUnitIRI rdfs:label ?PropertyNameReferenceStateUnitValue .}}
    
  	BIND(strafter(str(?PropertyName),'#') AS ?PropertyLabel)   
}}"""

    query_text_compact = f"""SELECT DISTINCT ?PropertyNameValue
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species}")
    ?SpeciesIRI ?hasPropertyName ?PropertyNameValue .
}}"""

    question_text = f"""What are the properties of {species}?
What characteristics does {species} possess?
Can you list the attributes of {species}?
What are the distinguishing features of {species}?
Please describe the traits of {species}.
What are the unique qualities of {species}?
Could you outline the properties of {species}?
What can you tell me about {species}'s properties?
What makes up the nature of {species}?
What are the specific traits that define {species}?
Could you provide an overview of {species}'s properties?
What are {species}'s essential characteristics?
What are the key elements describing {species}?
How would you define the attributes of {species}?
What set of features characterizes {species}?
What are the inherent traits of {species}?
What do we know about {species}'s properties?
What are {species}'s fundamental properties?
What attributes can be linked to {species}?
What is known about the properties of {species}?
How would you detail {species}'s properties?
{species}
Give me a summary of {species}
Provide an overview of the {species}.
Can you give me a brief description of the {species}?
What are the key characteristics of the {species}?
Give me a summary of the physical traits of the {species}.
Give me a quick rundown of {species}.
I need a concise description of {species}.
What are the key points about {species}?
Give me an abridged summary of {species}.
I'm looking for a compact summary of {species}.
What are the main characteristics of {species}?
Give me a brief outline of {species}.
Can you summarize the important aspects of {species}?
I need a summarized version of {species}.
What are the key features of {species}?
Give me a condensed overview of {species}.
Can you provide a summary highlighting the main points about {species}?
What are the essential details about {species}?
Give me a summarized explanation of {species}.
Can you briefly describe {species}?"""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_all_identifiers(species):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IdentifierLabel ?IdentifierNameValue 
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species}")

    ?SpeciesIRI ?hasIdentifierName ?IdentifierNameIRI .
    ?IdentifierNameIRI  rdf:type ?IdentifierName .
    ?IdentifierName rdfs:subClassOf os:Identifier .
    ?IdentifierNameIRI os:value ?IdentifierNameValue .
    
  	BIND(strafter(str(?IdentifierName),'#') AS ?IdentifierLabel)   
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IdentifierNameValue
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species}")
    ?SpeciesIRI ?hasIdentifierName ?IdentifierNameValue . 
}}"""
  
    question_text = f"""What are the identifiers of {species}?
Which attributes uniquely identify {species}?
Can you list the markers that define {species}?
What labels can be used to recognize {species}?
What are the labels of {species}?
Which attributes are employed to establish {species} identity?
What are the hallmark identifiers of {species}?
What are the unique identifiers for {species} recognition?"""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_compare_properties(PropertyName, species1, species2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI  rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .

    FILTER( ?species = "{species1}" || ?species = "{species2}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}
}}"""

    query_text_compact = f"""SELECT DISTINCT ?{PropertyName}Value
WHERE {{
    ?SpeciesIRI ?hasIdentifier ?species .
    FILTER( ?species = "{species1}" || ?species = "{species2}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
}}"""
  
    question_text = f"""Compare {PropertyName} of {species1} and {species2}
What is the {PropertyName} of {species1} and {species2}
Analyze the {PropertyName} in {species1} and {species2}.
Contrast the {PropertyName} between {species1} and {species2}.
Examine how the {PropertyName} differs in {species1} and {species2}.
What are the differences in the {PropertyName} of {species1} and {species2}?
Investigate the {PropertyName} variations in {species1} and {species2}.
How does the {PropertyName} compare in {species1} and {species2}?
Compare the {PropertyName} exhibited by {species1} and {species2}.
What distinguishes the {PropertyName} of {species1} from {species2}?
Delve into the {PropertyName} distinctions between {species1} and {species2}.
Explore the similarities and differences in {PropertyName} between {species1} and {species2}.
Contrast the {PropertyName} features of {species1} and {species2}.
Compare and contrast the {PropertyName} of {species1} and {species2}.
Analyze the similarities and disparities in {PropertyName} between {species1} and {species2}.
Examine the variations in {PropertyName} demonstrated by {species1} and {species2}.
What can be said about the {PropertyName} in {species1} versus {species2}?
Investigate how the {PropertyName} varies between {species1} and {species2}.
Compare the {PropertyName} characteristics of {species1} and {species2}.
Explore the {PropertyName} distinctions in {species1} and {species2}.
What contrasts can be drawn regarding the {PropertyName} of {species1} and {species2}?
Break down the differences in {PropertyName} between {species1} and {species2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )
