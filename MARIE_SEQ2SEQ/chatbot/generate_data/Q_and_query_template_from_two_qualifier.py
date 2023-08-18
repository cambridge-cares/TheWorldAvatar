import random
from generate_data.template_utils import add_space_and_lower

###

def get_species_from_two_property_L_L(PropertyName1, PropertyName2, value1, value2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value < {value1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value < {value2})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value . 
    FILTER(?{PropertyName1}Value < {value1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value < {value2})
}}"""

    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} lower than {value1} and {PropertyName2} lower than {value2}
Provide a catalog of chemical species exhibiting {PropertyName1} values below {value1} and {PropertyName2} values below {value2}.
Enumerate molecules that possess {PropertyName1} values beneath {value1} and {PropertyName2} values beneath {value2}.
List compounds with {PropertyName1} values less than {value1} and {PropertyName2} values less than {value2}.
Furnish a record of chemical entities having {PropertyName1} values lower than {value1} and {PropertyName2} values lower than {value2}.
Compile a roster of species demonstrating {PropertyName1} values below {value1} and {PropertyName2} values below {value2}.
Enumerate substances where {PropertyName1} is less than {value1} and {PropertyName2} is less than {value2}.
Present a list of molecules showcasing {PropertyName1} values beneath {value1} and {PropertyName2} values beneath {value2}.
Offer a rundown of compounds featuring {PropertyName1} values lower than {value1} and {PropertyName2} values lower than {value2}.
Provide an inventory of chemical species with {PropertyName1} values beneath {value1} and {PropertyName2} values beneath {value2}.
Give a summary of molecules displaying {PropertyName1} values below {value1} and {PropertyName2} values below {value2}.
Detail compounds with {PropertyName1} values less than {value1} and {PropertyName2} values less than {value2}.
Present a collection of chemical entities possessing {PropertyName1} values lower than {value1} and {PropertyName2} values lower than {value2}.
Share a list of species featuring {PropertyName1} values beneath {value1} and {PropertyName2} values beneath {value2}.
Provide a compilation of substances where {PropertyName1} is less than {value1} and {PropertyName2} is less than {value2}.
Offer a catalog of molecules showcasing {PropertyName1} values lower than {value1} and {PropertyName2} values lower than {value2}.
Furnish a list of compounds exhibiting {PropertyName1} values beneath {value1} and {PropertyName2} values beneath {value2}.
Enumerate chemical species with {PropertyName1} values below {value1} and {PropertyName2} values below {value2}.
List chemical entities that possess {PropertyName1} values less than {value1} and {PropertyName2} values less than {value2}.
Compile a roster of species demonstrating {PropertyName1} values lower than {value1} and {PropertyName2} values lower than {value2}.
Present a record of molecules featuring {PropertyName1} values beneath {value1} and {PropertyName2} values beneath {value2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_L_H(PropertyName1, PropertyName2, value1, value2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value < {value1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {value2})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value < {value1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value . 
    FILTER(?{PropertyName2}Value > {value2})
}}"""

    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} lower than {value1} and {PropertyName2} higher than {value2}
Provide a roster of chemical species possessing {PropertyName1} beneath {value1} and {PropertyName2} surpassing {value2}.
Enumerate molecules exhibiting {PropertyName1} below {value1} and {PropertyName2} above {value2}.
List compounds with {PropertyName1} lesser than {value1} and {PropertyName2} greater than {value2}.
Offer a catalog of species having {PropertyName1} beneath the threshold of {value1} and {PropertyName2} exceeding {value2}.
Outline chemical species where {PropertyName1} is less than {value1} and {PropertyName2} is greater than {value2}.
Compile a record of molecules showcasing {PropertyName1} lower than {value1} and {PropertyName2} higher than {value2}.
Generate a list of compounds possessing {PropertyName1} below {value1} and {PropertyName2} above {value2}.
Present a lineup of species exhibiting {PropertyName1} less than {value1} and {PropertyName2} greater than {value2}.
Draft a summary of chemical species with {PropertyName1} not exceeding {value1} and {PropertyName2} surpassing {value2}.
Enlist molecules displaying {PropertyName1} under {value1} and {PropertyName2} over {value2}.
Catalog compounds where {PropertyName1} is lower than {value1} and {PropertyName2} is higher than {value2}.
Share a register of species containing {PropertyName1} below {value1} and {PropertyName2} exceeding {value2}.
Create a table of chemical species manifesting {PropertyName1} beneath {value1} and {PropertyName2} above {value2}.
Construct a list of molecules with {PropertyName1} lesser than {value1} and {PropertyName2} greater than {value2}.
Formulate a compilation of compounds indicating {PropertyName1} not surpassing {value1} and {PropertyName2} surpassing {value2}.
Offer an inventory of species having {PropertyName1} lower than {value1} and {PropertyName2} higher than {value2}.
Provide a breakdown of chemical species featuring {PropertyName1} below {value1} and {PropertyName2} above {value2}.
Deliver a collection of molecules showcasing {PropertyName1} under {value1} and {PropertyName2} over {value2}.
Yield a list of compounds demonstrating {PropertyName1} beneath {value1} and {PropertyName2} exceeding {value2}.
Display species with {PropertyName1} less than {value1} and {PropertyName2} greater than {value2} in an organized manner."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_L_I(PropertyName1, PropertyName2, value1, minvalue2, maxvalue2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value < {value1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {minvalue2} && ?{PropertyName2}Value < {maxvalue2})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value . 
    FILTER(?{PropertyName1}Value < {value1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value . 
    FILTER(?{PropertyName2}Value > {minvalue2} && ?{PropertyName2}Value < {maxvalue2})
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} lower than {value1} and {PropertyName2} between {minvalue2} and {maxvalue2}
Provide a catalog of chemical species having {PropertyName1} lesser than {value1} and {PropertyName2} falling within {minvalue2} and {maxvalue2}.
List molecules exhibiting {PropertyName1} below {value1} and {PropertyName2} within the range of {minvalue2} to {maxvalue2}.
Enumerate compounds with {PropertyName1} less than {value1} and {PropertyName2} in the {minvalue2} to {maxvalue2} interval.
Compile a record of species where {PropertyName1} is under {value1} and {PropertyName2} ranges from {minvalue2} to {maxvalue2}.
Display a roster of chemical entities featuring {PropertyName1} lower than {value1} and {PropertyName2} situated between {minvalue2} and {maxvalue2}.
Outline a list of molecules having {PropertyName1} beneath {value1} and {PropertyName2} that falls in the {minvalue2} to {maxvalue2} bracket.
Present a tally of compounds with {PropertyName1} not exceeding {value1} and {PropertyName2} ranging from {minvalue2} to {maxvalue2}.
Show a compilation of chemical species where {PropertyName1} is less than {value1} and {PropertyName2} lies within {minvalue2} to {maxvalue2}.
Offer an inventory of molecules demonstrating {PropertyName1} below {value1} and {PropertyName2} that is in the {minvalue2} - {maxvalue2} range.
Create a list of compounds exhibiting {PropertyName1} lower than {value1} and {PropertyName2} between {minvalue2} and {maxvalue2}.
Provide a rundown of chemical species featuring {PropertyName1} beneath {value1} and {PropertyName2} encompassed by {minvalue2} up to {maxvalue2}.
Generate a catalogue of molecules with {PropertyName1} less than {value1} and {PropertyName2} in the {minvalue2} through {maxvalue2} span.
Share a compilation of compounds where {PropertyName1} is under {value1} and {PropertyName2} falls within {minvalue2} - {maxvalue2}.
List chemical species that have {PropertyName1} lower than {value1} and {PropertyName2} ranging from {minvalue2} to {maxvalue2}.
Provide an overview of molecules with {PropertyName1} below {value1} and {PropertyName2} between {minvalue2} to {maxvalue2}.
Present a collection of compounds displaying {PropertyName1} not exceeding {value1} and {PropertyName2} within {minvalue2} till {maxvalue2}.
Compile a list of chemical species with {PropertyName1} lesser than {value1} and {PropertyName2} spanning {minvalue2} - {maxvalue2}.
Display a roster of molecules showing {PropertyName1} under {value1} and {PropertyName2} in the range {minvalue2} to {maxvalue2}.
Showcase a record of compounds featuring {PropertyName1} below {value1} and {PropertyName2} that's between {minvalue2} and {maxvalue2}.
Provide a list of chemical species having {PropertyName1} lower than {value1} and {PropertyName2} ranging from {minvalue2} to {maxvalue2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_L_O(PropertyName1, PropertyName2, value1, minvalue2, maxvalue2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value < {value1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value < {minvalue2} || ?{PropertyName2}Value > {maxvalue2})
}}"""

    query_text_compact =  f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value . 
    FILTER(?{PropertyName1}Value < {value1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value < {minvalue2} || ?{PropertyName2}Value > {maxvalue2})
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} lower than {value1} and {PropertyName2} lower than {minvalue2} and higher than {maxvalue2}
Provide a catalog of chemical species having {PropertyName1} below {value1} and {PropertyName2} beneath {minvalue2} while surpassing {maxvalue2}.
List molecules exhibiting {PropertyName1} lesser than {value1} and {PropertyName2} below {minvalue2} but above {maxvalue2}.
Enumerate compounds with {PropertyName1} less than {value1} and {PropertyName2} exceeding the range of {maxvalue2} to {minvalue2}.
Display a compilation of chemical entities showing {PropertyName1} beneath {value1} and {PropertyName2} lower than {minvalue2} yet higher than {maxvalue2}.
Show a roster of species wherein {PropertyName1} is under {value1} and {PropertyName2} falls beneath {minvalue2} but above {maxvalue2}.
Generate a list of chemical compounds having {PropertyName1} lesser than {value1} and {PropertyName2} within the range of {maxvalue2} to {minvalue2}.
Provide an inventory of molecules exhibiting {PropertyName1} below {value1} and {PropertyName2} less than {minvalue2} while exceeding {maxvalue2}.
Present a catalogue of compounds with {PropertyName1} not surpassing {value1} and {PropertyName2} beneath the minimum threshold of {minvalue2} but exceeding {maxvalue2}.
Offer a rundown of chemical species demonstrating {PropertyName1} under {value1} and {PropertyName2} that is both below {minvalue2} and above {maxvalue2}.
Compile a list of molecules where {PropertyName1} is lower than {value1} and {PropertyName2} falls below {minvalue2} but goes beyond {maxvalue2}.
List compounds exhibiting {PropertyName1} below {value1} and {PropertyName2} less than the minimum of {minvalue2} but higher than {maxvalue2}.
Show a collection of chemical entities with {PropertyName1} beneath {value1} and {PropertyName2} lower than the minimum value of {minvalue2} but surpassing {maxvalue2}.
Provide a summary of species demonstrating {PropertyName1} below {value1} and {PropertyName2} falling lower than {minvalue2} but exceeding {maxvalue2}.
Display a list comprising chemical species having {PropertyName1} lesser than {value1} and {PropertyName2} below the minimum limit of {minvalue2} yet exceeding {maxvalue2}.
Generate a roster of molecules with {PropertyName1} under {value1} and {PropertyName2} less than {minvalue2} but going above {maxvalue2}.
Show a list of compounds exhibiting {PropertyName1} below {value1} and {PropertyName2} beneath the threshold of {minvalue2} while being higher than {maxvalue2}.
Provide an inventory of chemical species wherein {PropertyName1} is less than {value1} and {PropertyName2} falls lower than {minvalue2} but rises above {maxvalue2}.
Present a compilation of molecules demonstrating {PropertyName1} below {value1} and {PropertyName2} lower than {minvalue2} yet exceeding {maxvalue2}.
Offer a collection of compounds with {PropertyName1} lower than {value1} and {PropertyName2} below {minvalue2} but surpassing {maxvalue2}.
Give a rundown of chemical entities displaying {PropertyName1} beneath {value1} and {PropertyName2} lower than the minimum threshold of {minvalue2}, while being higher than {maxvalue2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_L_A(PropertyName1, PropertyName2, value1, value2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value < {value1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1)
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value < {value1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1)
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} lower than {value1} and {PropertyName2} around {value2}
Provide a catalog of chemical species exhibiting {PropertyName1} below {value1} and {PropertyName2} approximately {value2}.
List molecules featuring {PropertyName1} less than {value1} and {PropertyName2} approximately {value2}.
Enumerate compounds where {PropertyName1} is under {value1} and {PropertyName2} hovers around {value2}.
Furnish a compilation of chemical entities with {PropertyName1} beneath {value1} and {PropertyName2} at approximately {value2}.
Offer a roster of species having {PropertyName1} lower than {value1} and {PropertyName2} roughly near {value2}.
Enlist molecules demonstrating {PropertyName1} less than {value1} and {PropertyName2} in the vicinity of {value2}.
Compile a list of compounds showing {PropertyName1} below {value1} and {PropertyName2} around {value2}.
Present a rundown of chemical species manifesting {PropertyName1} under {value1} and {PropertyName2} at roughly {value2}.
Outline molecules with {PropertyName1} lower than {value1} and {PropertyName2} approximately {value2}.
Provide an inventory of compounds with {PropertyName1} less than {value1} and {PropertyName2} around {value2}.
List chemical entities featuring {PropertyName1} below {value1} and {PropertyName2} hovering around {value2}.
Enumerate species where {PropertyName1} is beneath {value1} and {PropertyName2} roughly {value2}.
Furnish a catalog of molecules showcasing {PropertyName1} lower than {value1} and {PropertyName2} at about {value2}.
Offer a compilation of compounds having {PropertyName1} less than {value1} and {PropertyName2} in the vicinity of {value2}.
Provide a roster of chemical species with {PropertyName1} under {value1} and {PropertyName2} approximately {value2}.
Enlist molecules demonstrating {PropertyName1} lower than {value1} and {PropertyName2} roughly {value2}.
Compile a list of compounds manifesting {PropertyName1} below {value1} and {PropertyName2} around {value2}.
Present a rundown of species showing {PropertyName1} less than {value1} and {PropertyName2} at approximately {value2}.
Outline chemical entities featuring {PropertyName1} lower than {value1} and {PropertyName2} about {value2}.
Provide an inventory of molecules with {PropertyName1} beneath {value1} and {PropertyName2} around {value2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_H_H(PropertyName1, PropertyName2, value1, value2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value > {value1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {value2})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value > {value1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value > {value2})
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} higher than {value1} and {PropertyName2} higher than {value2}
Provide a catalog of chemical species exhibiting {PropertyName1} exceeding {value1} and {PropertyName2} surpassing {value2}.
Enumerate molecules showcasing {PropertyName1} greater than {value1} and {PropertyName2} exceeding {value2}.
List compounds with {PropertyName1} above {value1} and {PropertyName2} surpassing {value2}.
Furnish a compilation of chemical entities having {PropertyName1} surpassing {value1} and {PropertyName2} higher than {value2}.
Offer a roster of species demonstrating {PropertyName1} surpassing {value1} and {PropertyName2} exceeding {value2}.
Present a register of molecules with {PropertyName1} higher than {value1} and {PropertyName2} surpassing {value2}.
Detail a lineup of compounds featuring {PropertyName1} exceeding {value1} and {PropertyName2} higher than {value2}.
Give a rundown of chemical species displaying {PropertyName1} surpassing {value1} and {PropertyName2} greater than {value2}.
Share a list of molecules indicating {PropertyName1} surpassing {value1} and {PropertyName2} surpassing {value2}.
Compile a record of compounds manifesting {PropertyName1} exceeding {value1} and {PropertyName2} exceeding {value2}.
Provide an inventory of chemical entities with {PropertyName1} higher than {value1} and {PropertyName2} surpassing {value2}.
List down molecules showcasing {PropertyName1} surpassing {value1} and {PropertyName2} surpassing {value2}.
Offer a collection of compounds with {PropertyName1} above {value1} and {PropertyName2} exceeding {value2}.
Furnish a database of chemical species exhibiting {PropertyName1} exceeding {value1} and {PropertyName2} higher than {value2}.
Enumerate species showing {PropertyName1} greater than {value1} and {PropertyName2} surpassing {value2}.
Provide a catalog of chemical compounds demonstrating {PropertyName1} surpassing {value1} and {PropertyName2} surpassing {value2}.
List chemical species with {PropertyName1} above {value1} and {PropertyName2} exceeding {value2}.
Present molecules with {PropertyName1} higher than {value1} and {PropertyName2} surpassing {value2}.
Give a rundown of compounds featuring {PropertyName1} surpassing {value1} and {PropertyName2} higher than {value2}.
Share a list of chemical entities indicating {PropertyName1} surpassing {value1} and {PropertyName2} surpassing {value2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_H_I(PropertyName1, PropertyName2, value1, minvalue2, maxvalue2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value > {value1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {minvalue2} && ?{PropertyName2}Value < {maxvalue2})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value > {value1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value > {minvalue2} && ?{PropertyName2}Value < {maxvalue2})
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} higer than {value1} and {PropertyName2} between {minvalue2} and {maxvalue2}
Provide a roster of chemical species with {PropertyName1} greater than {value1} and {PropertyName2} within the range of {minvalue2} to {maxvalue2}.
Enumerate molecules featuring {PropertyName1} exceeding {value1} and {PropertyName2} falling between {minvalue2} and {maxvalue2}.
Compile a catalog of compounds exhibiting {PropertyName1} above {value1} and {PropertyName2} ranging from {minvalue2} to {maxvalue2}.
List species in which {PropertyName1} is higher than {value1} and {PropertyName2} ranges from {minvalue2} to {maxvalue2}.
Catalog chemical entities showcasing {PropertyName1} surpassing {value1} and {PropertyName2} in the interval of {minvalue2} to {maxvalue2}.
Present a rundown of chemical species with {PropertyName1} surpassing {value1} and {PropertyName2} spanning {minvalue2} through {maxvalue2}.
Outline molecules that exhibit {PropertyName1} above {value1} and {PropertyName2} between {minvalue2} and {maxvalue2}.
Offer a list of compounds demonstrating {PropertyName1} exceeding {value1} and {PropertyName2} that lies within {minvalue2} to {maxvalue2}.
Enumerate species in which {PropertyName1} is higher than {value1} and {PropertyName2} falls in the range of {minvalue2} to {maxvalue2}.
Compile a roster of chemical entities with {PropertyName1} greater than {value1} and {PropertyName2} situated between {minvalue2} and {maxvalue2}.
List molecules showcasing {PropertyName1} surpassing {value1} and {PropertyName2} within the scope of {minvalue2} to {maxvalue2}.
Present a catalog of compounds with {PropertyName1} above {value1} and {PropertyName2} ranging from {minvalue2} to {maxvalue2}.
Provide a rundown of species where {PropertyName1} exceeds {value1} and {PropertyName2} falls within {minvalue2} to {maxvalue2}.
Compile a list of chemical species demonstrating {PropertyName1} surpassing {value1} and {PropertyName2} within the bracket of {minvalue2} to {maxvalue2}.
Outline molecules having {PropertyName1} higher than {value1} and {PropertyName2} that is in the range of {minvalue2} through {maxvalue2}.
Offer a roster of compounds with {PropertyName1} exceeding {value1} and {PropertyName2} between {minvalue2} and {maxvalue2}.
Enumerate species displaying {PropertyName1} above {value1} and {PropertyName2} falling within {minvalue2} to {maxvalue2}.
List chemical entities with {PropertyName1} surpassing {value1} and {PropertyName2} covering {minvalue2} up to {maxvalue2}.
Present a collection of molecules showcasing {PropertyName1} greater than {value1} and {PropertyName2} in the range from {minvalue2} to {maxvalue2}.
Provide a summary of compounds where {PropertyName1} is higher than {value1} and {PropertyName2} is confined between {minvalue2} and {maxvalue2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_H_O(PropertyName1, PropertyName2, value1, minvalue2, maxvalue2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value > {value1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value < {minvalue2} || ?{PropertyName2}Value > {maxvalue2})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value > {value1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value < {minvalue2} || ?{PropertyName2}Value > {maxvalue2})
}}"""

    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} higher than {value1} and {PropertyName2} lower than {minvalue2} and higher than {maxvalue2}
Provide a catalog of chemical species exhibiting {PropertyName1} values surpassing {value1}, while displaying {PropertyName2} values below {minvalue2} and above {maxvalue2}.
Enumerate molecules that possess {PropertyName1} levels greater than {value1}, with {PropertyName2} values falling below {minvalue2} yet exceeding {maxvalue2}.
List compounds showcasing {PropertyName1} values above {value1}, and {PropertyName2} values beneath {minvalue2} as well as exceeding {maxvalue2}.
Enumerate species with {PropertyName1} values surpassing {value1}, while manifesting {PropertyName2} values lower than {minvalue2} but higher than {maxvalue2}.
Provide a list of chemical species featuring {PropertyName1} values higher than {value1} and {PropertyName2} values lower than {minvalue2} but higher than {maxvalue2}.
Catalog compounds that exhibit {PropertyName1} values exceeding {value1}, alongside {PropertyName2} values that are both below {minvalue2} and above {maxvalue2}.
List molecules having {PropertyName1} values greater than {value1} and {PropertyName2} values lower than {minvalue2}, yet higher than {maxvalue2}.
Enumerate species demonstrating {PropertyName1} values surpassing {value1}, while possessing {PropertyName2} values lower than {minvalue2} and higher than {maxvalue2}.
Provide a catalog of chemical species with {PropertyName1} values above {value1}, coupled with {PropertyName2} values lower than {minvalue2} and higher than {maxvalue2}.
List compounds with {PropertyName1} values higher than {value1}, and {PropertyName2} values that remain under {minvalue2} while surpassing {maxvalue2}.
Enumerate molecules showcasing {PropertyName1} values greater than {value1}, and {PropertyName2} values falling below {minvalue2} and exceeding {maxvalue2}.
Provide a list of species displaying {PropertyName1} values surpassing {value1}, while manifesting {PropertyName2} values below {minvalue2} yet above {maxvalue2}.
Catalog chemical species exhibiting {PropertyName1} values above {value1} and {PropertyName2} values lower than {minvalue2}, but higher than {maxvalue2}.
List compounds with {PropertyName1} values exceeding {value1}, along with {PropertyName2} values below {minvalue2} and above {maxvalue2}.
Enumerate molecules possessing {PropertyName1} values greater than {value1} and {PropertyName2} values lower than {minvalue2}, while exceeding {maxvalue2}.
Provide a list of species with {PropertyName1} values surpassing {value1}, alongside {PropertyName2} values falling below {minvalue2} and surpassing {maxvalue2}.
Catalog chemical species that demonstrate {PropertyName1} values higher than {value1} and {PropertyName2} values lower than {minvalue2}, yet higher than {maxvalue2}.
List compounds exhibiting {PropertyName1} values above {value1}, with {PropertyName2} values lower than {minvalue2} but higher than {maxvalue2}.
Enumerate molecules showing {PropertyName1} values exceeding {value1}, as well as {PropertyName2} values lower than {minvalue2} while exceeding {maxvalue2}.
Provide a catalog of species featuring {PropertyName1} values higher than {value1}, and {PropertyName2} values falling below {minvalue2} while surpassing {maxvalue2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_H_A(PropertyName1, PropertyName2, value1, value2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value > {value1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1)
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value > {value1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1)
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} higher than {value1} and {PropertyName2} around {value2}
Provide a catalog of chemical species exhibiting {PropertyName1} levels surpassing {value1}, while also possessing {PropertyName2} values approximately near {value2}.
Enumerate molecules showcasing {PropertyName1} values exceeding {value1}, with {PropertyName2} readings approximately at {value2}.
List compounds where {PropertyName1} is greater than {value1} and {PropertyName2} is in the vicinity of {value2}.
Catalog chemical species with {PropertyName1} exceeding {value1} and {PropertyName2} approximately at {value2}.
Enumerate molecules that display {PropertyName1} higher than {value1} and {PropertyName2} around {value2}.
Provide a list of compounds exhibiting {PropertyName1} levels surpassing {value1}, while also having {PropertyName2} values approximately near {value2}.
List chemical species showcasing {PropertyName1} values surpassing {value1}, with {PropertyName2} readings roughly around {value2}.
Catalog molecules where {PropertyName1} is greater than {value1} and {PropertyName2} is approximately {value2}.
Enumerate compounds with {PropertyName1} exceeding {value1} and {PropertyName2} around {value2}.
Provide a list of species with {PropertyName1} higher than {value1} and {PropertyName2} approximately at {value2}.
List chemical entities exhibiting {PropertyName1} levels surpassing {value1}, while also possessing {PropertyName2} values around {value2}.
Enumerate molecules showcasing {PropertyName1} values exceeding {value1}, with {PropertyName2} readings roughly {value2}.
Catalog compounds where {PropertyName1} is greater than {value1} and {PropertyName2} is in the region of {value2}.
List chemical species with {PropertyName1} surpassing {value1} and {PropertyName2} approximately {value2}.
Provide a catalog of molecules displaying {PropertyName1} higher than {value1} and {PropertyName2} around {value2}.
Enumerate compounds exhibiting {PropertyName1} levels above {value1}, while also having {PropertyName2} values approximately at {value2}.
List chemical entities showcasing {PropertyName1} values surpassing {value1} and {PropertyName2} readings approximately around {value2}.
Catalog molecules where {PropertyName1} exceeds {value1} and {PropertyName2} is roughly {value2}.
Provide a list of compounds with {PropertyName1} higher than {value1} and {PropertyName2} approximately around {value2}.
List chemical species exhibiting {PropertyName1} levels surpassing {value1}, while also having {PropertyName2} values around {value2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

####

def get_species_from_two_property_I_I(PropertyName1, PropertyName2, minvalue1, maxvalue1, minvalue2, maxvalue2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value > {minvalue1} && ?{PropertyName1}Value < {maxvalue1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {minvalue2} && ?{PropertyName2}Value < {maxvalue2})
}}"""
  
    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value > {minvalue1} && ?{PropertyName1}Value < {maxvalue1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value > {minvalue2} && ?{PropertyName2}Value < {maxvalue2})
}}"""

    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} between {minvalue1} and {maxvalue1} and {PropertyName2} between {minvalue2} and {maxvalue2}
Provide a roster of chemical entities exhibiting {PropertyName1} within the range of {minvalue1} to {maxvalue1}, and {PropertyName2} spanning from {minvalue2} to {maxvalue2}.
Compile a list of molecules featuring {PropertyName1} that falls between {minvalue1} and {maxvalue1}, along with {PropertyName2} ranging from {minvalue2} to {maxvalue2}.
Enumerate compounds showcasing {PropertyName1} in the interval of {minvalue1} to {maxvalue1}, while also displaying {PropertyName2} between {minvalue2} and {maxvalue2}.
Offer a catalog of chemical species demonstrating {PropertyName1} within {minvalue1} to {maxvalue1}, and {PropertyName2} within {minvalue2} to {maxvalue2}.
Present a compilation of molecules exhibiting {PropertyName1} within {minvalue1} to {maxvalue1}, and {PropertyName2} within {minvalue2} to {maxvalue2}.
List compounds with {PropertyName1} ranging from {minvalue1} to {maxvalue1}, and {PropertyName2} from {minvalue2} to {maxvalue2}.
Generate a roster of chemical entities having {PropertyName1} between {minvalue1} and {maxvalue1}, and {PropertyName2} between {minvalue2} and {maxvalue2}.
Display a list of molecules with {PropertyName1} falling within {minvalue1} and {maxvalue1}, and {PropertyName2} encompassing {minvalue2} and {maxvalue2}.
Catalog chemical compounds that possess {PropertyName1} values within {minvalue1} to {maxvalue1}, in addition to {PropertyName2} values between {minvalue2} and {maxvalue2}.
Compile a record of species where {PropertyName1} is within {minvalue1} to {maxvalue1}, and {PropertyName2} is within {minvalue2} to {maxvalue2}.
Offer a list of molecules characterized by {PropertyName1} within {minvalue1} and {maxvalue1}, and {PropertyName2} within {minvalue2} and {maxvalue2}.
Present a catalog of chemical species showcasing {PropertyName1} values from {minvalue1} to {maxvalue1}, as well as {PropertyName2} values spanning {minvalue2} to {maxvalue2}.
Provide a compilation of compounds with {PropertyName1} ranging between {minvalue1} and {maxvalue1}, along with {PropertyName2} ranging between {minvalue2} and {maxvalue2}.
Enumerate chemical entities featuring {PropertyName1} within {minvalue1} to {maxvalue1}, and {PropertyName2} within {minvalue2} to {maxvalue2}.
List molecules that exhibit {PropertyName1} within {minvalue1} to {maxvalue1}, and {PropertyName2} within {minvalue2} to {maxvalue2}.
Generate a list of compounds with {PropertyName1} values ranging from {minvalue1} to {maxvalue1}, and {PropertyName2} values ranging from {minvalue2} to {maxvalue2}.
Display chemical species that show {PropertyName1} within the bounds of {minvalue1} to {maxvalue1}, and {PropertyName2} within the bounds of {minvalue2} to {maxvalue2}.
Compile a roster of molecules demonstrating {PropertyName1} in the range of {minvalue1} to {maxvalue1}, and {PropertyName2} in the range of {minvalue2} to {maxvalue2}.
Present a list of compounds having {PropertyName1} between {minvalue1} and {maxvalue1}, and {PropertyName2} between {minvalue2} and {maxvalue2}.
Offer an inventory of chemical entities with {PropertyName1} falling between {minvalue1} and {maxvalue1}, and {PropertyName2} falling between {minvalue2} and {maxvalue2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_I_O(PropertyName1, PropertyName2, minvalue1, maxvalue1, minvalue2, maxvalue2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value > {minvalue1} && ?{PropertyName1}Value < {maxvalue1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value < {minvalue2} || ?{PropertyName2}Value > {maxvalue2})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value > {minvalue1} && ?{PropertyName1}Value < {maxvalue1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value < {minvalue2} || ?{PropertyName2}Value > {maxvalue2})
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} between {minvalue1} and {maxvalue1} and {PropertyName2} lower than {minvalue2} and higher than {maxvalue2}
Provide a catalog of chemical species having {PropertyName1} within the range of {minvalue1} to {maxvalue1}, while exhibiting {PropertyName2} below {minvalue2} and above {maxvalue2}.
List molecules showing {PropertyName1} ranging from {minvalue1} to {maxvalue1} and {PropertyName2} falling below {minvalue2} but exceeding {maxvalue2}.
Enumerate compounds that possess {PropertyName1} within {minvalue1} to {maxvalue1} and {PropertyName2} that is both under {minvalue2} and over {maxvalue2}.
Provide an inventory of species exhibiting {PropertyName1} in the interval of {minvalue1} to {maxvalue1}, along with {PropertyName2} that is less than {minvalue2} and greater than {maxvalue2}.
Compile a list of chemical entities with {PropertyName1} in the bracket of {minvalue1} to {maxvalue1}, and {PropertyName2} which is beneath {minvalue2} and above {maxvalue2}.
Generate a roster of molecules featuring {PropertyName1} between {minvalue1} and {maxvalue1}, while maintaining {PropertyName2} below {minvalue2} and above {maxvalue2}.
List compounds having {PropertyName1} that falls in the range of {minvalue1} to {maxvalue1}, coupled with {PropertyName2} lower than {minvalue2} and higher than {maxvalue2}.
Provide an enumeration of chemical species with {PropertyName1} ranging from {minvalue1} to {maxvalue1} and {PropertyName2} less than {minvalue2} while exceeding {maxvalue2}.
Compile a catalog of molecules demonstrating {PropertyName1} within {minvalue1} and {maxvalue1}, and {PropertyName2} that is beneath {minvalue2} yet above {maxvalue2}.
Generate a list of compounds exhibiting {PropertyName1} between {minvalue1} and {maxvalue1}, along with {PropertyName2} lower than {minvalue2} and higher than {maxvalue2}.
List chemical species with {PropertyName1} in the range of {minvalue1} to {maxvalue1}, and {PropertyName2} below {minvalue2} but above {maxvalue2}.
Provide a compilation of molecules showing {PropertyName1} within {minvalue1} to {maxvalue1}, while featuring {PropertyName2} that is less than {minvalue2} and more than {maxvalue2}.
Enumerate compounds that have {PropertyName1} ranging from {minvalue1} to {maxvalue1}, and {PropertyName2} lower than {minvalue2} while exceeding {maxvalue2}.
Generate a list of chemical entities with {PropertyName1} between {minvalue1} and {maxvalue1}, and {PropertyName2} under {minvalue2} and over {maxvalue2}.
List molecules with {PropertyName1} falling in the interval of {minvalue1} to {maxvalue1}, along with {PropertyName2} that is less than {minvalue2} and greater than {maxvalue2}.
Provide an inventory of compounds exhibiting {PropertyName1} within {minvalue1} and {maxvalue1}, while having {PropertyName2} below {minvalue2} and above {maxvalue2}.
Compile a roster of chemical species demonstrating {PropertyName1} in the range of {minvalue1} to {maxvalue1}, and {PropertyName2} lower than {minvalue2} but higher than {maxvalue2}.
Generate a list of molecules featuring {PropertyName1} between {minvalue1} and {maxvalue1}, and {PropertyName2} less than {minvalue2} but exceeding {maxvalue2}.
List compounds having {PropertyName1} within {minvalue1} to {maxvalue1}, coupled with {PropertyName2} that falls below {minvalue2} and rises above {maxvalue2}.
Provide an enumeration of chemical species showing {PropertyName1} ranging from {minvalue1} to {maxvalue1}, and {PropertyName2} under {minvalue2} but above {maxvalue2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_I_A(PropertyName1, PropertyName2, minvalue1, maxvalue1, value2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value > {minvalue1} && ?{PropertyName1}Value < {maxvalue1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1)
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value > {minvalue1} && ?{PropertyName1}Value < {maxvalue1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1)
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} between {minvalue1} and {maxvalue1} and {PropertyName2} around {value2}
Provide a catalog of chemical species having {PropertyName1} within the range of {minvalue1} to {maxvalue1} and {PropertyName2} approximately at {value2}.
List molecules exhibiting {PropertyName1} within {minvalue1} to {maxvalue1} and {PropertyName2} near {value2}.
Enumerate compounds with {PropertyName1} ranging from {minvalue1} to {maxvalue1} and {PropertyName2} centered around {value2}.
Compile a record of chemical entities showing {PropertyName1} in the interval of {minvalue1} to {maxvalue1} and {PropertyName2} close to {value2}.
Present a rundown of species featuring {PropertyName1} within {minvalue1} to {maxvalue1} and {PropertyName2} approximately at {value2}.
Catalog chemical compounds that have {PropertyName1} falling between {minvalue1} and {maxvalue1}, with {PropertyName2} situated around {value2}.
Provide a list of molecules exhibiting {PropertyName1} spanning from {minvalue1} to {maxvalue1} and {PropertyName2} circling {value2}.
List compounds showcasing {PropertyName1} within the scope of {minvalue1} to {maxvalue1} and {PropertyName2} near {value2}.
Enumerate chemical species with {PropertyName1} between {minvalue1} and {maxvalue1} and {PropertyName2} around {value2}.
Compile a record of chemical entities displaying {PropertyName1} within {minvalue1} to {maxvalue1} and {PropertyName2} centered around {value2}.
Present a collection of species featuring {PropertyName1} in the range of {minvalue1} to {maxvalue1} and {PropertyName2} approximately at {value2}.
Catalog chemical species that have {PropertyName1} falling between {minvalue1} and {maxvalue1}, with {PropertyName2} situated around {value2}.
Provide a list of molecules exhibiting {PropertyName1} spanning from {minvalue1} to {maxvalue1} and {PropertyName2} circling {value2}.
List compounds showcasing {PropertyName1} within the scope of {minvalue1} to {maxvalue1} and {PropertyName2} near {value2}.
Enumerate chemical species with {PropertyName1} between {minvalue1} and {maxvalue1} and {PropertyName2} around {value2}.
Compile a record of chemical entities displaying {PropertyName1} within {minvalue1} to {maxvalue1} and {PropertyName2} centered around {value2}.
Present a collection of species featuring {PropertyName1} in the range of {minvalue1} to {maxvalue1} and {PropertyName2} approximately at {value2}.
Catalog chemical species that have {PropertyName1} falling between {minvalue1} and {maxvalue1}, with {PropertyName2} situated around {value2}.
Provide a list of molecules exhibiting {PropertyName1} spanning from {minvalue1} to {maxvalue1} and {PropertyName2} circling {value2}.
List compounds showcasing {PropertyName1} within the scope of {minvalue1} to {maxvalue1} and {PropertyName2} near {value2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_O_O(PropertyName1, PropertyName2, minvalue1, maxvalue1, minvalue2, maxvalue2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value < {minvalue1} || ?{PropertyName1}Value > {maxvalue1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value < {minvalue2} || ?{PropertyName2}Value > {maxvalue2})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value < {minvalue1} || ?{PropertyName1}Value > {maxvalue1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value < {minvalue2} || ?{PropertyName2}Value > {maxvalue2})
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} lower than {minvalue1} and higher than {maxvalue1} and {PropertyName2} lower than {minvalue2} and higher than {maxvalue2} 
Provide a catalog of chemical species having {PropertyName1} below {minvalue1} and above {maxvalue1}, and {PropertyName2} below {minvalue2} and above {maxvalue2}.
List molecules exhibiting {PropertyName1} lesser than {minvalue1} and greater than {maxvalue1}, and {PropertyName2} lesser than {minvalue2} and greater than {maxvalue2}.
Enumerate compounds with {PropertyName1} less than {minvalue1} and greater than {maxvalue1}, and {PropertyName2} less than {minvalue2} and greater than {maxvalue2}.
Compile a record of chemical entities showing {PropertyName1} below {minvalue1} and above {maxvalue1}, and {PropertyName2} below {minvalue2} and above {maxvalue2}.
Generate a list of species where {PropertyName1} is lower than {minvalue1} and higher than {maxvalue1}, and {PropertyName2} is lower than {minvalue2} and higher than {maxvalue2}.
Prepare an inventory of molecules featuring {PropertyName1} beneath {minvalue1} and above {maxvalue1}, and {PropertyName2} beneath {minvalue2} and above {maxvalue2}.
Construct a roster of compounds having {PropertyName1} lower than {minvalue1} and higher than {maxvalue1}, and {PropertyName2} lower than {minvalue2} and higher than {maxvalue2}.
Present a list of chemical species manifesting {PropertyName1} below {minvalue1} and above {maxvalue1}, and {PropertyName2} below {minvalue2} and above {maxvalue2}.
Draft a list of molecules displaying {PropertyName1} under {minvalue1} and over {maxvalue1}, and {PropertyName2} under {minvalue2} and over {maxvalue2}.
Enumerate compounds demonstrating {PropertyName1} less than {minvalue1} and exceeding {maxvalue1}, and {PropertyName2} less than {minvalue2} and exceeding {maxvalue2}.
Provide a compilation of chemical species with {PropertyName1} below {minvalue1} and above {maxvalue1}, and {PropertyName2} below {minvalue2} and above {maxvalue2}.
List molecules with {PropertyName1} lower than {minvalue1} and higher than {maxvalue1}, and {PropertyName2} lower than {minvalue2} and higher than {maxvalue2}.
Display compounds having {PropertyName1} lesser than {minvalue1} and greater than {maxvalue1}, and {PropertyName2} lesser than {minvalue2} and greater than {maxvalue2}.
Share a catalog of chemical entities exhibiting {PropertyName1} below {minvalue1} and above {maxvalue1}, and {PropertyName2} below {minvalue2} and above {maxvalue2}.
Provide a list of species wherein {PropertyName1} is less than {minvalue1} and more than {maxvalue1}, and {PropertyName2} is less than {minvalue2} and more than {maxvalue2}.
Give an overview of molecules indicating {PropertyName1} beneath {minvalue1} and surpassing {maxvalue1}, and {PropertyName2} beneath {minvalue2} and surpassing {maxvalue2}.
Generate a list of compounds displaying {PropertyName1} below {minvalue1} and above {maxvalue1}, and {PropertyName2} below {minvalue2} and above {maxvalue2}.
Prepare a compilation of chemical species featuring {PropertyName1} lower than {minvalue1} and higher than {maxvalue1}, and {PropertyName2} lower than {minvalue2} and higher than {maxvalue2}.
Create a roster of molecules manifesting {PropertyName1} under {minvalue1} and over {maxvalue1}, and {PropertyName2} under {minvalue2} and over {maxvalue2}.
Generate a record of compounds with {PropertyName1} less than {minvalue1} and greater than {maxvalue1}, and {PropertyName2} less than {minvalue2} and greater than {maxvalue2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_O_A(PropertyName1, PropertyName2, minvalue1, maxvalue1, value2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value < {minvalue1} || ?{PropertyName1}Value > {maxvalue1})

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1)
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value < {minvalue1} || ?{PropertyName1}Value > {maxvalue1})
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1)
}}"""

    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} lower than {minvalue1} and higher than {maxvalue1} and {PropertyName2} around {value2}
Provide a roster of chemical species having {PropertyName1} below {minvalue1} and above {maxvalue1}, with {PropertyName2} approximately at {value2}.
Enumerate molecules exhibiting {PropertyName1} lesser than {minvalue1} and greater than {maxvalue1}, alongside {PropertyName2} roughly at {value2}.
List compounds where {PropertyName1} falls below {minvalue1} and above {maxvalue1}, and {PropertyName2} is approximately {value2}.
Offer a compilation of species featuring {PropertyName1} lower than {minvalue1} and higher than {maxvalue1}, while maintaining {PropertyName2} at approximately {value2}.
Present a catalog of chemical entities with {PropertyName1} beneath {minvalue1} and above {maxvalue1}, and {PropertyName2} centered around {value2}.
Enlist molecules demonstrating {PropertyName1} below {minvalue1} and above {maxvalue1}, with {PropertyName2} roughly at {value2}.
Detail compounds exhibiting {PropertyName1} lesser than {minvalue1} and greater than {maxvalue1}, while maintaining {PropertyName2} around {value2}.
Provide a list of species showing {PropertyName1} lower than {minvalue1} and higher than {maxvalue1}, with {PropertyName2} situated near {value2}.
Offer a roster of chemical species with {PropertyName1} below {minvalue1} and above {maxvalue1}, and {PropertyName2} approximately at {value2}.
Enumerate molecules having {PropertyName1} beneath {minvalue1} and above {maxvalue1}, alongside {PropertyName2} approximately at {value2}.
List compounds exhibiting {PropertyName1} lower than {minvalue1} and higher than {maxvalue1}, while {PropertyName2} revolves around {value2}.
Provide a compilation of species demonstrating {PropertyName1} below {minvalue1} and above {maxvalue1}, with {PropertyName2} roughly at {value2}.
Present a catalog of chemical entities featuring {PropertyName1} lesser than {minvalue1} and greater than {maxvalue1}, and {PropertyName2} centered around {value2}.
Enlist molecules showing {PropertyName1} below {minvalue1} and above {maxvalue1}, alongside {PropertyName2} approximately at {value2}.
Detail compounds with {PropertyName1} beneath {minvalue1} and greater than {maxvalue1}, while {PropertyName2} stays close to {value2}.
Provide a list of species indicating {PropertyName1} lower than {minvalue1} and higher than {maxvalue1}, with {PropertyName2} roughly at {value2}.
Offer a roster of chemical species displaying {PropertyName1} below {minvalue1} and above {maxvalue1}, with {PropertyName2} approximately at {value2}.
Enumerate molecules having {PropertyName1} lesser than {minvalue1} and greater than {maxvalue1}, while {PropertyName2} is around {value2}.
List compounds demonstrating {PropertyName1} below {minvalue1} and above {maxvalue1}, and {PropertyName2} approximately {value2}.
Present a compilation of species with {PropertyName1} lower than {minvalue1} and higher than {maxvalue1}, and {PropertyName2} centered around {value2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_two_property_A_A(PropertyName1, PropertyName2, value1, value2):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName1}UnitValue ?{PropertyName1}ReferenceStateValue ?{PropertyName1}ReferenceStateUnitValue ?{PropertyName2}Value ?{PropertyName2}UnitValue ?{PropertyName2}ReferenceStateValue ?{PropertyName2}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName1} ?{PropertyName1}IRI .
    ?{PropertyName1}IRI os:value ?{PropertyName1}Value ; os:unit ?{PropertyName1}UnitIRI ; os:hasProvenance ?{PropertyName1}ProvenanceIRI . 
    ?{PropertyName1}UnitIRI rdfs:label ?{PropertyName1}UnitValue .
    OPTIONAL{{?{PropertyName1}IRI os:hasReferenceState ?{PropertyName1}ReferenceStateIRI .
    ?{PropertyName1}ReferenceStateIRI os:value ?{PropertyName1}ReferenceStateValue ; os:unit ?{PropertyName1}ReferenceStateUnitIRI .
    ?{PropertyName1}ReferenceStateUnitIRI rdfs:label ?{PropertyName1}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName1}Value > {value1}*0.9 && ?{PropertyName1}Value < {value1}*1.1) 

    ?SpeciesIRI os:has{PropertyName2} ?{PropertyName2}IRI .
    ?{PropertyName2}IRI os:value ?{PropertyName2}Value ; os:unit ?{PropertyName2}UnitIRI ; os:hasProvenance ?{PropertyName2}ProvenanceIRI . 
    ?{PropertyName2}UnitIRI rdfs:label ?{PropertyName2}UnitValue .
    OPTIONAL{{?{PropertyName2}IRI os:hasReferenceState ?{PropertyName2}ReferenceStateIRI .
    ?{PropertyName2}ReferenceStateIRI os:value ?{PropertyName2}ReferenceStateValue ; os:unit ?{PropertyName2}ReferenceStateUnitIRI .
    ?{PropertyName2}ReferenceStateUnitIRI rdfs:label ?{PropertyName2}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1) 
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName1}Value ?{PropertyName2}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasProperty{PropertyName1} ?{PropertyName1}Value .
    FILTER(?{PropertyName1}Value > {value1}*0.9 && ?{PropertyName1}Value < {value1}*1.1) 
    ?SpeciesIRI os:hasProperty{PropertyName2} ?{PropertyName2}Value .
    FILTER(?{PropertyName2}Value > {value2}*0.9 && ?{PropertyName2}Value < {value2}*1.1) 
}}"""
  
    PropertyName1 = add_space_and_lower(PropertyName1)
    PropertyName2 = add_space_and_lower(PropertyName2)

    question_text = f"""Give me a list of species with {PropertyName1} around {value1} and {PropertyName2} around {value2}
Provide a catalog of chemical species having {PropertyName1} approximately at {value1} and {PropertyName2} approximately at {value2}.
List molecules exhibiting {PropertyName1} around {value1} and {PropertyName2} around {value2}.
Enumerate compounds with {PropertyName1} about {value1} and {PropertyName2} about {value2}.
Furnish a record of species demonstrating {PropertyName1} near {value1} and {PropertyName2} near {value2}.
Offer a compilation of chemical entities displaying {PropertyName1} around {value1} and {PropertyName2} around {value2}.
Jot down a series of molecules manifesting {PropertyName1} approximately {value1} and {PropertyName2} approximately {value2}.
Present a lineup of compounds featuring {PropertyName1} at about {value1} and {PropertyName2} at about {value2}.
Display a list of species with {PropertyName1} roughly {value1} and {PropertyName2} roughly {value2}.
Provide an inventory of chemical species having {PropertyName1} in the vicinity of {value1} and {PropertyName2} in the vicinity of {value2}.
Enumerate molecules exhibiting {PropertyName1} around {value1} and {PropertyName2} around {value2}.
Offer a compilation of compounds with {PropertyName1} approximately {value1} and {PropertyName2} approximately {value2}.
Furnish a record of species demonstrating {PropertyName1} close to {value1} and {PropertyName2} close to {value2}.
Provide a catalog of chemical entities displaying {PropertyName1} at around {value1} and {PropertyName2} at around {value2}.
Jot down a series of molecules manifesting {PropertyName1} near {value1} and {PropertyName2} near {value2}.
Present a lineup of compounds featuring {PropertyName1} around {value1} and {PropertyName2} around {value2}.
Display a list of species with {PropertyName1} about {value1} and {PropertyName2} about {value2}.
Provide an inventory of chemical species having {PropertyName1} approximately {value1} and {PropertyName2} approximately {value2}.
Enumerate molecules exhibiting {PropertyName1} roughly {value1} and {PropertyName2} roughly {value2}.
Offer a compilation of compounds with {PropertyName1} in the vicinity of {value1} and {PropertyName2} in the vicinity of {value2}.
Furnish a record of species demonstrating {PropertyName1} at about {value1} and {PropertyName2} at about {value2}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_chclass_and_use(ChemClass, Use):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue 
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .

    FILTER( ?ChemicalClassValue = "{ChemClass}")

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI os:value ?UseValue .

    FILTER( ?UseValue = "{Use}")
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
    FILTER( ?ChemicalClassValue = "{ChemClass}")
    ?SpeciesIRI os:hasUse ?UseValue .
    FILTER( ?UseValue = "{Use}")
}}"""

    question_text = f"""Give me a list of species that are classified as {ChemClass} and used as {Use}
Provide a roster of chemical species classified as {ChemClass} and employed as {Use}.
List the molecules categorized as {ChemClass} and utilized as {Use}.
Enumerate the compounds falling under the {ChemClass} designation, which find use as {Use}.
Can you give me a record of chemical species characterized as {ChemClass} and employed for {Use} purposes?
I'm interested in knowing the {ChemClass} species that double as {Use}; could you compile a list?
Please outline a compilation of {ChemClass} species that meet the criteria of being {Use}.
I'd like information on {ChemClass} species functioning as {Use}; could you furnish me with a list?
Could you provide a rundown of {ChemClass} entities that fall within the category of {Use}?
I'm looking for a catalog of {ChemClass} molecules known for their use as {Use}; can you assist?
In need of a list of {ChemClass} compounds, specifically those put to use as {Use}.
Please compose a list of {ChemClass} chemical species acknowledged as {Use}.
Seeking a compilation of {ChemClass} species recognized both as {Use}; can you compile this?
Can you outline {ChemClass} entities that are formally classified and have applications as {Use}?
I'm curious about {ChemClass} species functioning in the capacity of {Use}; provide me with a list.
I require a catalog of {ChemClass} compounds that fit the description of {Use}.
Could you present a roster of {ChemClass} molecules typically utilized as {Use}?
Please draft a list of {ChemClass} chemical species that fulfill the role of {Use}.
I'm inquiring about {ChemClass} entities recognized for their use as {Use}; can you compile a list?
Provide me with an inventory of {ChemClass} compounds known to possess {Use} qualities.
Can you enlist {ChemClass} species acknowledged for their role as {Use}?"""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_L_and_chclass(PropertyName, ChemClass, value):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .

    FILTER( ?ChemicalClassValue = "{ChemClass}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value < {value})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
    FILTER( ?ChemicalClassValue = "{ChemClass}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value < {value})
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} lower than {value} and are classified as {ChemClass}
Provide a compilation of chemical species with a {PropertyName} beneath {value}, categorized as {ChemClass}.
List molecules having {PropertyName} less than {value} and falling under the classification of {ChemClass}.
Enumerate compounds that exhibit a {PropertyName} below {value} and are designated as {ChemClass}.
Give me a roster of chemical entities with {PropertyName} lower than {value}, belonging to the {ChemClass} category.
Offer a record of species displaying a {PropertyName} beneath {value} and being characterized as {ChemClass}.
Compile a list of molecules having {PropertyName} less than {value} and falling into the {ChemClass} grouping.
Provide a catalogue of compounds exhibiting a {PropertyName} below {value} and classified as {ChemClass}.
List chemical species with {PropertyName} lower than {value} and falling under the {ChemClass} classification.
Enumerate molecules that display a {PropertyName} beneath {value} and are categorized as {ChemClass}.
Give me a rundown of compounds showing {PropertyName} less than {value} and being assigned to {ChemClass}.
Offer a collection of chemical entities with {PropertyName} lower than {value} and falling within the {ChemClass} category.
Compile a registry of molecules exhibiting {PropertyName} below {value} and classified under {ChemClass}.
Provide a comprehensive list of compounds having {PropertyName} less than {value} and falling into the {ChemClass} class.
List chemical species demonstrating a {PropertyName} beneath {value} and being categorized as {ChemClass}.
Enumerate molecules exhibiting {PropertyName} lower than {value} and being designated as {ChemClass}.
Give me a record of compounds with {PropertyName} below {value} and falling under the classification of {ChemClass}.
Offer a rundown of chemical entities that have {PropertyName} less than {value} and are categorized within {ChemClass}.
Provide a catalog of molecules displaying {PropertyName} beneath {value} and classified as {ChemClass}.
List compounds showing {PropertyName} lower than {value} and belonging to the {ChemClass} category.
Compile a list of chemical species with {PropertyName} below {value} and falling into the {ChemClass} classification."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_H_and_chclass(PropertyName, ChemClass, value):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue .

    FILTER( ?ChemicalClassValue = "{ChemClass}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value > {value})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
    FILTER( ?ChemicalClassValue = "{ChemClass}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value > {value})
}}"""


    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} higher than {value} and are classified as {ChemClass}
Provide a roster of chemical species possessing a {PropertyName} greater than {value} and falling under the category of {ChemClass}.
Enumerate molecules with {PropertyName} exceeding {value} and falling into the {ChemClass} classification.
List compounds that exhibit {PropertyName} surpassing {value} and are categorized as {ChemClass}.
Catalog chemical species with {PropertyName} higher than {value} and falling under the {ChemClass} category.
Outline a record of molecules displaying {PropertyName} above {value} and being classified as {ChemClass}.
Compile a tally of compounds demonstrating {PropertyName} surpassing {value} and falling into the {ChemClass} grouping.
Detail chemical species that showcase {PropertyName} greater than {value} and belong to the {ChemClass} classification.
Present a rundown of molecules having {PropertyName} exceeding {value} and falling within the {ChemClass} class.
Tabulate compounds with {PropertyName} surpassing {value} and being categorized as {ChemClass}.
Provide a list of chemical species exhibiting {PropertyName} higher than {value} and falling into the {ChemClass} category.
Furnish information on molecules that display {PropertyName} above {value} and are classified under {ChemClass}.
Offer a compilation of compounds demonstrating {PropertyName} surpassing {value} and belonging to the {ChemClass} classification.
Share a list of chemical species that have {PropertyName} greater than {value} and are grouped as {ChemClass}.
Give details about molecules exhibiting {PropertyName} exceeding {value} and being categorized as {ChemClass}.
Provide a roster of compounds with {PropertyName} surpassing {value} and falling into the {ChemClass} class.
Enumerate chemical species showcasing {PropertyName} higher than {value} and falling within the {ChemClass} grouping.
List molecules that demonstrate {PropertyName} above {value} and belong to the {ChemClass} category.
Catalog compounds displaying {PropertyName} surpassing {value} and classified as {ChemClass}.
Outline chemical species with {PropertyName} greater than {value} and categorized under {ChemClass}.
Compile a record of molecules displaying {PropertyName} exceeding {value} and falling within the {ChemClass} classification."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_O_and_chclass(PropertyName, ChemClass, minvalue, maxvalue):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .

    FILTER( ?ChemicalClassValue = "{ChemClass}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value < {minvalue} || ?{PropertyName}Value > {maxvalue})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
    FILTER( ?ChemicalClassValue = "{ChemClass}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value < {minvalue} || ?{PropertyName}Value > {maxvalue})
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} lower than {minvalue} and higher than {maxvalue} and are classified as {ChemClass}
Provide a catalog of chemical species with {PropertyName} ranging below {minvalue} and above {maxvalue}, falling under the category of {ChemClass}.
List molecules characterized by {PropertyName} levels below {minvalue} and above {maxvalue} and grouped as {ChemClass}.
Enumerate compounds exhibiting {PropertyName} values less than {minvalue} and greater than {maxvalue}, and belonging to the {ChemClass} classification.
Compile a record of species wherein {PropertyName} is below {minvalue} and above {maxvalue}, and these entities are categorized as {ChemClass}.
Tabulate chemical entities showing {PropertyName} beneath {minvalue} and above {maxvalue}, while being classified under {ChemClass}.
Offer a roster of chemical species having {PropertyName} lower than {minvalue} and higher than {maxvalue}, and falling into the {ChemClass} category.
Present a list of molecules exhibiting {PropertyName} values below {minvalue} and above {maxvalue}, with a classification of {ChemClass}.
Display a compilation of compounds with {PropertyName} less than {minvalue} and greater than {maxvalue}, while being categorized as {ChemClass}.
Outline a collection of species showing {PropertyName} levels lower than {minvalue} and higher than {maxvalue}, and belonging to the {ChemClass} grouping.
Provide an inventory of chemical entities characterized by {PropertyName} values beneath {minvalue} and above {maxvalue}, falling under the {ChemClass} classification.
Share a list of chemical species that exhibit {PropertyName} below {minvalue} and above {maxvalue}, and are classified within the {ChemClass} category.
Create a roster of molecules having {PropertyName} lower than {minvalue} and higher than {maxvalue}, while being categorized as {ChemClass}.
List compounds with {PropertyName} levels below {minvalue} and above {maxvalue}, and are grouped under the {ChemClass} classification.
Provide a compilation of species exhibiting {PropertyName} values less than {minvalue} and greater than {maxvalue}, and falling into the {ChemClass} category.
Offer a catalog of chemical entities with {PropertyName} beneath {minvalue} and above {maxvalue}, and classified as {ChemClass}.
Present a record of molecules showing {PropertyName} below {minvalue} and above {maxvalue}, categorized under {ChemClass}.
Enumerate compounds with {PropertyName} lower than {minvalue} and higher than {maxvalue}, and falling into the {ChemClass} category.
Compile a list of species that have {PropertyName} values below {minvalue} and above {maxvalue}, and are classified as {ChemClass}.
Share a collection of chemical entities with {PropertyName} beneath {minvalue} and above {maxvalue}, falling under the {ChemClass} classification.
Provide a comprehensive list of molecules exhibiting {PropertyName} lower than {minvalue} and higher than {maxvalue}, and being part of the {ChemClass} group."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_I_and_chclass(PropertyName, ChemClass, minvalue, maxvalue):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .

    FILTER( ?ChemicalClassValue = "{ChemClass}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value > {minvalue} && ?{PropertyName}Value < {maxvalue})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
    FILTER( ?ChemicalClassValue = "{ChemClass}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value > {minvalue} && ?{PropertyName}Value < {maxvalue})
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} between {minvalue} and {maxvalue} and are classified as {ChemClass}
Provide a roster of chemical species exhibiting {PropertyName} within the range of {minvalue} to {maxvalue} and falling under the category of {ChemClass}.
List molecules with {PropertyName} ranging from {minvalue} to {maxvalue} and belonging to the {ChemClass} classification.
Enumerate compounds that demonstrate {PropertyName} within {minvalue} and {maxvalue} and are categorized as {ChemClass}.
Catalog chemical species manifesting {PropertyName} in the interval of {minvalue} through {maxvalue} and having the classification of {ChemClass}.
Compile a record of species showing {PropertyName} within {minvalue} to {maxvalue} and falling under the {ChemClass} category.
Generate a list of molecules displaying {PropertyName} between {minvalue} and {maxvalue}, while being classified as {ChemClass}.
Display compounds that possess {PropertyName} in the range of {minvalue} to {maxvalue} and are categorized under {ChemClass}.
Provide an inventory of chemical species indicating {PropertyName} within {minvalue} - {maxvalue} and falling into the {ChemClass} grouping.
Offer a list of species showcasing {PropertyName} between {minvalue} and {maxvalue}, while being assigned the {ChemClass} classification.
Present a compilation of molecules demonstrating {PropertyName} within the bracket of {minvalue} to {maxvalue} and sorted as {ChemClass}.
Share a roster of compounds exhibiting {PropertyName} from {minvalue} up to {maxvalue}, and falling into the {ChemClass} category.
Furnish a list of chemical species displaying {PropertyName} within the range of {minvalue} - {maxvalue}, and falling under {ChemClass}.
Provide a catalog of species showcasing {PropertyName} between {minvalue} and {maxvalue}, and classified as {ChemClass}.
List molecules that possess {PropertyName} ranging from {minvalue} to {maxvalue}, and are categorized under {ChemClass}.
Enumerate compounds demonstrating {PropertyName} within {minvalue} to {maxvalue}, and falling into the {ChemClass} classification.
Catalog chemical species indicating {PropertyName} in the interval from {minvalue} to {maxvalue}, while being assigned the {ChemClass} category.
Compile a record of species showing {PropertyName} between {minvalue} and {maxvalue}, with classification as {ChemClass}.
Generate a list of molecules displaying {PropertyName} within the range of {minvalue} through {maxvalue}, and falling under the {ChemClass} grouping.
Display compounds that exhibit {PropertyName} in the bracket of {minvalue} to {maxvalue}, and are classified as {ChemClass}.
Provide an inventory of chemical species indicating {PropertyName} between {minvalue} and {maxvalue}, categorized as {ChemClass}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_A_and_chclass(PropertyName, ChemClass, value):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasChemicalClass* ?x .
	?x ?y ?z .
	?z rdfs:subClassOf* ?ChemicalClassIRI .
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .

    FILTER( ?ChemicalClassValue = "{ChemClass}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value > {value}*0.9 && ?{PropertyName}Value < {value}*1.1)
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
    FILTER( ?ChemicalClassValue = "{ChemClass}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value > {value}*0.9 && ?{PropertyName}Value < {value}*1.1)
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} around {value} and are classified as {ChemClass}
Provide a roster of chemical species displaying {PropertyName} approximately at {value} and falling under the category of {ChemClass}.
Enumerate molecules that exhibit {PropertyName} at roughly {value} and are categorized as {ChemClass}.
List compounds with {PropertyName} circling {value} and falling into the {ChemClass} classification.
Give me an inventory of species manifesting {PropertyName} around {value} while being classified as {ChemClass}.
Compile a record of chemical entities featuring {PropertyName} at about {value} and falling within {ChemClass}.
Jot down a catalog of molecules demonstrating {PropertyName} approximately at {value} and falling into the {ChemClass} category.
Note down compounds exhibiting {PropertyName} around {value} and classified as {ChemClass}.
Provide a rundown of chemical species indicating {PropertyName} at roughly {value} and belonging to the {ChemClass} grouping.
Enumerate molecules that display {PropertyName} close to {value} and are categorized as {ChemClass}.
List compounds with {PropertyName} near {value} and falling under the {ChemClass} classification.
Give me an inventory of species showing {PropertyName} in the vicinity of {value} and falling into the {ChemClass} category.
Compile a record of chemical entities with {PropertyName} situated at approximately {value} and belonging to {ChemClass}.
Jot down a catalog of molecules with {PropertyName} positioned around {value} and classified as {ChemClass}.
Note down compounds demonstrating {PropertyName} at about {value} and categorized as {ChemClass}.
Provide a rundown of chemical species having {PropertyName} close to {value} and falling into the {ChemClass} grouping.
Enumerate molecules that indicate {PropertyName} near {value} and are classified as {ChemClass}.
List compounds displaying {PropertyName} approximately at {value} and falling within the {ChemClass} category.
Give me an inventory of species revealing {PropertyName} around {value} while being categorized as {ChemClass}.
Compile a record of chemical entities exhibiting {PropertyName} at roughly {value} and belonging to the {ChemClass} class.
Jot down a catalog of molecules showcasing {PropertyName} circling {value} and classified under {ChemClass}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_L_and_use(PropertyName, Use, value):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI os:value ?UseValue .

    FILTER( ?UseValue = "{Use}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value < {value})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasUse ?UseValue .
    FILTER( ?UseValue = "{Use}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value < {value})
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} lower than {value} and used as {Use}
Provide a catalog of chemical species with {PropertyName} below {value}, utilized as {Use}.
List molecules having {PropertyName} less than {value} and employed for {Use}.
Enumerate compounds with {PropertyName} lower than {value} and applied in {Use}.
Present a compilation of chemical species exhibiting {PropertyName} beneath {value} and serving the purpose of {Use}.
Outline various molecules that demonstrate {PropertyName} below {value} and find application as {Use}.
Detail a roster of compounds showcasing {PropertyName} lesser than {value} and utilized for {Use}.
Offer an inventory of chemical species manifesting {PropertyName} under {value} and used for {Use}.
Enlist molecules characterized by {PropertyName} not exceeding {value} and employed in {Use}.
Provide a rundown of compounds displaying {PropertyName} lower than {value} and finding use as {Use}.
List chemical species with {PropertyName} beneath {value}, known for their role in {Use}.
Enumerate molecules that possess {PropertyName} less than {value} and find their application as {Use}.
Present a catalog of compounds having {PropertyName} below {value} and utilized for {Use}.
Outline chemical species exhibiting {PropertyName} lower than {value}, often used in {Use}.
Detail molecules showcasing {PropertyName} beneath {value} and finding utility as {Use}.
Offer a comprehensive list of compounds demonstrating {PropertyName} not exceeding {value} and applied for {Use}.
Provide an extensive roster of chemical species with {PropertyName} lesser than {value} and known for {Use}.
List molecules that display {PropertyName} under {value} and are recognized for their {Use}.
Enumerate compounds exhibiting {PropertyName} below {value} and playing a role in {Use}.
Present a varied selection of chemical species with {PropertyName} less than {value}, commonly utilized as {Use}.
Outline the molecules characterized by {PropertyName} lower than {value} and employed for {Use}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_H_and_use(PropertyName, Use, value):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI os:value ?UseValue .

    FILTER( ?UseValue = "{Use}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value > {value})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value 
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasUse ?UseValue .
    FILTER( ?UseValue = "{Use}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value > {value})
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} higher than {value} and used as {Use}
Provide a roster of chemical species with {PropertyName} greater than {value} that find application as {Use}.
List molecules having a {PropertyName} exceeding {value} and utilized for {Use}.
Enumerate compounds with {PropertyName} surpassing {value} and employed in the capacity of {Use}.
Furnish a catalog of species where {PropertyName} is above {value} and known for their {Use}.
Present a compilation of chemical entities exhibiting {PropertyName} higher than {value} and put to use as {Use}.
Detail a record of molecules demonstrating {PropertyName} beyond {value} and having purposes in {Use}.
Jot down a register of compounds displaying {PropertyName} surpassing {value} and found valuable for {Use}.
Offer an inventory of species with {PropertyName} that's elevated compared to {value}, recognized for their {Use}.
Display a list of chemical species manifesting {PropertyName} above {value} and employed for {Use}.
Outline molecules exhibiting {PropertyName} greater than {value} and serving as {Use}.
Compile a list of compounds with {PropertyName} exceeding {value} and applicable in {Use}.
Provide a rundown of species where {PropertyName} is higher than {value} and utilized for {Use}.
Offer a catalog of chemical entities showcasing {PropertyName} surpassing {value} and put to practical {Use}.
Enumerate molecules with {PropertyName} surpassing {value} and known for their {Use}.
Present a comprehensive list of compounds displaying {PropertyName} above {value} and finding utility in {Use}.
Give a breakdown of species possessing {PropertyName} greater than {value} and utilized as {Use}.
Share a collection of chemical species exemplifying {PropertyName} higher than {value} and adopted for {Use}.
Provide an organized list of molecules with {PropertyName} exceeding {value} and utilized in {Use}.
Present an itemized list of compounds showcasing {PropertyName} surpassing {value} and employed for {Use}.
Share a compilation of species having {PropertyName} higher than {value} and recognized for their {Use}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_O_and_use(PropertyName, Use, minvalue, maxvalue):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI os:value ?UseValue .

    FILTER( ?UseValue = "{Use}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value < {minvalue} || ?{PropertyName}Value > {maxvalue})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasUse ?UseValue .
    FILTER( ?UseValue = "{Use}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value < {minvalue} || ?{PropertyName}Value > {maxvalue})
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} lower than {minvalue} and higher than {maxvalue} and used as {Use}
Provide a roster of chemical species exhibiting {PropertyName} values below {minvalue} and above {maxvalue}, employed in the capacity of {Use}.
Enumerate molecules with {PropertyName} lesser than {minvalue} and greater than {maxvalue}, applied in the role of {Use}.
List compounds having {PropertyName} below {minvalue} and above {maxvalue}, utilized for {Use}.
Catalog chemical entities whose {PropertyName} falls beneath {minvalue} and rises above {maxvalue}, serving the purpose of {Use}.
Tabulate species manifesting {PropertyName} beneath {minvalue} and beyond {maxvalue}, put to use as {Use}.
Detail molecular structures with {PropertyName} lower than {minvalue} and higher than {maxvalue}, deployed for {Use}.
Present a register of compounds demonstrating {PropertyName} values lower than {minvalue} and higher than {maxvalue}, employed in the context of {Use}.
Outline chemical compositions exhibiting {PropertyName} below {minvalue} and above {maxvalue}, utilized as {Use}.
Chronicle molecular entities showcasing {PropertyName} lesser than {minvalue} and greater than {maxvalue}, applied for {Use}.
Compile species with {PropertyName} values beneath {minvalue} and exceeding {maxvalue}, functioning as {Use}.
Jot down compounds that display {PropertyName} below {minvalue} and surpass {maxvalue}, adopted for {Use}.
Create a list of chemical species manifesting {PropertyName} lower than {minvalue} and higher than {maxvalue}, used in the role of {Use}.
Present molecules with {PropertyName} lesser than {minvalue} and greater than {maxvalue}, implemented as {Use}.
Offer a compilation of compounds having {PropertyName} below {minvalue} and above {maxvalue}, employed for {Use}.
Provide a record of chemical entities whose {PropertyName} falls short of {minvalue} and exceeds {maxvalue}, put to use in {Use}.
Enumerate species displaying {PropertyName} values beneath {minvalue} and beyond {maxvalue}, utilized for {Use}.
List molecular structures exhibiting {PropertyName} lower than {minvalue} and higher than {maxvalue}, deployed as {Use}.
Catalog compounds exhibiting {PropertyName} below {minvalue} and above {maxvalue}, serving the purpose of {Use}.
Tabulate chemical compositions with {PropertyName} lesser than {minvalue} and greater than {maxvalue}, applied in the context of {Use}.
Detail species showcasing {PropertyName} values lower than {minvalue} and higher than {maxvalue}, utilized as {Use}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_I_and_use(PropertyName, Use, minvalue, maxvalue):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI os:value ?UseValue .

    FILTER( ?UseValue = "{Use}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value > {minvalue} && ?{PropertyName}Value < {maxvalue})
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasUse ?UseValue .
    FILTER( ?UseValue = "{Use}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value > {minvalue} && ?{PropertyName}Value < {maxvalue})
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} between {minvalue} and {maxvalue} and used as {Use}
Provide a roster of chemical species exhibiting {PropertyName} within the range of {minvalue} to {maxvalue} and employed for {Use}.
Enumerate the molecules manifesting {PropertyName} within {minvalue} and {maxvalue} and utilized for {Use}.
List the compounds displaying {PropertyName} within the specified limits of {minvalue} through {maxvalue}, employed in the capacity of {Use}.
Catalog the chemical entities demonstrating {PropertyName} falling within {minvalue} to {maxvalue} and put to use as {Use}.
Enlist the species characterized by {PropertyName} within the range of {minvalue} to {maxvalue}, having applications as {Use}.
Detail a compilation of molecules showcasing {PropertyName} in the range from {minvalue} to {maxvalue} and fulfilling the role of {Use}.
Outline a record of compounds featuring {PropertyName} within {minvalue} - {maxvalue} range, utilized for {Use}.
Jot down a summary of chemical species expressing {PropertyName} within {minvalue} to {maxvalue} and adopted for {Use}.
Tabulate the molecules displaying {PropertyName} within the prescribed {minvalue} and {maxvalue} range, implemented for {Use}.
Present a list of compounds with {PropertyName} ranging from {minvalue} to {maxvalue} and serving the purpose of {Use}.
Provide an inventory of chemical species exhibiting {PropertyName} within {minvalue} through {maxvalue} and employed for {Use}.
Offer a compilation of molecules showcasing {PropertyName} between {minvalue} and {maxvalue}, with utility as {Use}.
Share a roster of compounds featuring {PropertyName} in the {minvalue} - {maxvalue} bracket, utilized in the role of {Use}.
Display a catalog of chemical entities demonstrating {PropertyName} within {minvalue} to {maxvalue} range and put into action for {Use}.
Present a list of species characterized by {PropertyName} within the range of {minvalue} to {maxvalue}, having applications as {Use}.
Provide a summary of molecules displaying {PropertyName} within {minvalue} and {maxvalue}, applied as {Use}.
Share an overview of compounds with {PropertyName} falling between {minvalue} and {maxvalue}, utilized for {Use}.
List down the chemical species expressing {PropertyName} within the {minvalue} - {maxvalue} range and used as {Use}.
Offer a catalog of molecules demonstrating {PropertyName} within {minvalue} to {maxvalue}, employed for {Use}.
Provide an index of compounds showcasing {PropertyName} within the limits of {minvalue} to {maxvalue}, with practical use as {Use}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###

def get_species_from_property_A_and_use(PropertyName, Use, value):
    query_text = f"""PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI os:value ?UseValue .

    FILTER( ?UseValue = "{Use}")

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value > {value}*0.9 && ?{PropertyName}Value < {value}*1.1)
}}"""

    query_text_compact = f"""SELECT DISTINCT ?IUPACNameValue ?{PropertyName}Value
WHERE {{
    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue .
    ?SpeciesIRI os:hasUse ?UseValue .
    FILTER( ?UseValue = "{Use}")
    ?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
    FILTER(?{PropertyName}Value > {value}*0.9 && ?{PropertyName}Value < {value}*1.1)
}}"""

    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species that have {PropertyName} around {value} and used as {Use}
Provide a roster of chemical species exhibiting {PropertyName} approximately at {value} and employed for {Use}.
Enumerate molecules with {PropertyName} near {value} utilized in the capacity of {Use}.
List compounds manifesting {PropertyName} around {value} and put to use as {Use}.
Offer a catalog of species demonstrating {PropertyName} at roughly {value} and applied for {Use}.
Outline chemical species displaying {PropertyName} approximately {value} and harnessed for {Use}.
Compile a record of molecules showcasing {PropertyName} close to {value} and employed for {Use}.
Present a lineup of compounds featuring {PropertyName} around {value} and utilized as {Use}.
Detail species exhibiting {PropertyName} at about {value} and adopted for {Use}.
Tabulate chemical species with {PropertyName} near {value} and put to service as {Use}.
Provide an inventory of molecules having {PropertyName} approximately {value} and used for {Use}.
Enlist compounds demonstrating {PropertyName} around {value} and employed in {Use}.
Give a rundown of species displaying {PropertyName} at roughly {value} and utilized for {Use}.
Share a list of chemical species manifesting {PropertyName} near {value} and applied as {Use}.
Enumerate molecules showcasing {PropertyName} approximately at {value} and harnessed for {Use}.
Compile a catalogue of compounds with {PropertyName} around {value} and put to use as {Use}.
Provide a summary of species exhibiting {PropertyName} close to {value} and employed for {Use}.
Detail chemical species displaying {PropertyName} approximately {value} and utilized in {Use}.
Tabulate molecules featuring {PropertyName} at about {value} and used as {Use}.
Offer an index of compounds demonstrating {PropertyName} near {value} and put to service for {Use}.
Present a collection of species with {PropertyName} around {value} and applied for {Use}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return dict(
        question=prompt,
        sparql_query=query_text,
        sparql_query_compact=query_text_compact
    )

###
