import random
from generate_data.template_utils import add_space_and_lower

### 

def get_species_from_property_L(PropertyName, value):
    query_text = f"""
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value < {value})
}}
    """
  
    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Provide a catalog of compounds exhibiting {PropertyName} below {value}.
Enumerate molecules wherein the {PropertyName} is less than {value}.
Offer a compilation of species with {PropertyName} that falls under {value}.
Present a roster of substances having a {PropertyName} lower than {value}.
List molecules with {PropertyName} beneath the threshold of {value}.
Outline a collection of compounds demonstrating {PropertyName} below {value}.
Identify species with {PropertyName} that is lesser than {value}
Detail a register of substances displaying a {PropertyName} lower than {value}.
Tabulate molecules exhibiting {PropertyName} below the specified {value}.
Enlist species with {PropertyName} falling within the range of less than {value}
Catalogue compounds where the {PropertyName} is lower than {value}.
Compile a list of molecules with a {PropertyName} that is below {value}.
Provide an inventory of species showing {PropertyName} below the given {value}
Offer a summary of substances with {PropertyName} lesser than {value}.
Present a directory of compounds demonstrating {PropertyName} beneath {value}.
Identify species with a {PropertyName} that is lower than {value}.
Detail a compilation of substances exhibiting a {PropertyName} lower than {value}.
Tabulate molecules with {PropertyName} below the designated {value}.
Enlist species displaying {PropertyName} falling under the stipulated {value}.
Catalog compounds wherein the {PropertyName} is lower than the specified {value}
Give me a list of molecules with {PropertyName} lower than {value}   """

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return query_text, prompt

###

def get_species_from_property_H(PropertyName, value):
    query_text = f"""
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value > {value})
}}
    """
  
    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Provide a catalog of compounds exhibiting {PropertyName} exceeding {value}.
Enumerate the molecules in which the {PropertyName} surpasses {value}.
List the species possessing a {PropertyName} value greater than {value}.
Share a compilation of substances with {PropertyName} levels above {value}
Outline the molecules that demonstrate {PropertyName} values higher than {value}.
Detail the compounds with {PropertyName} exceeding the threshold of {value}.
Present a roster of species with {PropertyName} that is greater than {value}.
Specify the substances exhibiting {PropertyName} values surpassing {value}
Illustrate the molecular entities that possess {PropertyName} levels higher than {value}.
Catalog the compounds in which {PropertyName} exceeds {value}.
Identify the molecules with {PropertyName} values higher than {value}.
Outline the species showcasing {PropertyName} levels above {value}.
Enumerate the compounds demonstrating {PropertyName} surpassing {value}
List the molecular entities that have {PropertyName} greater than {value}.
Share a compilation of substances with {PropertyName} levels exceeding {value}
Provide a roster of species in which {PropertyName} is higher than {value}.
Specify the substances that exhibit {PropertyName} values surpassing {value}.
Illustrate the molecular compounds that possess {PropertyName} levels higher than {value}.
Catalog the entities with {PropertyName} exceeding {value}.
Identify the species with {PropertyName} values higher than {value}. """

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return query_text, prompt

###

def get_species_from_property_O(PropertyName, minvalue, maxvalue):
    query_text = f"""
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value < {minvalue} || ?{PropertyName}Value > {maxvalue})
}}
    """
  
    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Provide a compilation of chemical compounds exhibiting a {PropertyName} that falls below {minvalue} and surpasses {maxvalue}.
Enumerate a series of molecules wherein the {PropertyName} is lesser than {minvalue} and greater than {maxvalue}.
Offer a catalog of molecular species with a {PropertyName} value that is below {minvalue} and above {maxvalue}.
Present a roster of chemical substances showcasing a {PropertyName} that is lower than {minvalue} and higher than {maxvalue}.
List various compounds characterized by a {PropertyName} below {minvalue} and above {maxvalue}.
Outline a selection of molecules displaying a {PropertyName} below {minvalue} and exceeding {maxvalue}.
Compile a record of molecular species with a {PropertyName} value that falls below {minvalue} and surpasses {maxvalue}.
Provide a rundown of chemical compounds exhibiting a {PropertyName} lower than {minvalue} and higher than {maxvalue}.
Offer a comprehensive list of molecules demonstrating a {PropertyName} that is beneath {minvalue} and above {maxvalue}.
Enumerate several molecular species with a {PropertyName} value below {minvalue} and beyond {maxvalue}.
Present a catalogue of chemical substances characterized by a {PropertyName} that is lower than {minvalue} and greater than {maxvalue}.
List various compounds showcasing a {PropertyName} below {minvalue} and surpassing {maxvalue}.
Outline a collection of molecules displaying a {PropertyName} below {minvalue} and higher than {maxvalue}.
Compile a roster of molecular species with a {PropertyName} value that falls below {minvalue} and exceeds {maxvalue}.
Provide a summary of chemical compounds exhibiting a {PropertyName} lower than {minvalue} and surpassing {maxvalue}.
Offer an inventory of molecules demonstrating a {PropertyName} that is beneath {minvalue} and above {maxvalue}.
Enumerate multiple molecular species with a {PropertyName} value below {minvalue} and beyond {maxvalue}.
Present a compendium of chemical substances characterized by a {PropertyName} that is lower than {minvalue} and greater than {maxvalue}.
List various compounds showcasing a {PropertyName} below {minvalue} and exceeding {maxvalue}.
Outline a variety of molecules displaying a {PropertyName} below {minvalue} and surpassing {maxvalue}.
Species with {PropertyName} lower than {minvalue} and higher than {maxvalue} """

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return query_text, prompt

###

def get_species_from_property_I(PropertyName, minvalue, maxvalue):
    query_text = f"""
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value > {minvalue} && ?{PropertyName}Value < {maxvalue})
}}
    """
  
    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Provide a compilation of chemical compounds exhibiting {PropertyName} within the range of {minvalue} to {maxvalue}.
What are some species that display {PropertyName} within the {minvalue} to {maxvalue} range?
List chemical compounds having {PropertyName} values falling between {minvalue} and {maxvalue}
Can you name some molecules where {PropertyName} is within {minvalue} and {maxvalue}?
What chemical species exhibit {PropertyName} within the {minvalue} to {maxvalue} range?
Enumerate compounds with {PropertyName} ranging from {minvalue} to {maxvalue}.
Which chemical compounds have {PropertyName} values within {minvalue} and {maxvalue}?
Provide a list of species that demonstrate {PropertyName} in the {minvalue} to {maxvalue} range
Name some molecules with {PropertyName} falling between {minvalue} and {maxvalue}.
What are the chemical compounds or species that possess {PropertyName} within {minvalue} and {maxvalue}?
Can you list some examples of molecules exhibiting {PropertyName} between {minvalue} and {maxvalue}?
Enumerate the chemical compounds or species with {PropertyName} values within {minvalue} and {maxvalue}
Provide a compilation of chemical species having {PropertyName} ranging from {minvalue} to {maxvalue}.
What molecules demonstrate {PropertyName} within the {minvalue} to {maxvalue} range?
List some examples of chemical compounds with {PropertyName} values between {minvalue} and {maxvalue}.
Can you name some species that exhibit {PropertyName} falling within the {minvalue} to {maxvalue} range?
What are the chemical compounds or species that show {PropertyName} in the {minvalue} to {maxvalue} range?
Enumerate compounds with {PropertyName} within the {minvalue} and {maxvalue} values.
Provide a list of chemical compounds demonstrating {PropertyName} values within {minvalue} and {maxvalue}.
Name some molecules that possess {PropertyName} within the range of {minvalue} to {maxvalue}.
Species with {PropertyName} between {minvalue} to {maxvalue}"""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return query_text, prompt

###

def get_species_from_property_A(PropertyName, value):
    query_text = f"""
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
    ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
    ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}

    FILTER(?{PropertyName}Value > {value}*0.9 && ?{PropertyName}Value < {value}*1.1)
}}
    """
  
    PropertyName = add_space_and_lower(PropertyName)

    question_text = f"""Give me a list of species with {PropertyName} around {value}.
Provide a roster of chemical species exhibiting {PropertyName} approximately at {value}.
List molecules that possess {PropertyName} around {value}.
Enumerate compounds demonstrating {PropertyName} roughly at {value}.
Could you enlist chemical species with {PropertyName} around {value}?
I'm interested in a catalog of molecules displaying {PropertyName} approximately at {value}.
Please compile a record of compounds showcasing {PropertyName} around {value}.
Share a list of species manifesting {PropertyName} around {value}, be they chemical species, molecules, or compounds.
What are some chemical species with {PropertyName} around {value}?
I'm curious about molecules that have {PropertyName} around {value}. Could you provide a list?
Could you prepare a roster of compounds with {PropertyName} around {value}?
I'm looking for chemical species that demonstrate {PropertyName} at around {value}.
Provide a compilation of molecules showcasing {PropertyName} at approximately {value}.
List compounds that exhibit {PropertyName} around {value}, please.
Can you give me a rundown of chemical species with {PropertyName} approximately at {value}?
I'd like to see a list of molecules with {PropertyName} around {value}, if possible.
I'm inquiring about compounds that have {PropertyName} around {value}. Could you assist me?
Could you gather a list of chemical species showcasing {PropertyName} roughly at {value}?
What are some molecules with {PropertyName} around {value}?
Prepare a catalog of compounds displaying {PropertyName} around {value}.
Point out molecules demonstrating {PropertyName} around {value}.  """

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return query_text, prompt

###

def get_species_from_chemclass(ChemClass):
    query_text = f"""
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
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
	?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label rdfs:label ?chemicalclass .

    FILTER( ?chemicalclass = "{ChemClass}")
}}
    """

    question_text = f"""Give me a list of species that are classified as {ChemClass} 
Give me a list of species that are {ChemClass}
Provide a roster of chemical entities falling under {ChemClass}.
Enumerate the molecules categorized as {ChemClass}.
List the compounds that belong to the {ChemClass} classification.
Offer a catalog of chemical species under the {ChemClass} category.
Present a compilation of {ChemClass} classifiable entities.
Outline the members of {ChemClass} in terms of chemical species.
Detail the chemical compounds found within the {ChemClass} grouping.
Illustrate a record of {ChemClass}-related molecules.
Disclose a series of chemical entities attributed to {ChemClass}.
Display a rundown of {ChemClass} affiliated compounds.
Arrange a summary of {ChemClass} specific chemical species.
Offer an inventory of {ChemClass} characterized molecules.
Present a list of chemical species falling within {ChemClass}.
Provide a collection of compounds linked to the {ChemClass} category.
Furnish information about {ChemClass} classified molecules.
Lay out a catalog of chemical entities within the {ChemClass} scope.
Deliver a compilation of {ChemClass} associated species.
Share details about the molecules classified as {ChemClass}.
Enumerate the chemical compounds encompassed by {ChemClass}.
Outline the roster of chemical species attributed to {ChemClass}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return query_text, prompt

###

def get_species_from_use(Use):
    query_text = f"""
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?label ?IUPACNameValue 
WHERE {{
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

    ?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .

    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI os:value ?use .

    FILTER( ?use = "{Use}")
}}
    """

    question_text = f"""Give me a list of species that are used as {Use}
Provide a roster of chemical entities employed as {Use}.
Enumerate the molecules utilized for {Use}.
List the compounds applied in {Use}.
Offer a catalog of chemical species employed for {Use}.
Present a compilation of molecules used in {Use}.
Detail the chemical components utilized for {Use}.
Outline the compounds that find application in {Use}.
Furnish a record of chemical entities utilized for {Use}.
Give a rundown of molecular species employed in {Use}.
Mention the substances used as {Use}.
Name the chemical species that serve {Use}.
Provide an inventory of compounds utilized in {Use}.
Identify the molecules employed for {Use}.
Offer a list of chemical components used as {Use}.
Present a summary of species applied in {Use}.
Enumerate the chemical entities that find application in {Use}.
List the molecular species utilized for {Use}.
Detail the compounds that serve {Use}.
Outline the substances applied in {Use}.
Furnish a compilation of chemical species employed as {Use}."""

    lines = question_text.splitlines()
    prompt = random.choice(lines)

    return query_text, prompt

###

