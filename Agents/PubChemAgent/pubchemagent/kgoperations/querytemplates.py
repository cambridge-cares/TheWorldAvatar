from pubchemagent.kgoperations.queryendpoints import SPARQL_ENDPOINTS
import json

def get_iri_query(inchi_string):
    query="""
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI ?Inchi (datatype(?Inchi) AS ?type)
    WHERE
    {
    ?speciesIRI rdf:type OntoSpecies:Species ;
                OntoSpecies:inChI ?Inchi .
      FILTER (str(?Inchi) ="#InChI#")
    }    
    """.replace('#InChI#', inchi_string)
    return query

def spec_inchi_query(inchi_string):
    query = """
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI ?Inchi
    WHERE
    {
    ?speciesIRI rdf:type OntoSpecies:Species .
    ?speciesIRI OntoSpecies:hasInChI ?Inchi .
    ?InChi OntoSpecies:value ?Inchistr .
    FILTER REGEX(str(?Inchistr), REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(""" + '"' + inchi_string + '"' + """, "InChI=1", "InChI=1"), "/t.+", ""), "/b.+", ""), "\\\\(", "\\\\\\\\("), "\\\\)", "\\\\\\\\)"), "i")
    }
    """
    return query

def pubchem_prop_insert(uuid, props):
    insert_str = """
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX otk: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdf:type os:Species .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_Species_#uuid#> rdf:type os:#key# .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_Species_#uuid#> os:value \'#value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_Species_#uuid#> os:hasProvenance \'#provenance#\' .
    }    
    """.replace('#uuid#', uuid)
    for item in props.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(props.get(item)))
        
    return insert_str

def pubchem_identifiers_insert(uuid, identifiers):
    insert_str = """
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX otk: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdf:type os:Species .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_Species_#uuid#> rdf:type os:#key# .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_Species_#uuid#> os:value \'#value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_Species_#uuid#> os:hasProvenance \'#provenance#\' .
    }    
    """.replace('#uuid#', uuid)
    for item in identifiers.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(identifiers.get(item)))
        
    return insert_str

# insert for elements

# element's identifiers insert
def pubchem_elem_id_insert(uuid, item, prov_iri, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> rdf:type pt:Element .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:Identifier . 
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:value \'#value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    }    
    """.replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#item#', str(item))
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

# element's numerical properties (not thermo)
def pubchem_elem_num_prop_insert(uuid, item, prov_iri, unit_iri, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> rdf:type pt:Element .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:Property .
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:value \'#value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#>  .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:dateOfAccess \'#dateofaccess#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:originalDataString \'#description#\' .
    }    
    """.replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#unit_uuid#', unit_iri).replace('#item#', str(item))
    for item in data['value'].keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data['value'].get(item)))
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_elem_thermo_prop_insert(uuid, item, prov_iri, unit_iri, ref_unit_iri, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> rdf:type pt:Element .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:ThermoProperty .
    <http://www.theworldavatar.com/kg/ontospecies/ReferenceState_#key#_#item#_Element_#uuid#>  rdf:type os:ReferenceState .
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:value \'#value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#>  .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:hasReferenceState <http://www.theworldavatar.com/kg/ontospecies/ReferenceState_#key#_#item#_Element_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/ReferenceState_#key#_#item#_Element_#uuid#> os:value \'#ref_value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/ReferenceState_#key#_#item#_Element_#uuid#> os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#ref_unit_uuid#>  .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:dateOfAccess \'#dateofaccess#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:originalDataString \'#description#\' .
    }    
    """.replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#unit_uuid#', unit_iri).replace('#item#', str(item)).replace('#ref_unit_uuid#', ref_unit_iri)
    for item in data['value'].keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data['value'].get(item)))
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_elem_string_prop_insert(uuid, item, prov_iri, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> rdf:type pt:Element .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:Property .
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:value \'#description#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:dateOfAccess \'#dateofaccess#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Element_#uuid#> os:originalDataString \'#description#\' .
    }    
    """.replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#item#', str(item))
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_elem_classification_insert(uuid, item, prov_iri, classification_uuid, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> rdf:type pt:Element .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#classification_uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:Classification .
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#classification_uuid#> .
    }    
    """.replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#item#', str(item)).replace('#classification_uuid#', classification_uuid)
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str


# query and insert of indipendent classes

def provenance_insert(uuid, provenance_string):
    insert_str="""
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> rdf:type okin:Reference ;
                OntoSpecies:value \'#Provenance#\' .
    }    
    """.replace('#Provenance#', provenance_string).replace('#prov_uuid#', uuid)
    return insert_str

def get_provenance_query(provenance_string):
    query="""
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

    SELECT ?provenanceIRI 
    WHERE
    {
    ?provenanceIRI rdf:type okin:Reference ;
                OntoSpecies:value ?Provenance .
      FILTER (str(?Provenance) ="#Provenance#")
    }    
    """.replace('#Provenance#', provenance_string)
    return query

def unit_insert(uuid, unit_string):
    insert_str="""
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#> rdf:type om:Unit ;
                rdf:label \'#Unit#\' .
    }    
    """.replace('#Unit#', unit_string).replace('#unit_uuid#', uuid)
    return insert_str

def get_unit_query(unit_string):
    query="""
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

    SELECT ?unitIRI 
    WHERE
    {
    ?unitIRI rdf:type om:Unit ;
                rdf:label ?Unit .
      FILTER (str(?Unit) ="#Unit#")
    }    
    """.replace('#Unit#', unit_string)
    return query

def generic_insert(name, typeIRI, uuid, string):
    insert_str="""
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/#name#_#uuid#> rdf:type #TypeIRI# ;
                rdf:label \'#String#\' .
    }    
    """.replace('#String#', string).replace('#uuid#', uuid).replace('#TypeIRI#', typeIRI).replace('#name#', name)
    return insert_str

def generic_query(typeIRI, string):
    query="""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?IRI 
    WHERE
    {
    ?IRI rdf:type #TypeIRI# ;
         rdf:label ?string .
      FILTER (str(?string) ="#String#")
    }    
    """.replace('#TypeIRI#', typeIRI).replace('#String#', string)
    return query