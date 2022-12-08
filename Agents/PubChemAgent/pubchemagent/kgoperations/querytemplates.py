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


def ontospecies_data_query(osIRI):
    query = """
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX otk: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

        SELECT ?MolecularWeight ?MolecularFormula ?enthalpy_ref
               ?enthalpy_ref_unit ?enthalpy_ref_temp
               ?enthalpy_ref_temp_unit ?enthalpy_ref_prov
               ?InChI ?CID ?SMILES ?InChIKey
        WHERE {
            <#osIRI#> os:hasMolecularWeight ?y .
            ?y os:value ?MolecularWeight .
            <#osIRI#> rdfs:label ?MolecularFormula .
            <#osIRI#> os:inChI ?InChI .
            OPTIONAL{
                <#osIRI#> os:pubChemCID ?CID .
            }
            OPTIONAL{
                <#osIRI#> os:SMILES ?SMILES .
            }
            OPTIONAL{
                <#osIRI#> os:inChIKey ?inChIKey .
            }
            FILTER (!REGEX(str(?MolecularFormula), "Species", "i"))

            OPTIONAL {
                <#osIRI#> os:hasStandardEnthalpyOfFormation ?standardEnthalpyOfFormation .
                ?standardEnthalpyOfFormation os:hasPhase ?phase ;
                            os:hasReferenceTemperature ?tref ;
                            os:hasProvenance ?prov ;
                            os:value ?enthalpy_ref ;
                            os:units ?enthalpy_ref_unit .

                ?phase a otk:GasPhase .

                ?tref os:value ?enthalpy_ref_temp ;
                      os:units ?enthalpy_ref_temp_unit .

                ?prov rdfs:label ?enthalpy_ref_prov .
                FILTER (!REGEX(str(?enthalpy_ref_prov), "Reference", "i"))
            }
        }""".replace('#osIRI#',osIRI)
    return query

def test_data_query():
    query = """
    SELECT *
    WHERE{
    ?x ?y ?z . 
    } LIMIT 10
    """
    return query

def test_data_insert(OsIRI):
    insert_str = """
    PREFIX dc: <http://purl.org/dc/elements/1.1/>
    INSERT DATA
    { 
      <http://example/book10> dc:title "A very new book" ;
                             dc:creator "Jethro Akroyd" .
    }
    """
    return insert_str

def pubchem_prop_insert_ali(uuid, cid, props):
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
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:hasMolecularWeight <http://www.theworldavatar.com/kg/ontospecies/MolecularWeight_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/MolecularWeight_Species_#uuid#> os:value #Molecular Weight# .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdfs:label \'#Molecular Formula#\' .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> skos:altLabel \'#Allowed IUPAC Name#\' .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> skos:altLabel \'#Preferred IUPAC Name#\' .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:inChI \'#Standard InChI#\' .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:pubChemCID #CID# .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:SMILES \'#Canonical SMILES#\' .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:inChIKey \'#Standard InChIKey#\' .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdfs:comment \'The information collected here is obtained from PubChem PUG API.\' .
    }    
    """.replace('#CID#', cid).replace('#uuid#', uuid)
    # replace "InChI=1S", "InChI=1"
    props['Standard InChI']=props['Standard InChI'].replace("InChI=1S", "InChI=1")
    for item in props.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(props.get(item)))
        
    return insert_str


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

def pubchem_elem_insert(uuid, data):
    insert_str = """
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX otk: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> rdf:type pt:Element .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_Element_#uuid#> rdf:type os:#key# .
    <http://www.theworldavatar.com/kg/ontospecies/Element_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_Element_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_Element_#uuid#> os:value \'#value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_Element_#uuid#> os:hasProvenance \'#provenance#\' .
    }    
    """.replace('#uuid#', uuid)
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str