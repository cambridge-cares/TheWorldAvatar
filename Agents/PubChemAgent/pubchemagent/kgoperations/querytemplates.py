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

# identifiers insert
def pubchem_id_insert(typeIRI, type, uuid, item, prov_iri, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> rdf:type #TypeIRI# .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:Identifier . 
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:value \'#value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    }    
    """.replace('#TypeIRI#', typeIRI).replace('#type#', type).replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#item#', str(item))
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

# numerical properties insert (not thermo)
def pubchem_num_prop_insert(typeIRI, type, uuid, item, prov_iri, unit_iri, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> rdf:type #TypeIRI# .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:Property .
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:value \'#value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#>  .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:dateOfAccess \'#dateofaccess#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:originalDataString \"#description#\" .
    }    
    """.replace('#TypeIRI#', typeIRI).replace('#type#', type).replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#unit_uuid#', unit_iri).replace('#item#', str(item))
    for item in data['value'].keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data['value'].get(item)))
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_thermo_prop_insert(typeIRI, type, uuid, item, prov_iri, unit_iri, ref_unit_iri, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> rdf:type #TypeIRI# .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:ThermoProperty .
    <http://www.theworldavatar.com/kg/ontospecies/ReferenceState_#key#_#item#_#type#_#uuid#>  rdf:type os:ReferenceState .
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:value \'#value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#>  .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:hasReferenceState <http://www.theworldavatar.com/kg/ontospecies/ReferenceState_#key#_#item#_#type#_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/ReferenceState_#key#_#item#_#type#_#uuid#> os:value \'#ref_value#\' .
    <http://www.theworldavatar.com/kg/ontospecies/ReferenceState_#key#_#item#_#type#_#uuid#> os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#ref_unit_uuid#>  .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:dateOfAccess \'#dateofaccess#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:originalDataString \"#description#\" .
    }    
    """.replace('#TypeIRI#', typeIRI).replace('#type#', type).replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#unit_uuid#', unit_iri).replace('#item#', str(item)).replace('#ref_unit_uuid#', ref_unit_iri)
    for item in data['value'].keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data['value'].get(item)))
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_string_prop_insert(typeIRI, type, uuid, item, prov_iri, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> rdf:type #TypeIRI# .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:Property .
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:value \'#description#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:dateOfAccess \'#dateofaccess#\' .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_#type#_#uuid#> os:originalDataString \'#description#\' .
    }    
    """.replace('#TypeIRI#', typeIRI).replace('#type#', type).replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#item#', str(item))
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_classification_insert(typeIRI, type, uuid, item, prov_iri, classification_uuid, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> rdf:type #TypeIRI# .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#classification_uuid#> rdf:type os:#key# .
    os:#key# rdf:type os:Classification .
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#classification_uuid#> .
    }    
    """.replace('#TypeIRI#', typeIRI).replace('#type#', type).replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#item#', str(item)).replace('#classification_uuid#', classification_uuid)
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_use_insert(typeIRI, type, uuid, item, prov_iri, use_uuid, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> rdf:type #TypeIRI# .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#use_uuid#> rdf:type os:#key# .
    <http://www.theworldavatar.com/kg/ontospecies/#type#_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#use_uuid#> .
    }    
    """.replace('#TypeIRI#', typeIRI).replace('#type#', type).replace('#uuid#', uuid).replace('#prov_uuid#', prov_iri).replace('#item#', str(item)).replace('#use_uuid#', use_uuid)
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_atom_insert(uuid, elementIRI, geometryIRI, prov_uuid, unit_uuid, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX gc: <http://purl.org/gc/>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdf:type os:Species ;
        gc:hasAtom <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_Species_#uuid#> ;
        os:hasGeometry #GeometryIRI# .
    #GeometryIRI# rdf:type os:Geometry ;
        os:hasProverance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_Species_#uuid#> rdf:type gc:Atom ;
        os:hasXCoordinate <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_XCoordinate_1_Species_#uuid#> ;
        os:hasYCoordinate <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_YCoordinate_1_Species_#uuid#> ;
        os:hasZCoordinate <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_ZCoordinate_1_Species_#uuid#> ;
        os:hasCanonicalOrder \'#id#\' ;
        gc:isElement <#ElementIRI#> .
    <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_XCoordinate_1_Species_#uuid#> rdf:type os:XCoordinate ;
        os:value \'#x#\' ;
        os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#> ;
        os:fromGeometry #GeometryIRI# .
    <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_YCoordinate_1_Species_#uuid#> rdf:type os:XCoordinate ;
        os:value \'#y#\' ;
        os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#> ;
        os:fromGeometry #GeometryIRI# .
    <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_ZCoordinate_1_Species_#uuid#> rdf:type os:XCoordinate ;
        os:value \'#z#\' ;
        os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#> ;
        os:fromGeometry #GeometryIRI# .
    }    
    """.replace('#GeometryIRI#', geometryIRI).replace('#ElementIRI#', elementIRI).replace('#uuid#', uuid).replace('#prov_uuid#', prov_uuid).replace('#unit_uuid#', unit_uuid)
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_bond_insert(uuid, item, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdf:type os:Species ;
        os:hasAtomicBond <http://www.theworldavatar.com/kg/ontospecies/Bond_#item#_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/Bond_#item#_Species_#uuid#> rdf:type os:AtomicBond ;
        os:hasBondOrder \'#order#\' ;
        os:definedBy <http://www.theworldavatar.com/kg/ontospecies/Atom_#id1#_Species_#uuid#> ;
        os:definedBy <http://www.theworldavatar.com/kg/ontospecies/Atom_#id2#_Species_#uuid#> .
    }    
    """.replace('#uuid#', uuid).replace('#item#', str(item))
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))
        
    return insert_str

def pubchem_1DNMR_insert(uuid, i, prov_uuid, unit_uuid, solvent_uuid, it_uuid, data):
    if 'value' in data['frequency']:
        frequency =  str(data['frequency'].get('value'))
    else:
        frequency = ''
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdf:type os:Species .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Species_#uuid#> rdf:type os:#key# .
        os:#key# rdf:type os:1DNMRSpectra .
        os:1DNMRSpectra rdf:type os:SpectralInformation .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Species_#uuid#> os:hasFrequency <http://www.theworldavatar.com/kg/ontospecies/Frequency_#key#_#item#_Species_#uuid#> ;
        os:hasSolvent <http://www.theworldavatar.com/kg/ontospecies/Solvent_#solvent_uuid#>  ;
        os:hasInstrumentType <http://www.theworldavatar.com/kg/ontospecies/InstrumentType_#it_uuid#>  ;
        os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> ;
        os:hasSpectraGraph <http://www.theworldavatar.com/kg/ontospecies/SpectraGraph_#key#_#item#_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/SpectraGraph_#key#_#item#_Species_#uuid#> os:hasX1Axis \'Shifts [ppm]\' ;
        os:hasYAxis \'Intensity\' .
    <http://www.theworldavatar.com/kg/ontospecies/Frequency_#key#_#item#_Species#_#uuid#> os:value \'#frequency#\' ;
        os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#> .
    #peaks#    
    }    
    """.replace('#uuid#', uuid).replace('#prov_uuid#', prov_uuid).replace('#unit_uuid#', unit_uuid).replace('#item#', str(i)).replace('#solvent_uuid#', solvent_uuid).replace('#it_uuid#', it_uuid).replace('#frequency#', frequency)
    text_peak = ''
    k = 0
    for p_item in data['peaks'].get('x1'):
        shift1 = data['peaks'].get('x1')[k]
        intensity = data['peaks'].get('intensity')[k]
        k = k + 1
        text="""<http://www.theworldavatar.com/kg/ontospecies/SpectraGraph_#key#_#item#_Species_#uuid#> os:hasPeak <http://www.theworldavatar.com/kg/ontospecies/Peak_#p_item#_SpectraGraph_#key#_#item#_Species_#uuid#> .
            <http://www.theworldavatar.com/kg/ontospecies/Peak_#p_item#_SpectraGraph_#key#_#item#_Species_#uuid#> os:hasX1 \'#shift1#\' ;
                os:hasY \'#intensity#\' .
            """.replace('#uuid#', uuid).replace('#p_item#', str(k)).replace('#item#', str(i)).replace('#shift1#', shift1).replace('#intensity#', intensity)
        text_peak = text_peak + text
    insert_str=insert_str.replace('#peaks#', text_peak)
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))

    return insert_str    

def pubchem_2DNMR_insert(uuid, i, prov_uuid, unit_uuid, solvent_uuid, it_uuid, data):
    if 'value' in data['frequency']:
        frequency =  str(data['frequency'].get('value'))
    else:
        frequency = ''
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdf:type os:Species .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Species_#uuid#> rdf:type os:#key# .
        os:#key# rdf:type os:2DNMRSpectra .
        os:2DNMRSpectra rdf:type os:SpectralInformation .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Species_#uuid#> os:hasFrequency <http://www.theworldavatar.com/kg/ontospecies/Frequency_#key#_#item#_Species_#uuid#> ;
        os:hasSolvent <http://www.theworldavatar.com/kg/ontospecies/Solvent_#solvent_uuid#>  ;
        os:hasInstrumentType <http://www.theworldavatar.com/kg/ontospecies/InstrumentType_#it_uuid#>  ;
        os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> ;
        os:hasSpectraGraph <http://www.theworldavatar.com/kg/ontospecies/SpectraGraph_#key#_#item#_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/SpectraGraph_#key#_#item#_Species_#uuid#> os:hasX1Axis \'Shifts [ppm]\' ;
        os:hasX2Axis \'Shifts [ppm]\' ;
        os:hasYAxis \'Intensity\' .
    <http://www.theworldavatar.com/kg/ontospecies/Frequency_#key#_#item#_Species#_#uuid#> os:value \'#frequency#\' ;
        os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#> .
    #peaks#    
    }    
    """.replace('#uuid#', uuid).replace('#prov_uuid#', prov_uuid).replace('#unit_uuid#', unit_uuid).replace('#item#', str(i)).replace('#solvent_uuid#', solvent_uuid).replace('#it_uuid#', it_uuid).replace('#frequency#', frequency)
    text_peak = ''
    k = 0
    for p_item in data['peaks'].get('x1'):
        shift1 = data['peaks'].get('x1')[k]
        shift2 = data['peaks'].get('x2')[k]
        intensity = data['peaks'].get('intensity')[k]
        k = k + 1
        text="""<http://www.theworldavatar.com/kg/ontospecies/SpectraGraph_#key#_#item#_Species_#uuid#> os:hasPeak <http://www.theworldavatar.com/kg/ontospecies/Peak_#p_item#_SpectraGraph_#key#_#item#_Species_#uuid#> .
            <http://www.theworldavatar.com/kg/ontospecies/Peak_#p_item#_SpectraGraph_#key#_#item#_Species_#uuid#> os:hasX1 \'#shift1#\' ;
                os:hasX2 \'#shift2#\' ;
                os:hasY \'#intensity#\' .
            """.replace('#uuid#', uuid).replace('#p_item#', str(k)).replace('#item#', str(i)).replace('#shift1#', shift1).replace('#shift2#', shift2).replace('#intensity#', intensity)
        text_peak = text_peak + text
    insert_str=insert_str.replace('#peaks#', text_peak)
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))

    return insert_str  

def pubchem_ms_insert(uuid, i, prov_uuid, im_uuid, it_uuid, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdf:type os:Species .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Species_#uuid#> rdf:type os:#key# .
        os:#key# rdf:type os:MassSpectrometry .
        os:MassSpectrometry rdf:type os:SpectralInformation .
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> os:has#key# <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/#key#_#item#_Species_#uuid#> os:hasIonizationMode <http://www.theworldavatar.com/kg/ontospecies/IonizationMode_#im_uuid#> ;
        os:hasInstrumentType <http://www.theworldavatar.com/kg/ontospecies/InstrumentType_#it_uuid#>  ;
        os:hasProvenance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> ;
        os:hasSpectraGraph <http://www.theworldavatar.com/kg/ontospecies/SpectraGraph_#key#_#item#_Species_#uuid#> .
    <http://www.theworldavatar.com/kg/ontospecies/SpectraGraph_#key#_#item#_Species_#uuid#> os:hasX1Axis \'m/z\' ;
        os:hasYAxis \'Intensity\' .
    #peaks#    
    }    
    """.replace('#uuid#', uuid).replace('#prov_uuid#', prov_uuid).replace('#item#', str(i)).replace('#im_uuid#', im_uuid).replace('#it_uuid#', it_uuid)
    text_peak = ''
    k = 0
    for p_item in data['peaks'].get('x1'):
        shift1 = data['peaks'].get('x1')[k]
        intensity = data['peaks'].get('intensity')[k]
        k = k + 1
        text="""<http://www.theworldavatar.com/kg/ontospecies/SpectraGraph_#key#_#item#_Species_#uuid#> os:hasPeak <http://www.theworldavatar.com/kg/ontospecies/Peak_#p_item#_SpectraGraph_#key#_#item#_Species_#uuid#> .
            <http://www.theworldavatar.com/kg/ontospecies/Peak_#p_item#_SpectraGraph_#key#_#item#_Species_#uuid#> os:hasX1 \'#shift1#\' ;
                os:hasY \'#intensity#\' .
            """.replace('#uuid#', uuid).replace('#p_item#', str(k)).replace('#item#', str(i)).replace('#shift1#', shift1).replace('#intensity#', intensity)
        text_peak = text_peak + text
    insert_str=insert_str.replace('#peaks#', text_peak)
    for item in data.keys():
        insert_str=insert_str.replace('#'+str(item)+'#',str(data.get(item)))

    return insert_str    

# query and insert of indipendent classes
def generic_insert(name, typeIRI, uuid, string, comment):
    insert_str="""
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/#name#_#uuid#> rdf:type #TypeIRI# ;
                rdf:label \'#String#\' ;
                rdfs:comment \'#Comment#\'
    }    
    """.replace('#String#', string).replace('#uuid#', uuid).replace('#TypeIRI#', typeIRI).replace('#name#', name).replace('#Comment#', comment)
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

def element_query(symbol):
    query="""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX pt:<http://www.daml.org/2003/01/periodictable/PeriodicTable#>


    SELECT ?IRI 
    WHERE
    {
    ?IRI rdf:type pt:Element ;
         os:hasElementSymbol ?x .
    ?x os:value ?symbol .
      FILTER (str(?symbol) ="#Symbol#")
    }    
    """.replace('#Symbol#', symbol)
    return query


def pubchem_atom_insert_2(uuid, geometryIRI, prov_uuid, unit_uuid, data):
    insert_str = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX gc: <http://purl.org/gc/>
    
    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> rdf:type os:Species ;
        os:hasGeometry #GeometryIRI# .
    #GeometryIRI# rdf:type os:Geometry ;
        os:hasProverance <http://www.theworldavatar.com/kg/ontospecies/Reference_#prov_uuid#> .
    #atoms#
    }    
    """
    text_atom = ''
    for p_item in data.keys():
        text="""<http://www.theworldavatar.com/kg/ontospecies/Species_#uuid#> gc:hasAtom <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_Species_#uuid#> .
            <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_Species_#uuid#> rdf:type gc:Atom ;
                os:hasXCoordinate <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_XCoordinate_1_Species_#uuid#> ;
                os:hasYCoordinate <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_YCoordinate_1_Species_#uuid#> ;
                os:hasZCoordinate <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_ZCoordinate_1_Species_#uuid#> ;
                os:hasCanonicalOrder \'#id#\' ;
                gc:isElement <#element#> .
            <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_XCoordinate_1_Species_#uuid#> rdf:type os:XCoordinate ;
                os:value \'#x#\' ;
                os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#> ;
                os:fromGeometry #GeometryIRI# .
            <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_YCoordinate_1_Species_#uuid#> rdf:type os:XCoordinate ;
                os:value \'#y#\' ;
                os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#> ;
                os:fromGeometry #GeometryIRI# .
            <http://www.theworldavatar.com/kg/ontospecies/Atom_#id#_ZCoordinate_1_Species_#uuid#> rdf:type os:XCoordinate ;
                os:value \'#z#\' ;
                os:unit <http://www.theworldavatar.com/kg/ontospecies/Unit_#unit_uuid#> ;
                os:fromGeometry #GeometryIRI# .
        """
        for item in data[p_item].keys():
            text = text.replace('#'+str(item)+'#',str(data[p_item].get(item)))
        text_atom = text_atom + text
        
    insert_str=insert_str.replace('#atoms#', text_atom)
    insert_str=insert_str.replace('#GeometryIRI#', geometryIRI).replace('#uuid#', uuid).replace('#prov_uuid#', prov_uuid).replace('#unit_uuid#', unit_uuid)

    return insert_str