
def get_iri_query(inchi_string):
    query= f"""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?IRI 
    WHERE
    {{
    ?IRI rdf:type os:Species ;
         os:inChI ?x .
      FILTER (str(?x) ="{inchi_string}")
    }}    
    """
    return query

def spec_inchi_query(inchi_string):
    query = f"""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI ?Inchi
    WHERE
    {{
    ?speciesIRI rdf:type os:Species ;
            os:hasInChI ?Inchi .
    ?Inchi os:value ?Inchistr .
    FILTER (str(?Inchistr) = "{inchi_string}")
    }}
    """
    return query

# insert string prefixs
def pubchem_start_insert(typeIRI, type, uuid, MolecularFormula):
    insert_str = f"""
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX gc: <http://purl.org/gc/>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    
    INSERT DATA
    {{
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> rdf:type {typeIRI} .
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> rdfs:label \'{MolecularFormula}\' .
    """
    return insert_str

# identifiers insert
def pubchem_id_insert(typeIRI, type, uuid, item, prov_iri, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> rdf:type os:{data.get('key')} .
    os:{data.get('key')} rdfs:subClassOf os:Identifier . 
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:value "{data.get('value')}"^^xsd:string .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:hasProvenance <http://www.theworldavatar.com/kb/ontospecies/Reference_{prov_iri}> .    
    """
    return insert_str

def pubchem_synonym_insert(type, uuid, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> skos:altLabel "{data.get('value')}" .  
    """        
    return insert_str

# numerical properties insert (not thermo)
def pubchem_num_prop_insert(typeIRI, type, uuid, item, prov_iri, unit_iri, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> rdf:type os:{data.get('key')} .
    os:{data.get('key')} rdfs:subClassOf os:Property .
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:value \'{data['value'].get('value')}\'^^xsd:float .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:unit <http://www.theworldavatar.com/kb/ontospecies/Unit_{unit_iri}>  .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:hasProvenance <http://www.theworldavatar.com/kb/ontospecies/Reference_{prov_iri}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:dateOfAccess \'{data.get('dateofaccess')}\'^^xsd:date .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:originalDataString \'{data.get('description')}\'^^xsd:string .  
    """      
    return insert_str

def pubchem_thermo_prop_insert(typeIRI, type, uuid, item, prov_iri, unit_iri, ref_state_uuid, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> rdf:type os:{data.get('key')} .
    os:{data.get('key')} rdfs:subClassOf os:ThermoProperty .
    os:ThermoProperty rdfs:subClassOf os:Property .
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:value \'{data['value'].get('value')}\'^^xsd:float .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:unit <http://www.theworldavatar.com/kb/ontospecies/Unit_{unit_iri}>  .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:hasReferenceState <http://www.theworldavatar.com/kb/ontospecies/ReferenceState_{ref_state_uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:hasProvenance <http://www.theworldavatar.com/kb/ontospecies/Reference_{prov_iri}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:dateOfAccess \'{data.get('dateofaccess')}\'^^xsd:date .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:originalDataString "{data.get('description')}"^^xsd:string .   
    """       
    return insert_str

def pubchem_string_prop_insert(typeIRI, type, uuid, item, prov_iri, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> rdf:type {typeIRI} .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> rdf:type os:{data.get('key')} .
    os:{data.get('key')} rdfs:subClassOf os:Property .
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:value \'{data.get('description')}\'^^xsd:string .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:hasProvenance <http://www.theworldavatar.com/kb/ontospecies/Reference_{prov_iri}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:dateOfAccess \'{data.get('dateofaccess')}\'^^xsd:date .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{item}_{type}_{uuid}> os:originalDataString \'{data.get('description')}\'^^xsd:string .  
    """        
    return insert_str

def pubchem_classification_insert(typeIRI, type, uuid, item, prov_iri, classification_uuid, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{classification_uuid}> rdf:type os:{data.get('key')} .
    os:{data.get('key')} rdfs:subClassOf os:Classification .
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{classification_uuid}> .   
    """        
    return insert_str

def pubchem_hm_classification_insert(typeIRI, type, uuid, item, prov_iri, classification_uuid1, classification_uuid2, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{classification_uuid1}> rdf:type os:{data.get('key')} .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{classification_uuid2}> rdf:type os:{data.get('key')} .
    os:{data.get('key')} rdfs:subClassOf os:Classification .
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{classification_uuid1}> .   
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{classification_uuid1}> rdf:type <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{classification_uuid2}> .  
    """        
    return insert_str

def pubchem_use_insert(typeIRI, type, uuid, item, prov_iri, use_uuid, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{use_uuid}> rdf:type os:{data.get('key')} .
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{use_uuid}> .   
    """        
    return insert_str

def pubchem_group_insert(typeIRI, type, uuid, item, prov_iri, group_uuid, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{group_uuid}> rdf:type os:{data.get('key')} .
    <http://www.theworldavatar.com/kb/ontospecies/{type}_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{group_uuid}> .   
    """      
    return insert_str

def pubchem_atom_insert(uuid, geometryIRI, prov_uuid, unit_uuid, data):
    text_atom = ''
    for p_item in data.keys():
        text= f"""<http://www.theworldavatar.com/kb/ontospecies/Species_{uuid}> gc:hasAtom <http://www.theworldavatar.com/kb/ontospecies/Atom_{data[p_item].get('id')}_Species_{uuid}> .
            <http://www.theworldavatar.com/kb/ontospecies/Atom_{data[p_item].get('id')}_Species_{uuid}> rdf:type gc:Atom ;
                os:hasXCoordinate <http://www.theworldavatar.com/kb/ontospecies/Atom_{data[p_item].get('id')}_XCoordinate_1_Species_{uuid}> ;
                os:hasYCoordinate <http://www.theworldavatar.com/kb/ontospecies/Atom_{data[p_item].get('id')}_YCoordinate_1_Species_{uuid}> ;
                os:hasZCoordinate <http://www.theworldavatar.com/kb/ontospecies/Atom_{data[p_item].get('id')}_ZCoordinate_1_Species_{uuid}> ;
                os:hasCanonicalOrder \'{data[p_item].get('id')}\'^^xsd:integer ;
                gc:isElement <{data[p_item].get('element')}> .
            <http://www.theworldavatar.com/kb/ontospecies/Atom_{data[p_item].get('id')}_XCoordinate_1_Species_{uuid}> rdf:type os:XCoordinate ;
                os:value \'{data[p_item].get('x')}\'^^xsd:float ;
                os:unit <http://www.theworldavatar.com/kb/ontospecies/Unit_{unit_uuid}> ;
                os:fromGeometry {geometryIRI} .
            <http://www.theworldavatar.com/kb/ontospecies/Atom_{data[p_item].get('id')}_YCoordinate_1_Species_{uuid}> rdf:type os:YCoordinate ;
                os:value \'{data[p_item].get('y')}\'^^xsd:float ;
                os:unit <http://www.theworldavatar.com/kb/ontospecies/Unit_{unit_uuid}> ;
                os:fromGeometry {geometryIRI} .
            <http://www.theworldavatar.com/kb/ontospecies/Atom_{data[p_item].get('id')}_ZCoordinate_1_Species_{uuid}> rdf:type os:ZCoordinate ;
                os:value \'{data[p_item].get('z')}\'^^xsd:float ;
                os:unit <http://www.theworldavatar.com/kb/ontospecies/Unit_{unit_uuid}> ;
                os:fromGeometry {geometryIRI} .
        """
        text_atom = text_atom + text
    
    insert_str = f"""
    <http://www.theworldavatar.com/kb/ontospecies/Species_{uuid}> os:hasGeometry {geometryIRI} .
    {geometryIRI} rdf:type os:Geometry ;
        os:hasProverance <http://www.theworldavatar.com/kb/ontospecies/Reference_{prov_uuid}> .
    {text_atom}    
    """
    return insert_str

def pubchem_bond_insert(uuid, item, data):
    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/Species_{uuid}> rdf:type os:Species ;
        os:hasAtomicBond <http://www.theworldavatar.com/kb/ontospecies/Bond_{item}_Species_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/Bond_{item}_Species_{uuid}> rdf:type os:AtomicBond ;
        os:hasBondOrder \'{data.get('order')}\'^^xsd:integer ;
        os:definedBy <http://www.theworldavatar.com/kb/ontospecies/Atom_{data.get('id1')}_Species_{uuid}> ;
        os:definedBy <http://www.theworldavatar.com/kb/ontospecies/Atom_{data.get('id2')}_Species_{uuid}> .   
    """       
    return insert_str

def pubchem_1DNMR_insert(uuid, i, prov_uuid, unit_uuid, solvent_uuid, it_uuid, data):
    if 'value' in data['frequency']:
        frequency =  str(data['frequency'].get('value'))
    else:
        frequency = ''

    text_peak = ''
    k = 0
    for p_item in data['peaks'].get('x1'):
        shift1 = data['peaks'].get('x1')[k]
        intensity = data['peaks'].get('intensity')[k]
        k = k + 1
        text= f"""<http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> os:hasPeak <http://www.theworldavatar.com/kb/ontospecies/Peak_{str(k)}_SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> .
            <http://www.theworldavatar.com/kb/ontospecies/Peak_{str(k)}_SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> os:hasX1 \'{shift1}\'^^xsd:float ;
                os:hasY \'{intensity}\'^^xsd:float .
            """
        text_peak = text_peak + text

    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{str(i)}_Species_{uuid}> rdf:type os:{data.get('key')} .
        os:{data.get('key')} rdfs:subClassOf os:1DNMRSpectra .
        os:1DNMRSpectra rdfs:subClassOf os:NMRSpectra .
        os:NMRSpectra rdfs:subClassOf os:SpectralInformation .
    <http://www.theworldavatar.com/kb/ontospecies/Species_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{str(i)}_Species_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{str(i)}_Species_{uuid}> os:hasFrequency <http://www.theworldavatar.com/kb/ontospecies/Frequency_{data.get('key')}_{str(i)}_Species_{uuid}> ;
        os:hasSolvent <http://www.theworldavatar.com/kb/ontospecies/Solvent_{solvent_uuid}>  ;
        os:hasInstrumentType <http://www.theworldavatar.com/kb/ontospecies/InstrumentType_{it_uuid}>  ;
        os:hasProvenance <http://www.theworldavatar.com/kb/ontospecies/Reference_{prov_uuid}> ;
        os:hasSpectraGraph <http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> rdf:type os:SpectraGraph .
    <http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> os:hasX1Axis \'Shifts [ppm]\' ;
        os:hasYAxis \'Intensity\' .
    <http://www.theworldavatar.com/kb/ontospecies/Frequency_{data.get('key')}_{str(i)}_Species_{uuid}> os:value \'{frequency}\'^^xsd:float ;
        os:unit <http://www.theworldavatar.com/kb/ontospecies/Unit_{unit_uuid}> .
    {text_peak}       
    """
    return insert_str    

def pubchem_2DNMR_insert(uuid, i, prov_uuid, unit_uuid, solvent_uuid, it_uuid, data):
    if 'value' in data['frequency']:
        frequency =  str(data['frequency'].get('value'))
    else:
        frequency = ''

    text_peak = ''
    k = 0
    for p_item in data['peaks'].get('x1'):
        shift1 = data['peaks'].get('x1')[k]
        shift2 = data['peaks'].get('x2')[k]
        intensity = data['peaks'].get('intensity')[k]
        k = k + 1
        text= f"""<http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> os:hasPeak <http://www.theworldavatar.com/kb/ontospecies/Peak_{str(k)}_SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> .
            <http://www.theworldavatar.com/kb/ontospecies/Peak_{str(k)}_SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> os:hasX1 \'{shift1}\'^^xsd:float ;
                os:hasX2 \'{shift2}\'^^xsd:float ;
                os:hasY \'{intensity}\'^^xsd:float .
            """
        text_peak = text_peak + text

    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{str(i)}_Species_{uuid}> rdf:type os:{data.get('key')} .
        os:{data.get('key')} rdfs:subClassOf os:2DNMRSpectra .
        os:2DNMRSpectra rdfs:subClassOf os:NMRSpectra .
        os:NMRSpectra rdfs:subClassOf os:SpectralInformation .
    <http://www.theworldavatar.com/kb/ontospecies/Species_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{str(i)}_Species_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{str(i)}_Species_{uuid}> os:hasFrequency <http://www.theworldavatar.com/kb/ontospecies/Frequency_{data.get('key')}_{str(i)}_Species_{uuid}> ;
        os:hasSolvent <http://www.theworldavatar.com/kb/ontospecies/Solvent_{solvent_uuid}>  ;
        os:hasInstrumentType <http://www.theworldavatar.com/kb/ontospecies/InstrumentType_{it_uuid}>  ;
        os:hasProvenance <http://www.theworldavatar.com/kb/ontospecies/Reference_{prov_uuid}> ;
        os:hasSpectraGraph <http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> rdf:type os:SpectraGraph .
    <http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> os:hasX1Axis \'Shifts [ppm]\' ;
        os:hasX2Axis \'Shifts [ppm]\' ;
        os:hasYAxis \'Intensity\' .
    <http://www.theworldavatar.com/kb/ontospecies/Frequency_{data.get('key')}_{str(i)}_Species_{uuid}> os:value \'{frequency}\'^^xsd:float ;
        os:unit <http://www.theworldavatar.com/kb/ontospecies/Unit_{unit_uuid}> .
    {text_peak}        
    """
    return insert_str  

def pubchem_ms_insert(uuid, i, prov_uuid, im_uuid, it_uuid, data):

    text_peak = ''
    k = 0
    for p_item in data['peaks'].get('x1'):
        shift1 = data['peaks'].get('x1')[k]
        intensity = data['peaks'].get('intensity')[k]
        k = k + 1
        text= f"""<http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> os:hasPeak <http://www.theworldavatar.com/kb/ontospecies/Peak_{str(k)}_SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> .
            <http://www.theworldavatar.com/kb/ontospecies/Peak_{str(k)}_SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> os:hasX1 \'{shift1}\'^^xsd:float ;
                os:hasY \'{intensity}\'^^xsd:float .
            """
        text_peak = text_peak + text

    insert_str = f"""<http://www.theworldavatar.com/kb/ontospecies/Species_{uuid}> rdf:type os:Species .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{str(i)}_Species_{uuid}> rdf:type os:{data.get('key')} .
        os:{data.get('key')} rdfs:subClassOf os:MassSpectrometry .
        os:MassSpectrometry rdfs:subClassOf os:SpectralInformation .
    <http://www.theworldavatar.com/kb/ontospecies/Species_{uuid}> os:has{data.get('key')} <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{str(i)}_Species_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/{data.get('key')}_{str(i)}_Species_{uuid}> os:hasIonizationMode <http://www.theworldavatar.com/kb/ontospecies/IonizationMode_{im_uuid}> ;
        os:hasInstrumentType <http://www.theworldavatar.com/kb/ontospecies/InstrumentType_{it_uuid}>  ;
        os:hasProvenance <http://www.theworldavatar.com/kb/ontospecies/Reference_{prov_uuid}> ;
        os:hasSpectraGraph <http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> .
    <http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> rdf:type os:SpectraGraph .
    <http://www.theworldavatar.com/kb/ontospecies/SpectraGraph_{data.get('key')}_{str(i)}_Species_{uuid}> os:hasX1Axis \'m/z\' ;
        os:hasYAxis \'Intensity\' .
    {text_peak}        
    """
    return insert_str    

# query and insert of indipendent classes
def generic_insert(name, typeIRI, uuid, string, comment):
    if comment == None:
        comment = ''
    insert_str= f"""
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    INSERT DATA
    {{
    <http://www.theworldavatar.com/kb/ontospecies/{name}_{uuid}> rdf:type {typeIRI} ;
                rdfs:label "{string}" ;
                rdfs:comment \'{comment}\'
    }}    
    """
    return insert_str

def generic_query(typeIRI, string):
    query= f"""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?IRI 
    WHERE
    {{
    ?IRI rdf:type {typeIRI} ;
         rdfs:label ?string .
      FILTER (str(?string) ="{string}")
    }}    
    """
    return query

def element_query(symbol):
    query= f"""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX pt:<http://www.daml.org/2003/01/periodictable/PeriodicTable#>


    SELECT ?IRI 
    WHERE
    {{
    ?IRI rdf:type pt:Element ;
         os:hasElementSymbol ?x .
    ?x os:value ?symbol .
      FILTER (str(?symbol) ="{symbol}")
    }}    
    """
    return query

def ref_state_insert(uuid, ref_value, ref_unit_uuid):
    insert_str= f"""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

    INSERT DATA
    {{
    <http://www.theworldavatar.com/kb/ontospecies/ReferenceState_{uuid}> rdf:type os:ReferenceState .
    <http://www.theworldavatar.com/kb/ontospecies/ReferenceState_{uuid}> os:value \'{ref_value}\'^^xsd:float .
    <http://www.theworldavatar.com/kb/ontospecies/ReferenceState_{uuid}> os:unit <http://www.theworldavatar.com/kb/ontospecies/Unit_{ref_unit_uuid}>  .
    }}    
    """
    return insert_str

def ref_state_query(ref_value, ref_unit_uuid):
    query= f"""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?IRI 
    WHERE
    {{
    ?IRI rdf:type os:ReferenceState ;
         os:value ?ref_value ;
         os:unit <http://www.theworldavatar.com/kb/ontospecies/Unit_{ref_unit_uuid}> .
      FILTER ( ?ref_value = \'{ref_value}\' || str(?ref_value) = \'{ref_value}\')
    }}    
    """
    return query