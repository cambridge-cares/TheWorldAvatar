from pubchemagent.kgoperations.querykg import kg_operations
from pubchemagent.kgoperations.querytemplates import *
from pubchemagent.kgoperations.getkgdata import *
import uuid
import re
from pubchemagent.utils.default_configs import UPDATE_ENDPOINT
from pubchemagent.utils.url_configs import ONTOSPECIES_URL, ONTOKIN_URL, UNIT_URL

# a sample data addition function
def insert_ontospecies(typeIRI, type, uuid, data):
    prev_key = ''
    insert_str1 = ' '
    for item in data:
        insert_str = ''
        if data[item].get('key') == prev_key:
            i = i+1 
        else:
            i = 1
        if data[item].get('reference'):
            prov_IRI = '<' + ONTOKIN_URL + 'Reference>'
            prov_uuid = find_uuid('Reference' , prov_IRI, data[item].get('reference'))
        else:
            prov_IRI = '<' + ONTOKIN_URL + 'Reference>'
            prov_uuid = find_uuid('Reference' , prov_IRI, '')
        if data[item].get('type')=='identifier':
            insert_str = pubchem_id_insert(typeIRI, type, uuid, i,  prov_uuid, data[item])
        elif data[item].get('type') in {'num_prop', 'thermo_prop'}:
            if 'unit' in data[item].get('value'):
                unit_IRI = '<' + UNIT_URL + '>'
                unit_string = str(data[item].get('value').get('unit'))
                unit_string=re.sub("°","deg",unit_string)
                unit_uuid = find_uuid('Unit' , unit_IRI, unit_string)
                if data[item].get('type')=='num_prop':
                    insert_str = pubchem_num_prop_insert(typeIRI, type, uuid, i, prov_uuid, unit_uuid, data[item])
                elif data[item].get('type')=='thermo_prop':
                    ref_unit_string = str(data[item].get('value').get('ref_unit'))
                    ref_unit_string=re.sub("°","deg",ref_unit_string)
                    ref_unit_uuid = find_uuid('Unit' , unit_IRI, ref_unit_string)
                    ref_state_value = str(data[item].get('value').get('ref_value'))
                    ref_state_uuid = find_ref_state_uuid(ref_state_value, ref_unit_uuid)
                    insert_str = pubchem_thermo_prop_insert(typeIRI, type, uuid, i, prov_uuid, unit_uuid, ref_state_uuid, data[item])
        elif data[item].get('type') == 'string_prop':
            insert_str = pubchem_string_prop_insert(typeIRI, type, uuid, i, prov_uuid, data[item])
        elif data[item].get('type') == 'classification':
            if data[item].get('value2'):
                classificationIRI = '<' + ONTOSPECIES_URL + data[item].get('key') + '>'
                classification_uuid1 = find_uuid(data[item].get('key'), classificationIRI, data[item].get('value1'), data[item].get('description') )
                classification_uuid2 = find_uuid(data[item].get('key'), classificationIRI, data[item].get('value2'), data[item].get('description') )
                insert_str = pubchem_hm_classification_insert(typeIRI, type, uuid, i, prov_uuid, classification_uuid1, classification_uuid2, data[item])
            else:
                classificationIRI = '<' + ONTOSPECIES_URL + data[item].get('key') + '>'
                classification_uuid = find_uuid(data[item].get('key'), classificationIRI, data[item].get('value'), data[item].get('description') )
                insert_str = pubchem_classification_insert(typeIRI, type, uuid, i, prov_uuid, classification_uuid, data[item])
        elif data[item].get('type') == 'use':
            useIRI = '<' + ONTOSPECIES_URL + data[item].get('key') + '>'
            use_uuid = find_uuid(data[item].get('key'), useIRI, data[item].get('value'), data[item].get('description'))
            insert_str = pubchem_use_insert(typeIRI, type, uuid, i, prov_uuid, use_uuid, data[item])
        elif data[item].get('type') == 'group':
            groupIRI = '<' + ONTOSPECIES_URL + data[item].get('key') + '>'
            group_uuid = find_uuid(data[item].get('key'), groupIRI, data[item].get('value'), data[item].get('description'))
            insert_str = pubchem_group_insert(typeIRI, type, uuid, i, prov_uuid, group_uuid, data[item])
        elif data[item].get('type') == 'synonym':
            insert_str = pubchem_synonym_insert(type, uuid, data[item])

        prev_key = data[item].get('key')
        insert_str1 = insert_str1 + insert_str

    insert_str = insert_str1
    return insert_str

def insert_start(typeIRI, type, uuid, MolecularFormula):
    insert_str = pubchem_start_insert(typeIRI, type, uuid, MolecularFormula)
    return insert_str

def insert_end(insert_str):
    insert_str = insert_str + '}'
    sparqlendpoint = UPDATE_ENDPOINT
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    kg_client.insertkg(insertStr=insert_str)

def insert_structure(typeIRI, type, uuid, geometry, bonds):

    geomIRI = '<http://www.theworldavatar.com/kb/ontospecies/Geometry_1_Species_' + uuid + '>'

    prov_IRI = '<' + ONTOKIN_URL + 'Reference>'
    prov_uuid = find_uuid('Reference' , prov_IRI, 'https://pubchem.ncbi.nlm.nih.gov')

    unit_IRI = '<' + UNIT_URL + '>'
    unit_uuid = find_uuid('Unit' , unit_IRI, 'angstrom')

    for item in geometry:
        elementIRI = get_element_IRI(geometry[item].get('element'))
        geometry[item]['element']=elementIRI
    
    insert_str1 = pubchem_atom_insert(uuid, geomIRI, prov_uuid, unit_uuid, geometry)

    insert_str2 = ''
    for item in bonds:
        insert_str = pubchem_bond_insert(uuid, item+1, bonds[item])
        insert_str2 = insert_str2 + insert_str
    
    insert_str = insert_str1 + insert_str2

    return insert_str

def insert_spectra(typeIRI, type, uuid, data):

    insert_str1 = ' '
    prev_key = ''
    for item in data:
        if data[item].get('key') == prev_key:
            i = i+1 
        else:
            i = 1

        prov_uuid = ''
        solvent_uuid = ''
        unit_uuid = ''
        it_uuid = ''
        im_uuid = ''
        # check for reference
        if data[item].get('reference'):
            prov_IRI = '<' + ONTOKIN_URL + 'Reference>'
            prov_uuid = find_uuid('Reference' , prov_IRI, data[item].get('reference'))
        else:
            prov_IRI = '<' + ONTOKIN_URL + 'Reference>'
            prov_uuid = find_uuid('Reference' , prov_IRI, '', 'data without reference')

        # check for frequency
        if data[item].get('frequency') != '':
                unit_IRI = '<' + UNIT_URL + '>'
                unit_string = str(data[item].get('frequency').get('unit'))
                unit_string=re.sub("°","deg",unit_string)
                unit_uuid = find_uuid('Unit' , unit_IRI, unit_string)

        # check for ionization mode
        if data[item].get('ionization_mode') != '':
                im_IRI = '<' + ONTOSPECIES_URL + 'IonizationMode>'
                im_string = data[item].get('ionization_mode')
                im_uuid = find_uuid('IonizationMode' , im_IRI, im_string)

        # check for instrument type
        if data[item].get('instrument_type') != '':
                it_IRI = '<' + ONTOSPECIES_URL + 'InstrumentType>'
                it_string = data[item].get('instrument_type')
                it_uuid = find_uuid('InstrumentType' , it_IRI, it_string)
        
        # check for instrument solvent
        if data[item].get('solvent') != '':
                solvent_IRI = '<' + ONTOSPECIES_URL + 'Solvent>'
                solvent_string = data[item].get('solvent')
                solvent_uuid = find_uuid('Solvent' , solvent_IRI, solvent_string)

        if data[item].get('type')=='1DNMRSpectra':
            insert_str = pubchem_1DNMR_insert(uuid, i, prov_uuid, unit_uuid, solvent_uuid, it_uuid, data[item])
        elif data[item].get('type')=='2DNMRSpectra':
            insert_str = pubchem_2DNMR_insert(uuid, i, prov_uuid, unit_uuid, solvent_uuid, it_uuid, data[item])
        elif data[item].get('type') == 'MassSpectrometry':
            insert_str = pubchem_ms_insert(uuid, i, prov_uuid, im_uuid, it_uuid, data[item])

        prev_key = data[item].get('key')
        insert_str1 = insert_str1 + insert_str

    insert_str = insert_str1

    return insert_str


def find_uuid(name, typeIRI, string, comment = ''):
        IRI = get_uuid(typeIRI, string)
        if IRI:
            uuid = IRI.partition('_')[2]
        else:
            uuid = create_uuid()
            insert_str = generic_insert(name, typeIRI, uuid, string, comment)
            sparqlendpoint = UPDATE_ENDPOINT
            # create a SPARQL object for performing the query
            kg_client = kg_operations(sparqlendpoint)
            kg_client.insertkg(insertStr=insert_str)
        return uuid

def find_ref_state_uuid(ref_value, ref_unit_uuid):
        IRI = get_ref_uuid(ref_value, ref_unit_uuid)
        if IRI:
            uuid = IRI.partition('_')[2]
        else:
            uuid = create_uuid()
            insert_str = ref_state_insert(uuid, ref_value, ref_unit_uuid)
            sparqlendpoint = UPDATE_ENDPOINT
            # create a SPARQL object for performing the query
            kg_client = kg_operations(sparqlendpoint)
            kg_client.insertkg(insertStr=insert_str)
        return uuid
 
# create a new UUID
def create_uuid():
    return str(uuid.uuid4())