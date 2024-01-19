from pubchemagent.kgoperations.getkgdata import *
from pubchemagent.kgoperations.addkgdata import * 
from pubchemagent.pug import pug_api, chebi_api 
from pubchemagent.pug.chebi_classification import get_chebi_tree
from pubchemagent.utils.url_configs import ONTOSPECIES_URL, PT_URL
import time
import logging

# supress excessive log output from imported packages
logging.getLogger('urllib3').setLevel(logging.CRITICAL)
logging.getLogger('requests').setLevel(logging.CRITICAL)
logging.getLogger('requests_cache').setLevel(logging.CRITICAL)
logging.getLogger('bioservices').setLevel(logging.CRITICAL)
logging.getLogger('matplotlib').setLevel(logging.CRITICAL)
logging.getLogger('suds').setLevel(logging.CRITICAL)

def species_instantiation(inchi):
    pug_access = pug_api()
    start_time = time.time()
    IRI, flag = get_iri_data(inchi)
    if IRI:
        uuid = IRI.partition('_')[2]
    else:
        uuid = create_uuid()
        flag = "true"
    print("--- Query IRI: %s seconds ---" % (time.time() - start_time))

    if flag == "true":
        start_time = time.time()
        data = pug_access.pug_request('InChI', inchi)
        cid, props, identifiers = pug_access.get_props(data)
        cid = pug_access.check_preferred(cid)
        data_3d = pug_access.pug_request_prop_3d(cid)
        if 'Record' in data_3d:
            geometry, bonds = pug_access.get_structure('3d', data_3d)
        else:
            geometry, bonds = pug_access.get_structure('2d', data)
        synonyms = pug_access.pug_request_synonyms(cid)
        exp_props = pug_access.pug_request_exp_prop(cid)
        uses = pug_access.pug_request_uses(cid)
        ghs = pug_access.pug_request_ghs_classification(cid)
        spectra = pug_access.pug_request_spectra(cid)
        chebi_prop = chebi_api.chebi_request(inchi)
        print("--- Import PubChem: %s seconds ---" % (time.time() - start_time))

        for item in identifiers:
            if identifiers[item]['key'] == 'MolecularFormula':
                MolecularFormula = identifiers[item]['value']

        typeIRI = '<' + ONTOSPECIES_URL + 'Species>'  
        start_time = time.time()
        insert_str = ''
        temp_str = insert_start(typeIRI, 'Species', uuid, MolecularFormula)
        insert_str = insert_str + temp_str
        temp_str = insert_structure(typeIRI, 'Species', uuid, geometry, bonds)   
        insert_str = insert_str + temp_str
        temp_str = insert_ontospecies(typeIRI, 'Species', uuid, identifiers)
        insert_str = insert_str + temp_str
        temp_str = insert_ontospecies(typeIRI, 'Species', uuid, synonyms)
        insert_str = insert_str + temp_str
        temp_str = insert_ontospecies(typeIRI, 'Species', uuid, props)
        insert_str = insert_str + temp_str
        temp_str = insert_ontospecies(typeIRI, 'Species', uuid, exp_props) 
        insert_str = insert_str + temp_str
        temp_str = insert_ontospecies(typeIRI, 'Species', uuid, uses) 
        insert_str = insert_str + temp_str
        temp_str = insert_ontospecies(typeIRI, 'Species', uuid, ghs)  
        insert_str = insert_str + temp_str
        for item in chebi_prop:
            if chebi_prop[item]['type']=='classification':
                classIRI = '<' + ONTOSPECIES_URL + 'ChemicalClass>'
                cIRI = get_uuid(classIRI, chebi_prop[item]['value'])
                if cIRI == None:
                    get_chebi_tree(chebi_prop[item]['chebiID'])
            elif chebi_prop[item]['type']=='group':
                classIRI = '<' + ONTOSPECIES_URL + 'FunctionalGroup>'
                cIRI = get_uuid(classIRI, chebi_prop[item]['value'])
                if cIRI == None:
                    get_chebi_tree(chebi_prop[item]['chebiID'])
        temp_str = insert_ontospecies(typeIRI, 'Species', uuid, chebi_prop)  
        insert_str = insert_str + temp_str
        temp_str = insert_spectra(typeIRI, 'Species', uuid, spectra)
        insert_str = insert_str + temp_str
        insert_end(insert_str)
        print("--- Instantiation: %s seconds ---" % (time.time() - start_time))

def element_instantiation(el):
    pug_access = pug_api()
    data = pug_access.pug_request_element(el)
    uuid = create_uuid()

    for item in data:
        if data[item]['key'] == 'ElementName':
            ElementName = data[item]['value']

    typeIRI = '<' + PT_URL + 'Element>'
    insert_str = ''
    temp_str = insert_start(typeIRI, 'Element', uuid, ElementName)
    insert_str = insert_str + temp_str
    temp_str = insert_ontospecies(typeIRI, 'Element', uuid, data)
    insert_str = insert_str + temp_str
    insert_end(insert_str)

