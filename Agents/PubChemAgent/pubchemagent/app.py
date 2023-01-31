from pubchemagent.kgoperations.getkgdata import *
from pubchemagent.kgoperations.addkgdata import * 
from pubchemagent.pug import pug_api
import time

def species_instantiation(inchi):
    pug_access = pug_api()
    IRI = get_iri_data(inchi)
    if IRI:
        uuid = IRI.partition('_')[2]
    else:
        uuid = create_uuid()

    start_time = time.time()
    data = pug_access.pug_request('InChI', inchi)
    cid, props, identifiers = pug_access.get_props(data)
    cid = pug_access.check_preferred(cid)
    data_3d = pug_access.pug_request_prop_3d(cid)
    if 'Record' in data_3d:
        geometry, bonds = pug_access.get_structure('3d', data_3d)
    else:
        geometry, bonds = pug_access.get_structure('2d', data)
    exp_props = pug_access.pug_request_exp_prop(cid)
    uses = pug_access.pug_request_uses(cid)
    ghs = pug_access.pug_request_ghs_classification(cid)
    spectra = pug_access.pug_request_spectra(cid)
    print("--- Import PubChem: %s seconds ---" % (time.time() - start_time))

    typeIRI = '<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species>'  
    start_time = time.time()
    insert_structure(typeIRI, 'Species', uuid, geometry, bonds)   
    print("--- Geometry: %s seconds ---" % (time.time() - start_time))
    start_time = time.time()
    insert_ontospecies(typeIRI, 'Species', uuid, identifiers)
    print("--- Identifier: %s seconds ---" % (time.time() - start_time))
    start_time = time.time()
    insert_ontospecies(typeIRI, 'Species', uuid, props)
    print("--- Comp. Prop.: %s seconds ---" % (time.time() - start_time))
    start_time = time.time()
    insert_ontospecies(typeIRI, 'Species', uuid, exp_props) 
    print("--- Exp. Prop.: %s seconds ---" % (time.time() - start_time)) 
    start_time = time.time()
    insert_ontospecies(typeIRI, 'Species', uuid, uses) 
    print("--- Uses: %s seconds ---" % (time.time() - start_time)) 
    start_time = time.time()
    insert_ontospecies(typeIRI, 'Species', uuid, ghs) 
    print("--- GHS: %s seconds ---" % (time.time() - start_time))   
    start_time = time.time()
    insert_spectra(typeIRI, 'Species', uuid, spectra)
    print("--- Spectra: %s seconds ---" % (time.time() - start_time))

def element_instantiation(el):
    pug_access = pug_api()
    data = pug_access.pug_request_element(el)
    uuid = create_uuid()
    typeIRI = '<http://www.daml.org/2003/01/periodictable/PeriodicTable#Element>'
    insert_ontospecies(typeIRI, 'Element', uuid, data)

