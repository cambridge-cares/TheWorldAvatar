from pubchemagent.kgoperations.getkgdata import *
from pubchemagent.kgoperations.addkgdata import * 
from pubchemagent.pug import pug_api

def query_with_inchi(inchi):
    pug_access = pug_api()
    IRI = get_iri_data(inchi)
    if IRI:
        # main get data
        data = get_ontospecies_data(IRI)
        return (data, 'TWA')

    else:

        data = pug_access.pug_request('InChI', inchi)
        props, identifiers = pug_access.get_props(data)
        CID = pug_access.get_cid(data)
        charge = pug_access.get_charge(data)
        props_3d = pug_access.pug_request_prop_3d(CID)
        exp_props = pug_access.pug_request_exp_prop(CID)
        uses = pug_access.pug_request_uses(CID)
        sh_props = pug_access.pug_request_sh_prop(CID)
    #    atom_id = pug_access.get_atoms(data)

        cid = CID['cid']
        uuid = create_uuid()
        insert_ontospecies_identifiers(uuid, identifiers)
        insert_ontospecies_props(uuid, props)
        insert_ontospecies_props(uuid, exp_props) 
        insert_ontospecies_props(uuid, sh_props)  
        insert_ontospecies_props(uuid, uses)     
        return (props, 'PubChem')

def populate_elements():
    pug_access = pug_api()
    for el in range(1,118):
        data = pug_access.pug_request_element(el)
        uuid = create_uuid()
        insert_ontospecies_element(uuid,data)

        
