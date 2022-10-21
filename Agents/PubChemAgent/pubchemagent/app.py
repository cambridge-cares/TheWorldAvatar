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
        props = pug_access.get_props(data)
        CID = pug_access.get_cid(data)
        cid = CID['cid']
        insert_ontospecies_data(str(cid), props)      
        return (props, 'PubChem')