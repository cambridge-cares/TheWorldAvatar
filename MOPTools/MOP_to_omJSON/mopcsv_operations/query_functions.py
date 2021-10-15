from MOP_to_omJSON.kgoperations.querykg import querykg
from MOP_to_omJSON.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from MOP_to_omJSON.kgoperations.querytemplate import inchi_query, mop_query

def run_iri_query(inchi):
    result  = querykg(SPARQL_ENDPOINTS['ontospecies'], inchi_query(inchi)) #query_endpoint(endpoint, spec_inchi_query(inchi))
    if result:
        if 'speciesIRI' in result[0].keys():
            result =  result[0]['speciesIRI']
    return result

def printJSON(mop_weight, mop_shape, mop_charge):
    result  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_query(mop_weight, mop_shape, mop_charge))
    decision = False
    if result:
        if 'MOP_Weight' in result[0].keys():
            if result[0]['MOP_Weight'] is not None: 
                pass
    else:
        decision = True
    return decision

