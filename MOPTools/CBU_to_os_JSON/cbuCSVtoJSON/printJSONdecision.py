from kgoperations.querykg import querykg
from kgoperations.queryendpoints import SPARQL_ENDPOINTS
from kgoperations.querytemplate import spec_inchi_query

def printJSON(inchi_string):
    result_1  = querykg(SPARQL_ENDPOINTS['ontospecies'], spec_inchi_query(inchi_string)) #query_endpoint(endpoint, spec_inchi_query(inchi))
    inchi_string = inchi_string.replace("InChI=1S/", "InChI=1/")
    result_2  = querykg(SPARQL_ENDPOINTS['ontospecies'], spec_inchi_query(inchi_string)) #query_endpoint(endpoint, spec_inchi_query(inchi)) 
    decision = False
    if result_1:
        if 'speciesIRI' in result_1[0].keys():
            if result_1[0]['speciesIRI'] is not None: 
                pass
    if result_2:
        if 'speciesIRI' in result_2[0].keys():
            if result_2[0]['speciesIRI'] is not None: 
                pass
    else:
        decision = True
    return decision