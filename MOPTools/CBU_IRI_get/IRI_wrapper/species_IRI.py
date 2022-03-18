from kgoperations.querykg import querykg
from kgoperations.queryendpoints import SPARQL_ENDPOINTS
from kgoperations.querytemplate import spec_inchi_query

def run_iri_query(inchi):
    result  = querykg(SPARQL_ENDPOINTS['ontospecies'], spec_inchi_query(inchi)) #query_endpoint(endpoint, spec_inchi_query(inchi))
    if result:
        if 'speciesIRI' in result[0].keys():
            result =  result[0]['speciesIRI']
    return result