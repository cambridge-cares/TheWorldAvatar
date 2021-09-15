from stdc.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from stdc.kgoperations.querykg import querykg
from stdc.kgoperations.querytemplates import ontocompchem_data_query, \
                                             ontospecies_data_query

def get_ontocompchem_data(ocIRI, osIRI):
    query = ontocompchem_data_query(ocIRI, osIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['ontocompchem']
    data = querykg(sparqlEndPoint=sparqlendpoint, queryStr=query)
    if data: data = data[0]
    if data['frequencies']:
        data['frequencies'] = ','.join(data['frequencies'].split())
    if data['rot_constants']:
        data['rot_constants'] = ','.join(data['rot_constants'].split())
    return data

def get_ontospecies_data(osIRI):
    query = ontospecies_data_query(osIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['ontospecies']
    data = querykg(sparqlEndPoint=sparqlendpoint, queryStr=query)
    if data: data = data[0]
    return data