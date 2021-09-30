from pesfit.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pesfit.kgoperations.querykg import querykg
from pesfit.kgoperations.querytemplates import ontopesscan_data_query

def get_ontopesscan_data(opesIRI):
    pesscan_sparqltemplate = ontopesscan_data_query(opesIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['ontopesscan']
    data = querykg(sparqlEndPoint=sparqlendpoint, queryStr=pesscan_sparqltemplate)
    if data:
        data = data[0]
        
    return data