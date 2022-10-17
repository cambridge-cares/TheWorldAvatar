from pubchem.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pubchem.kgoperations.querykg import kg_operations
from pubchem.kgoperations.querytemplates import ontospecies_data_query, spec_inchi_query, get_iri_query


def get_ontospecies_data(osIRI):
    
    query = ontospecies_data_query(osIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['copyontospecies']
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    data = kg_client.querykg(queryStr=query)


    if data:
        data = data[0]
    return data



def get_iri_data(inchi):
    query = get_iri_query(inchi_string=inchi)
    query = spec_inchi_query(inchi_string=inchi)
    sparqlendpoint = SPARQL_ENDPOINTS['copyontospecies']
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    data = kg_client.querykg(queryStr=query)
    iri = None
    if data:
        data = data[0]
        iri = data['speciesIRI']
    return iri