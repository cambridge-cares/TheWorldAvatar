from pubchem.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pubchem.kgoperations.querykg import kg_operations
from pubchem.kgoperations.querytemplates import test_data_insert
   

def insert_ontospecies_data(osIRI):
    
    query = test_data_insert(osIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['copyontospecies']
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    kg_client.insertkg(insertStr=query)
