from pubchem.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pubchem.kgoperations.querykg import kg_operations
from pubchem.kgoperations.querytemplates import test_data_insert
import uuid
   
# a sample data addition function
def insert_ontospecies_data(osIRI):
    
    insert_str = test_data_insert(osIRI)
    sparqlendpoint = SPARQL_ENDPOINTS['copyontospecies']
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    kg_client.insertkg(insertStr=insert_str)
 
# create a new UUID
def create_uuid():
    return str(uuid.uuid4())