from pubchemagent.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pubchemagent.kgoperations.querykg import kg_operations
from pubchemagent.kgoperations.querytemplates import pubchem_prop_insert
import uuid
   
# a sample data addition function
def insert_ontospecies_data(cid, props):
    uuid = create_uuid()
    insert_str = pubchem_prop_insert(uuid, cid, props)
    sparqlendpoint = SPARQL_ENDPOINTS['copyontospecies']
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    kg_client.insertkg(insertStr=insert_str)
 
# create a new UUID
def create_uuid():
    return str(uuid.uuid4())