from pubchemagent.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pubchemagent.kgoperations.querykg import kg_operations
from pubchemagent.kgoperations.querytemplates import *
import uuid
   
# a sample data addition function
def insert_ontospecies_identifiers(uuid, identifiers):
    for item in identifiers:
        insert_str = pubchem_identifiers_insert(uuid, identifiers[item])
        sparqlendpoint = SPARQL_ENDPOINTS['copyontospecies']
        # create a SPARQL object for performing the query
        kg_client = kg_operations(sparqlendpoint)
        kg_client.insertkg(insertStr=insert_str)

def insert_ontospecies_props(uuid, props):
    for item in props:
        insert_str = pubchem_prop_insert(uuid, props[item])
        sparqlendpoint = SPARQL_ENDPOINTS['copyontospecies']
        # create a SPARQL object for performing the query
        kg_client = kg_operations(sparqlendpoint)
        kg_client.insertkg(insertStr=insert_str)

def insert_ontospecies_elements(uuid, data):
    for item in data:
        insert_str = pubchem_elem_insert(uuid, data[item])
        sparqlendpoint = SPARQL_ENDPOINTS['copyontospecies']
        # create a SPARQL object for performing the query
        kg_client = kg_operations(sparqlendpoint)
        kg_client.insertkg(insertStr=insert_str)
 
# create a new UUID
def create_uuid():
    return str(uuid.uuid4())