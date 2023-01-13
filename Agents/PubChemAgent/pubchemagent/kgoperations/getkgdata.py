from pubchemagent.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pubchemagent.kgoperations.querykg import kg_operations
from pubchemagent.kgoperations.querytemplates import *


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
    # query = get_iri_query(inchi_string=inchi)
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

def get_provenance_data(prov_string):
    # query = get_iri_query(inchi_string=inchi)
    query = get_provenance_query(prov_string)
    sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    data = kg_client.querykg(queryStr=query)
    iri = None
    if data:
        data = data[0]
        iri = data['provenanceIRI']
    return iri

def get_unit_data(unit_string):
    # query = get_iri_query(inchi_string=inchi)
    query = get_unit_query(unit_string)
    sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    data = kg_client.querykg(queryStr=query)
    iri = None
    if data:
        data = data[0]
        iri = data['unitIRI']
    return iri

def get_uuid(typeIRI, string):
    # query = get_iri_query(inchi_string=inchi)
    query = generic_query(typeIRI, string)
    sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    data = kg_client.querykg(queryStr=query)
    iri = None
    if data:
        data = data[0]
        iri = data['IRI']
    return iri