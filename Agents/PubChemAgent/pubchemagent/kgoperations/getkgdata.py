from pubchemagent.kgoperations.querykg import kg_operations
from pubchemagent.kgoperations.querytemplates import *
import os
from pubchemagent.utils.default_configs import QUERY_ENDPOINT

from py4jps import agentlogging
logger = agentlogging.get_logger('prod')

def get_iri_data(inchi):
    """Return (iri, True) if found, else (None, False)."""

    kg_client = kg_operations(QUERY_ENDPOINT)

    # --- 1. Try original InChI ---
    query = get_iri_query(inchi)
    data = kg_client.querykg(queryStr=query)

    if data:
        iri = data[0]["speciesIRI"]
        return iri, True

    # --- 2. Try after replacing =1S/ with =1/ ---
    modified_inchi = inchi.replace("=1S/", "=1/")
    if modified_inchi != inchi:     # Only query again if the string changed
        query = get_iri_query(modified_inchi)
        data = kg_client.querykg(queryStr=query)

        if data:
            iri = data[0]["speciesIRI"]
            return iri, True

    # --- 3. Not found ---
    return None, False

def get_uuid(typeIRI, string):
    # query = get_iri_query(inchi_string=inchi)
    query = generic_query(typeIRI, string)
    sparqlendpoint = QUERY_ENDPOINT
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    data = kg_client.querykg(queryStr=query)
    iri = None
    if data:
        data = data[0]
        iri = data['IRI']
    return iri

def get_ref_uuid(ref_value, ref_unit_uuid):
    # query = get_iri_query(inchi_string=inchi)
    query = ref_state_query(ref_value, ref_unit_uuid)
    sparqlendpoint = QUERY_ENDPOINT
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    data = kg_client.querykg(queryStr=query)
    iri = None
    if data:
        data = data[0]
        iri = data['IRI']
    return iri

def get_element_IRI(symbol):
    # query = get_iri_query(inchi_string=inchi)
    query = element_query(symbol)
    sparqlendpoint = QUERY_ENDPOINT
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    data = kg_client.querykg(queryStr=query)
    iri = None
    if data:
        data = data[0]
        iri = data['IRI']
    return iri