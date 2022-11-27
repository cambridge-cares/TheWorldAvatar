# This script provides functionality to instantiate the minimum required
# data for a flood warning required for the Flood assessment (derivation) agent

from iris import *
from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT

from pyderivationagent.kg_operations import PySparqlClient


def instantiate_flood_warning(severity_iri):
    # Instantiate minimum required markup for flood warning
    query = f"""
        INSERT DATA {{
            <{flood_warning_iri}> <{RDF_TYPE}> <{RT_FLOOD_ALERT_WARNING}> . 
            <{flood_warning_iri}> <{OF_HAS_SEVERITY}> <{severity_iri}> . 
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


if __name__ == '__main__':
    
    # Retrieve severity IRI (specified in OntoFlood ABox)
    sev_iri = OF_FLOOD_WARNING

    # Initialise KG client
    kg_client = PySparqlClient(query_endpoint=SPARQL_QUERY_ENDPOINT,
                               update_endpoint=SPARQL_UPDATE_ENDPOINT)

    # Instantiate flood warning
    query = instantiate_flood_warning(sev_iri)
    kg_client.performUpdate(query)
