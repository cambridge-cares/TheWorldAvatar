##############################################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk), John Atherton (ja685@cam.ac.uk)   #
# Date: 03 July 2023                                                         #
##############################################################################

# The purpose of this module is to provide settings and functions to interact
# with the knowledge graph to retrieve already instantiated generators and
# their measurement IRIs

from py4jps import agentlogging
from agent.datamodel.iris import *
from agent.kgutils.kgclient import KGClient
from agent.utils.stack_configs import (QUERY_ENDPOINT, UPDATE_ENDPOINT)

# Initialises logger
logger = agentlogging.get_logger("prod")

def get_instantiated_generators(query_endpoint: str = QUERY_ENDPOINT,
                                  update_endpoint: str = UPDATE_ENDPOINT):
    """
        Retrieves names and IRIs of all instantiated generators in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            Dictionary of all instantiated gas generators (name as key, IRI as value)
            (empty dictionary in case no generators are instantiated)
    """

    # Initialises SPARQL query variables for gas generator IRIs and names
    var1, var2 = 'iri', 'name'

    # Initialises remote KG client with the endpoints specified
    logger.info("Initialises remote KG client with the endpoints specified: ", query_endpoint)
    kg_client = KGClient(query_endpoint, update_endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = ONTO_ENERGY_SYSTEM + \
            RDF + \
            RDFS + \
            'SELECT distinct ?' + var1 + ' ?' + var2 + ' ' \
            'WHERE { ?' + var1 + ' rdf:type ontoenergysystem:PowerGenerator; \
                                   rdfs:label ?' + var2 + '. }'

    # Executes query
    response = kg_client.performQuery(query)

    # Creates dictionary of query results with gas generator name as key and IRI as value
    res = dict()
    for r in response:
        res[r[var2]] = r[var1]

    return res


def get_instantiated_powerplants(query_endpoint: str = QUERY_ENDPOINT,
                                  update_endpoint: str = UPDATE_ENDPOINT):
    """
        Retrieves names and IRIs of all instantiated powerplants in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            Dictionary of all instantiated gas generators (name as key, IRI as value)
            (empty dictionary in case no generators are instantiated)
    """

    # Initialise SPARQL query variables for gas generator IRIs and names
    var1, var2 = 'iri', 'name'

    # Initialises remote KG client with the endpoints specified
    logger.info("Initialises remote KG client with the endpoints specified: ", query_endpoint)
    kg_client = KGClient(query_endpoint, update_endpoint)

    # Builds a SPARQL query to retrieve already instantiated powerplants
    query = ONTO_ENERGY_SYSTEM + \
            RDF + \
            RDFS + \
            'SELECT distinct ?' + var1 + ' ?' + var2 + ' ' \
            'WHERE { ?' + var1 + ' rdf:type ontoenergysystem:PowerPlant; \
                                   rdfs:label ?' + var2 + '. }'

    # Executes query
    response = kg_client.performQuery(query)

    # Create dictionary of query results with powerplant name as key and IRI as value
    res = dict()
    for r in response:
        res[r[var2]] = r[var1]
    return res

def get_measurementIRI(instance_IRI, query_endpoint: str = QUERY_ENDPOINT,
                                  update_endpoint: str = UPDATE_ENDPOINT):
    """
        Retrieves power MeasurementIRI for generator, which is actually connected to time series.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.
            generatorIRI - full generator IRI incl. namespace (without trailing '<' or '>').

        Returns:
            Full power MeasurementIRI incl. namespace (without trailing '<' or '>').
    """

    # Initialise SPARQL query variable
    var = 'iri'

    # Initialises remote KG client with the endpoints specified
    logger.info("Initialises remote KG client to query the measurement IRI of the instance: ", instance_IRI)
    kg_client = KGClient(query_endpoint, update_endpoint)

    # Builds a SPARQL query to retrieve already instantiated measurement IRIs
    query = ONTO_POW_SYS + \
            ONTO_ENERGY_SYSTEM + \
            OM + \
            TS + \
            RDF + \
            '''SELECT ?%s \
            WHERE { <%s> ontopowsys:hasActivePowerGenerated ?%s . \
                    ?%s rdf:type ontopowsys:GeneratedActivePower ; \
                        ts:hasTimeSeries ?ts }''' % (var, instance_IRI, var, var)

    # Executes query
    response = kg_client.performQuery(query)

    if len(response) == 0:
        return None
    elif len(response) > 1:
        raise ValueError('AMBIGUITY ERROR: generator connected to several time series!')
    else:
        return response[0][var]
