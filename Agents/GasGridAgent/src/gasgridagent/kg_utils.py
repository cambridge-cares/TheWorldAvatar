# The purpose of this module is to provide settings and functions to interact with the
# knowledge graph, which are required by both terminal_update.py and output_flow_data.py
# ======================================================================================

import os
import json
from pathlib import Path
from configobj import ConfigObj

# get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module
from gasgridagent.jpsSingletons import jpsBaseLibView


# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "..", "resources", "gasgridagent.properties"))

# Initialise global variables to be read from properties file
global QUERY_ENDPOINT, UPDATE_ENDPOINT, OUTPUT_DIR

# Define format of time series time entries: Year-Month-Day T hour:minute:second Z
FORMAT = '%Y-%m-%dT%H:%M:%SZ'

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    'comp':  'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#',
    'compa': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/',
    'om':    'http://www.ontology-of-units-of-measure.org/resource/om-2/',
    'rdf':   'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    'rdfs':  'http://www.w3.org/2000/01/rdf-schema#',
    'ts':    'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#',
    'xsd':   'http://www.w3.org/2001/XMLSchema#',
}


def read_properties_file(filepath):
    """
        Reads SPARQL endpoints and output directory from properties file (as global variables).

        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global OUTPUT_DIR, QUERY_ENDPOINT, UPDATE_ENDPOINT

    # Read properties file
    props = ConfigObj(filepath)

    # Extract output directory for JSON file containing retrieved time series data from KG
    try:
        OUTPUT_DIR = props['output.directory']
    except KeyError:
        raise KeyError('Key "output.directory" is missing in properties file: ' + filepath)
    if OUTPUT_DIR == '':
        raise KeyError('No "output.directory" value has been provided in properties file: ' + filepath)

    # Extract SPARQL Query endpoint of KG
    try:
        QUERY_ENDPOINT = props['sparql.query.endpoint']
    except KeyError:
        raise KeyError('Key "sparql.query.endpoint" is missing in properties file: ' + filepath)
    if QUERY_ENDPOINT == '':
        raise KeyError('No "sparql.query.endpoint" value has been provided in properties file: ' + filepath)

    # Extract SPARQL Update endpoint of KG
    try:
        UPDATE_ENDPOINT = props['sparql.update.endpoint']
    except KeyError:
        raise KeyError('Key "sparql.update.endpoint" is missing in properties file: ' + filepath)
    if UPDATE_ENDPOINT == '':
        raise KeyError('No "sparql.update.endpoint" value has been provided in properties file: ' + filepath)


def create_sparql_prefix(abbreviation):
    """
        Constructs proper SPARQL Prefix String for given namespace abbreviation.

        Arguments:
            abbreviation - namespace abbreviation to construct SPARQL PREFIX string for.

        Returns:
            SPARQL query prefix string in the form "PREFIX ns: <full IRI>".
    """

    # Define global scope for global variables
    global PREFIXES

    # Raise key error if given namespace abbreviation has not been specified
    if abbreviation not in PREFIXES.keys():
        raise KeyError('Prefix: "' + abbreviation + '" has not been specified')

    # Get full IRI from pre-specified prefixes dictionary
    iri = PREFIXES[abbreviation]

    if not iri.startswith('<'):
        iri = '<' + iri
    if not iri.endswith('>'):
        iri = iri + '>'

    return 'PREFIX ' + abbreviation + ': ' + iri + ' '


def get_instantiated_terminals(endpoint):
    """
        Retrieves names and IRIs of all instantiated GasTerminals in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            Dictionary of all instantiated gas terminals (name as key, IRI as value)
            (empty dictionary in case no terminals are instantiated)
    """

    # Initialise SPARQL query variables for gas terminal IRIs and names
    var1, var2 = 'iri', 'name'

    # Initialise remote KG client with only query endpoint specified
    print("Getting instantiated terminals from SPARQL endpoint:", endpoint)
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)
	
    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp') + \
            create_sparql_prefix('rdf') + \
            create_sparql_prefix('rdfs') + \
            'SELECT ?' + var1 + ' ?' + var2 + ' ' \
            'WHERE { ?' + var1 + ' rdf:type comp:GasTerminal; \
                                   rdfs:label ?' + var2 + '. }'

    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Create dictionary of query results with gas terminal name as key and IRI as value
    res = dict()
    for r in response:
        res[r[var2]] = r[var1]

    return res


def get_instantiated_gas_amounts(endpoint):
    """
        Retrieves IRIs of all instantiated GasAmounts (of type IntakenGas) in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            List of Strings of all instantiated gas amount IRIs
            (empty list in case no gas amounts are instantiated)
    """

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp') + \
            create_sparql_prefix('rdf') + \
            'SELECT ?a ' \
            'WHERE { ?a rdf:type comp:IntakenGas. }'

    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Extract list of IRI Strings from query results dictionary
    res = [list(r.values())[0] for r in response]

    return res


def get_instantiated_quantities(endpoint):
    """
        Retrieves IRIs of all instantiated Quantities (of type VolumetricFlowRate) in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            List of Strings of all instantiated (unit of measure) quantity IRIs
            (empty list in case no quantities are instantiated)
    """

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('om') + \
            create_sparql_prefix('rdf') + \
            'SELECT ?a ' \
            'WHERE { ?a rdf:type om:VolumetricFlowRate. }'

    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Extract list of IRI Strings from query results dictionary
    res = [list(r.values())[0] for r in response]

    return res


def get_instantiated_measurements(endpoint):
    """
        Retrieves IRIs of all instantiated Measurements (of type Measure) in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            List of Strings of all instantiated (unit of measure) measure IRIs
            (empty list in case no measures are instantiated)
    """

    # Initialise remote KG client with only query endpoint specified
    kgClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    query = create_sparql_prefix('om') + \
            create_sparql_prefix('rdf') + \
            'SELECT ?a ' \
            'WHERE { ?a rdf:type om:Measure. }'

    response = kgClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Extract list of IRI Strings from query results dictionary
    res = [list(r.values())[0] for r in response]
    return res


def get_measurementIRI(endpoint, terminalIRI):
    """
        Retrieves gas flow MeasurementIRI for terminal, which is actually connected to time series.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.
            terminalIRI - full gas terminal IRI incl. namespace (without trailing '<' or '>').

        Returns:
            Full gas flow MeasurementIRI incl. namespace (without trailing '<' or '>').
    """

    # Initialise SPARQL query variable
    var = 'iri'

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp') + \
            create_sparql_prefix('om') + \
            create_sparql_prefix('rdf') + \
            create_sparql_prefix('ts') + \
            '''SELECT ?%s \
            WHERE { <%s> comp:hasTaken ?gas . \
                    ?gas rdf:type comp:IntakenGas; \
                         ^om:hasPhenomenon/om:hasValue ?%s. \
                    ?%s ts:hasTimeSeries ?ts }''' % (var, terminalIRI, var, var)

    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    if len(response) == 0:
        return None
    elif len(response) > 1:
        raise ValueError('AMBIGUITY ERROR: Terminal connected to several gas flow time series!')
    else:
        return response[0][var]


def get_time_format(endpoint, terminalIRI):
    """
        Retrieves time format of gas flow time series entries stored in KG.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.
            terminalIRI - full gas terminal IRI incl. namespace (without trailing '<' or '>').

        Returns:
            Time series format as string.
    """

    # Initialise SPARQL query variable
    var = 'format'

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp') + \
            create_sparql_prefix('om') + \
            create_sparql_prefix('rdf') + \
            create_sparql_prefix('ts') + \
            '''SELECT ?%s \
            WHERE { <%s> comp:hasTaken ?gas . \
                    ?gas rdf:type comp:IntakenGas; \
                         ^om:hasPhenomenon/om:hasValue/ts:hasTimeSeries/ts:hasTimeUnit ?%s .}''' % \
            (var, terminalIRI, var)

    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    if len(response) == 0:
        return None
    else:
        return response[0][var]


# Run when module is imported
read_properties_file(PROPERTIES_FILE)
