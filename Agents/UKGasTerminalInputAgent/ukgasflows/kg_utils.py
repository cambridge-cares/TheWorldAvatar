# The purpose of this module is to provide settings and functions to interact with the
# knowledge graph, which are required by both terminal_update.py and output_flow_data.py
# ======================================================================================

import os
import json
from pathlib import Path
from configobj import ConfigObj

# get the jpsBaseLibGateWay instance from the jpsSingletons module
from ukgasflows.jpsSingletons import jpsBaseLibGW


# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "..", "resources", "timeseries.properties"))

# Initialise global variables to be read from properties file
global FALLBACK_KG, NAMESPACE, QUERY_ENDPOINT, UPDATE_ENDPOINT, OUTPUT_DIR

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
        Reads local KG location and namespace from properties file (as global variables).

        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global FALLBACK_KG, NAMESPACE, OUTPUT_DIR

    # Read properties file
    props = ConfigObj(filepath)

    # Extract local KG location and set as fallback
    try:
        FALLBACK_KG = props['fallback_kg']
    except KeyError:
        raise KeyError('Key "fallback_kg" is missing in properties file: ' + filepath)
    if FALLBACK_KG == '':
        raise KeyError('No "fallback_kg" value has been provided in properties file: ' + filepath)

    # Extract KG's SPARQL endpoint to use (namespace in Blazegraph)
    try:
        NAMESPACE = props['namespace']
    except KeyError:
        raise KeyError('Key "namespace" is missing in properties file: ' + filepath)
    if NAMESPACE == '':
        raise KeyError('No "namespace" value has been provided in properties file: ' + filepath)

    # Extract output directory for JSON file containing retrieved time series data from KG
    try:
        OUTPUT_DIR = props['output.directory']
    except KeyError:
        raise KeyError('Key "output.directory" is missing in properties file: ' + filepath)
    if OUTPUT_DIR == '':
        raise KeyError('No "output.directory" value has been provided in properties file: ' + filepath)


def setKGEndpoints(filepath):
    """
        Sets the correct URLs for KG's SPARQL Query and Update endpoints (as global variables).

        Arguments:
             filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global FALLBACK_KG, NAMESPACE, QUERY_ENDPOINT, UPDATE_ENDPOINT

    # Check for the KG_LOCATION environment variable, using local fallback
    kgRoot = os.getenv('KG_LOCATION', FALLBACK_KG)

    # Construct full URLs for SPARQL endpoints
    if kgRoot.endswith("/"):
        QUERY_ENDPOINT = kgRoot + "namespace/" + NAMESPACE + "/sparql"
        UPDATE_ENDPOINT = QUERY_ENDPOINT
    else:
        QUERY_ENDPOINT = kgRoot + "/namespace/" + NAMESPACE + "/sparql"
        UPDATE_ENDPOINT = QUERY_ENDPOINT

    # Write full SPARQL endpoints to properties file as required for Java TimeSeriesClient
    # Read properties file
    props = ConfigObj(filepath)
    # Write SPARQL endpoints to properties file (overwrite potentially existing entries)
    props['sparql.query.endpoint'] = QUERY_ENDPOINT
    props['sparql.update.endpoint'] = UPDATE_ENDPOINT
    props.write()


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

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
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
        Retrieves IRIs of all instantiated GasAmounts in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            List of Strings of all instantiated gas amount IRIs
            (empty list in case no gas amounts are instantiated)
    """

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
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
        Retrieves IRIs of all instantiated Quantities in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            List of Strings of all instantiated (unit of measure) quantity IRIs
            (empty list in case no quantities are instantiated)
    """

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
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
        Retrieves IRIs of all instantiated Measurements in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            List of Strings of all instantiated (unit of measure) measure IRIs
            (empty list in case no measures are instantiated)
    """

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('om') + \
            create_sparql_prefix('rdf') + \
            'SELECT ?a ' \
            'WHERE { ?a rdf:type om:Measure. }'
    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    # Extract list of IRI Strings from query results dictionary
    res = [list(r.values())[0] for r in response]

    return res


def check_timeseries_instantiation(endpoint, terminalIRI):
    """
        Check whether terminal already has an instantiated gas flow time series attached to it.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.
            terminalIRI - full gas terminal IRI incl. namespace (without trailing '<' or '>').

        Returns:
            True - if gas terminal is associated with a time series via the specified path.
            False - otherwise.
    """

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp') + \
            create_sparql_prefix('om') + \
            create_sparql_prefix('ts') + \
            'SELECT * ' \
            'WHERE { <' + terminalIRI + '> comp:hasTaken/^om:hasPhenomenon/om:hasValue/ts:hasTimeSeries ?a }'
    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    # Return True if any TimeSeries is associated with gas terminal via specified property path
    if len(response) > 0:
        return True
    else:
        return False


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

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp') + \
            create_sparql_prefix('om') + \
            create_sparql_prefix('ts') + \
            'SELECT ?' + var + ' '\
            'WHERE { <' + terminalIRI + '> comp:hasTaken/^om:hasPhenomenon/om:hasValue ?' + var + '. }'
    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    if len(response) == 0:
        return None
    else:
        return response[0][var]


def get_time_format(endpoint, terminalIRI):
    """
        Retrieves time format of time series entries stored in KG.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.
            terminalIRI - full gas terminal IRI incl. namespace (without trailing '<' or '>').

        Returns:
            Time series format as string.
    """

    # Initialise SPARQL query variable
    var = 'format'

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp') + \
            create_sparql_prefix('om') + \
            create_sparql_prefix('ts') + \
            'SELECT ?' + var + ' '\
            'WHERE { <' + terminalIRI + '> comp:hasTaken/^om:hasPhenomenon/om:hasValue/' \
                                          'ts:hasTimeSeries/ts:hasTimeUnit ?' + var + '. }'
    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    if len(response) == 0:
        return None
    else:
        return response[0][var]