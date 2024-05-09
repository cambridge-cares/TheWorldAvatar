# The purpose of this module is to provide settings and functions to interact with the
# knowledge graph, which are required by both generator_update.py and output_flow_data.py
# ======================================================================================

import os
import re
import json
import psycopg2
import requests
from pathlib import Path
from configobj import ConfigObj

# get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module
from jpsSingletons import jpsBaseLibView
# get the BMRS API Dataframe generating and CSV writing script. 
#from ScriptMapQuery import BMRS_API_Input_JA_7 as bmrs


# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, ".", "resources", "ukexportcurtailmentimporter.properties"))

# Initialise global variables to be read from properties file
global QUERY_ENDPOINT, UPDATE_ENDPOINT, OUTPUT_DIR
global MAPBOX_APIKEY
global DB_URL, DB_USER, DB_PASSWORD

# Define format of time series time entries: Year-Month-Day T hour:minute:second Z
FORMAT = '%Y-%m-%dT%H:%M:%SZ'

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    'ontoenergysystem':  'http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#',
    'ontoenergysystem_kb': 'http://www.theworldavatar.com/kb/ontoenergysystem/',
    'ontopowsys':  'http://www.theworldavatar.com/ontology/ontopowsys/electrical_system.owl#',
    'om':    'http://www.ontology-of-units-of-measure.org/resource/om-2/',
    'rdf':   'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    'rdfs':  'http://www.w3.org/2000/01/rdf-schema#',
    'ts':    'https://www.theworldavatar.com/kg/ontotimeseries/',
    'xsd':   'http://www.w3.org/2001/XMLSchema#',
    'ontoeip':   'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#',
}


def read_properties_file(filepath):
    """
        Reads SPARQL endpoints and output directory from properties file (as global variables).

        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global OUTPUT_DIR, QUERY_ENDPOINT, UPDATE_ENDPOINT, MAPBOX_APIKEY, DB_URL, DB_USER, DB_PASSWORD
	
    # Read properties file
    props = ConfigObj(filepath)

    # Read output directory for JSON file containing retrieved time series data from KG
    try:
        OUTPUT_DIR = props['output.directory']
    except KeyError:
        raise KeyError('Key "output.directory" is missing in properties file: ' + filepath)
    if OUTPUT_DIR == '':
        raise KeyError('No "output.directory" value has been provided in properties file: ' + filepath)

    # Read SPARQL Query endpoint of KG
    try:
        QUERY_ENDPOINT = props['sparql.query.endpoint']
    except KeyError:
        raise KeyError('Key "sparql.query.endpoint" is missing in properties file: ' + filepath)
    if QUERY_ENDPOINT == '':
        raise KeyError('No "sparql.query.endpoint" value has been provided in properties file: ' + filepath)

    # Read SPARQL Update endpoint of KG
    try:
        UPDATE_ENDPOINT = props['sparql.update.endpoint']
    except KeyError:
        raise KeyError('Key "sparql.update.endpoint" is missing in properties file: ' + filepath)
    if UPDATE_ENDPOINT == '':
        raise KeyError('No "sparql.update.endpoint" value has been provided in properties file: ' + filepath)

    # Read Mapbox API key (required for visualisation using DTVF)
    try:
        MAPBOX_APIKEY = props['mapbox.apiKey']
    except KeyError:
        raise KeyError('Key "mapbox.apiKey" is missing in properties file: ' + filepath)
    if MAPBOX_APIKEY == '':
        raise KeyError('No "mapbox.apiKey" value has been provided in properties file: ' + filepath)

    # Read PostgreSQL database URL
    try:
        DB_URL = props['db.url']
    except KeyError:
        raise KeyError('Key "db.url" is missing in properties file: ' + filepath)
    if DB_URL == '':
        raise KeyError('No "db.url" value has been provided in properties file: ' + filepath)

    # Read PostgreSQL database username
    try:
        DB_USER = props['db.user']
    except KeyError:
        raise KeyError('Key "db.user" is missing in properties file: ' + filepath)
    if DB_USER == '':
        raise KeyError('No "db.user" value has been provided in properties file: ' + filepath)

    # Read PostgreSQL database password
    try:
        DB_PASSWORD = props['db.password']
    except KeyError:
        raise KeyError('Key "db.password" is missing in properties file: '+ filepath)
    if DB_PASSWORD == '':
        raise KeyError('No "db.user" value has been provided in properties file: ' + filepath)

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

def set_mapbox_apikey():
    """
        Populates Mapbox API key field in 'index.html' file within the dtvf_visualisation folder
        with API key provided in properties file
    """

    # Define global scope for global variables
    global MAPBOX_APIKEY, OUTPUT_DIR

    # Get filepath to "index.html" file in DTVF folder
    fp = Path.joinpath(Path(OUTPUT_DIR).parent, 'index.html')

    # Read current "index.html" file
    with open(fp, 'r') as f:
        old_html = f.read()

    # Find old API key
    match = re.search(r'mapboxAPI\s*=\s*".*"', old_html)
    old_key = match.group()
    # Replace old/default API key with actual one
    new_key = 'mapboxAPI = "{}"'.format(MAPBOX_APIKEY)
    new_html = old_html.replace(old_key, new_key)

    # Write updated "index.html" file
    with open(fp, 'w') as f:
        f.write(new_html)

def create_postgres_db():
    """
        Creates PostgreSQL database with name as specified in db.url field in the properties file
        Please note: The PostgreSQL server is assumed to be available at DEFAULT HOST (i.e. localhost)
        and PORT (i.e. 5432)
    """

    # Extract database name from DB URL provided in properties file
    # (for details see: https://www.postgresql.org/docs/7.4/jdbc-use.html)
    db_name = DB_URL.split(':')[-1]

    # Create PostgreSQL database with extracted name
    # (for details see: https://www.psycopg.org/docs/module.html)
    conn = None
    try:
        # Connect to PostgreSQL server (via DEFAULT host and port)
        conn = psycopg2.connect(user=DB_USER, password=DB_PASSWORD, host='localhost')
        conn.autocommit = True
        # Create cursor object
        cur = conn.cursor()
        # Create db table
        cur.execute('CREATE DATABASE ' + db_name)
        # Close communication with the PostgreSQL database server
        cur.close()
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    finally:
        if conn is not None:
            conn.close()


def create_blazegraph_namespace():
    """
        Creates Blazegraph namespace with name as specified in SPARQL endpoints
    """

    # Extract Blazegraph REST API url from SPARQL endpoint
    url = UPDATE_ENDPOINT[:UPDATE_ENDPOINT.find('namespace') + len('namespace')]

    # Extract name for new namespace from SPARQL endpoint
    ns = UPDATE_ENDPOINT[UPDATE_ENDPOINT.find('namespace') + len('namespace') + 1:]
    ns = ns[:ns.find('/')]

    # Define POST request header and payload
    header = {'Content-type': 'text/plain'}

    payload = 'com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n' \
              'com.bigdata.rdf.sail.isolatableIndices=false\r\n' \
              'com.bigdata.rdf.sail.truthMaintenance=false\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n' \
              'com.bigdata.namespace.{}.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n' \
              'com.bigdata.rdf.sail.namespace={}\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.quads=false\r\n' \
              'com.bigdata.namespace.{}.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.geoSpatial=true\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false'.format(ns, ns, ns)

    # Post the request
    response = requests.post(url, payload, headers=header)

    if response.status_code == 201:
        print('New namespace \"{}\" successfully created.\n'.format(ns))
    elif response.status_code == 409:
        print('Namespace \"{}\" already exists\n'.format(ns))
    else:
        print('Request status code: {}\n'.format(response.status_code))


def get_instantiated_generators(endpoint):
    """
        Retrieves names and IRIs of all instantiated generators in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            Dictionary of all instantiated gas generators (name as key, IRI as value)
            (empty dictionary in case no generators are instantiated)
    """

    # Initialise SPARQL query variables for gas generator IRIs and names
    var1, var2 = 'iri', 'name'

    # Initialise remote KG client with only query endpoint specified
    print("Getting instantiated generators from SPARQL endpoint:", endpoint)
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('ontoenergysystem') + \
            create_sparql_prefix('rdf') + \
            create_sparql_prefix('rdfs') + \
            'SELECT distinct ?' + var1 + ' ?' + var2 + ' ' \
            'WHERE { ?' + var1 + ' rdf:type ontoenergysystem:PowerGenerator; \
                                   rdfs:label ?' + var2 + '. }'

    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Create dictionary of query results with gas generator name as key and IRI as value
    res = dict()
    for r in response:
        res[r[var2]] = r[var1]

    return res

def get_generic_instances(endpoint):
    """
        Retrieves names and IRIs of all instantiated powerplants in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            Dictionary of all instantiated gas generators (name as key, IRI as value)
            (empty dictionary in case no generators are instantiated)
    """

    # Initialise SPARQL query variables for gas generator IRIs and names
    var1 = 'iri'

    # Initialise remote KG client with only query endpoint specified
    print("Getting instantiated powerplants from SPARQL endpoint:", endpoint)
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('ontoenergysystem') + \
            create_sparql_prefix('rdf') + \
            create_sparql_prefix('rdfs') + \
            'SELECT distinct ?' + var1 + ' ' \
            'WHERE { ?' + var1 + ' ?y ?z . } LIMIT 10'

    print("Query:", query)
    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    return response

def get_instantiated_powerplants(endpoint):
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

    # Initialise remote KG client with only query endpoint specified
    print("Getting instantiated powerplants from SPARQL endpoint:", endpoint)
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('ontoenergysystem') + \
            create_sparql_prefix('rdf') + \
            create_sparql_prefix('rdfs') + \
            'SELECT distinct ?' + var1 + ' ?' + var2 + ' ' \
            'WHERE { ?' + var1 + ' rdf:type ontoenergysystem:PowerPlant; \
                                   rdfs:label ?' + var2 + '. }'

    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    # Create dictionary of query results with powerplant name as key and IRI as value
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
            'SELECT distinct ?a ' \
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
            'SELECT distinct ?a ' \
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
            'SELECT distinct ?a ' \
            'WHERE { ?a rdf:type om:Measure. }'

    response = kgClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Extract list of IRI Strings from query results dictionary
    res = [list(r.values())[0] for r in response]
    return res


def get_measurementIRI(endpoint, instance_IRI):
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

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('ontopowsys') + \
            create_sparql_prefix('ontoenergysystem') + \
            create_sparql_prefix('om') + \
            create_sparql_prefix('ts') + \
            create_sparql_prefix('rdf') + \
            '''SELECT ?%s \
            WHERE { <%s> ontopowsys:hasActivePowerGenerated ?%s . \
                    ?%s rdf:type ontoenergysystem:ExportedActivePower ; \
                        ts:hasTimeSeries ?ts } LIMIT 1''' % (var, instance_IRI, var, var)

    response = KGClient.execute(query)
    print("Query Line Print: ", query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    if len(response) == 0:
        return None
    elif len(response) > 1:
        raise ValueError('AMBIGUITY ERROR: generator connected to several gas flow time series!')
    else:
        return response[0][var]


def get_curtailment_measurementIRI(endpoint, instance_IRI):
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

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('ontopowsys') + \
            create_sparql_prefix('ontoenergysystem') + \
            create_sparql_prefix('om') + \
            create_sparql_prefix('ts') + \
            create_sparql_prefix('rdf') + \
            '''SELECT ?%s \
            WHERE { <%s> ontopowsys:hasActivePowerGenerated ?%s . \
                    ?%s rdf:type ontoenergysystem:CurtailedActiveEnergy ; \
                        ts:hasTimeSeries ?ts } LIMIT 1''' % (var, instance_IRI, var, var)

    response = KGClient.execute(query)
    print("Query Line Print: ", query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    if len(response) == 0:
        return None
    elif len(response) > 1:
        raise ValueError('AMBIGUITY ERROR: generator connected to several gas flow time series!')
    else:
        return response[0][var]


def get_EIC(endpoint, instance_IRI):
    """
        Retrieves the Energy Identification Code (EIC) of an entity or instance related to
        gas or electricity sector, e.g. power plant.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.
            instance_IRI - full IRI of the provided instance.

        Returns:
            the EIC of the instance.
    """

    # Initialise SPARQL query variable
    var = 'EIC'

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(endpoint)

    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('ontoenergysystem') + \
            '''SELECT ?%s \
            WHERE { <%s> ontoenergysystem:hasEIC ?%s }''' % (var, instance_IRI, var)

    response = KGClient.execute(query)
    print("Query Line Print: ", query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    if len(response) == 0:
        return None
    elif len(response) > 1:
        raise ValueError('AMBIGUITY ERROR: generator connected to several gas flow time series!')
    else:
        return response[0][var]


def get_time_format(endpoint, generatorIRI):
    """
        Retrieves time format of gas flow time series entries stored in KG.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.
            generatorIRI - full gas generator IRI incl. namespace (without trailing '<' or '>').

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
            (var, generatorIRI, var)

    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    if len(response) == 0:
        return None
    else:
        return response[0][var]


# Run when module is imported
read_properties_file(PROPERTIES_FILE)
