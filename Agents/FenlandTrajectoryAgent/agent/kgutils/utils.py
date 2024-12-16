# The purpose of this module is to provide settings and functions relevant to
# both 1) instantiating and also 2) retrieving time series objects to/from KG
# ===============================================================================
import os
import re
import psycopg2
import requests
from pathlib import Path

from configobj import ConfigObj
from pathlib import Path
# Initialise logger
from twa import agentlogging
logger = agentlogging.get_logger("prod")

from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
# Define format of time series time entries: Year-Month-Day T hour:minute:second Z
FORMAT = '%Y-%m-%dT%H:%M:%SZ'

PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent.parent, 'resources', 'properties.properties'))
# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    # Namespace for this example data

    # Namespaces for used ontologies
    'om': 'http://www.ontology-of-units-of-measure.org/resource/om-2/',
    'ex': 'http://www.theworldavatar.com/kb/fenlandtrajectory/',
    'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    'rdfs': 'http://www.w3.org/2000/01/rdf-schema#',
    'ts': 'http://www.theworldavatar.com/kb/ontotimeseries/',
    'xsd': 'http://www.w3.org/2001/XMLSchema#',
    'geolit': 'http://www.bigdata.com/rdf/geospatial/literals/v1',
    'geo': 'http://www.bigdata.com/rdf/geospatial#',
    'ontodevice': 'https://www.theworldavatar.com/kg/ontodevice.owl/'
}


def validate_and_initialize_configs():
    """
    Validates the configurations for database and SPARQL endpoints received from stack_configs.
    """
    # Validate configurations received from stack_configs.py
    if not DB_URL or not DB_USER or not DB_PASSWORD:
        raise ValueError("Database configurations are incomplete.")
    if not SPARQL_QUERY_ENDPOINT or not SPARQL_UPDATE_ENDPOINT:
        raise ValueError("SPARQL endpoint configurations are incomplete.")

    print("Configurations validated successfully.")

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
        Creates PostgreSQL database with name as specified in db.url field in the stack_configs
        Please note: The PostgreSQL server is assumed to be available at the host and port specified in DB_URL
    """

    # Extract database name from DB URL provided in stack_configs
    db_name = DB_URL.split('/')[-1]

    # Create PostgreSQL database with extracted name
    conn = None
    try:
        # Connect to PostgreSQL server (using the host and port from DB_URL)
        conn = psycopg2.connect(user=DB_USER, password=DB_PASSWORD, host=DB_URL.split('@')[1].split(':')[0], port=DB_URL.split(':')[-1])
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


def create_blazegraph_namespace(endpoint=SPARQL_UPDATE_ENDPOINT,
                                quads=False, geospatial=False):
    """
    Creates Blazegraph namespace with name as specified in SPARQL update endpoint

    Arguments:
        quads - Boolean Flag whether quad/triple namespace shall be created
        geospatial - Boolean flag whether to enable geospatial capabilities
    """

    # Turns boolean flags for quads and geospatial into strings
    quads = str(quads).lower()
    geospatial = str(geospatial).lower()

    # Extracts Blazegraph REST API url from SPARQL endpoint
    url = endpoint[:endpoint.find('namespace') + len('namespace')]

    # Extracts name for new namespace from SPARQL endpoint
    ns = endpoint[endpoint.find('namespace') + len('namespace') + 1:]
    ns = ns[:ns.find('/')]

    # Defines POST request header and payload
    header = {'Content-type': 'text/plain'}

    payload = 'com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n' \
              'com.bigdata.rdf.sail.isolatableIndices=false\r\n' \
              'com.bigdata.rdf.sail.truthMaintenance=false\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n' \
              'com.bigdata.namespace.{}.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n' \
              'com.bigdata.rdf.sail.namespace={}\r\n' \
              f'com.bigdata.rdf.store.AbstractTripleStore.quads={quads}\r\n' \
              'com.bigdata.namespace.{}.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n' \
              f'com.bigdata.rdf.store.AbstractTripleStore.geoSpatial={geospatial}\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false'.format(ns, ns, ns)

    # Posts the request
    response = requests.post(url, payload, headers=header)

    if response.status_code == 201:
        logger.info('New namespace \"{}\" successfully created.\n'.format(ns))
    elif response.status_code == 409:
        logger.info('Namespace \"{}\" already exists\n'.format(ns))
    else:
        logger.info('Request status code: {}\n'.format(response.status_code))



