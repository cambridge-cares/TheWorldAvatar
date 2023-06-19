'''
Define Util functions for building queries and read properties file
'''
# ===============================================================================
import os
from pathlib import Path

from configobj import ConfigObj

# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "resources", "timeseries.properties"))

# Initialise global variables to be read from properties file
global QUERY_ENDPOINT, UPDATE_ENDPOINT
global DB_URL, DB_USER, DB_PASSWORD

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    # Namespace for this example data
    'ts': 'https://www.theworldavatar.com/kg/ontotimeseries/',
    'ohn': 'https://www.theworldavatar.com/kg/ontoheatnetwork/',
    'om': 'http://www.ontology-of-units-of-measure.org/resource/om-2/',
    'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    'oems': 'https://www.theworldavatar.com/kg/ontoems/',
    'kg': 'https://www.theworldavatar.com/kg/pms_dh/',
    'ocape': 'http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#'

}

# Define time format for timeseries
FORMAT = '%Y-%m-%dT%H:%M:%SZ'

def read_properties_file(filepath):
    """
        Reads SPARQL endpoints and output directory from properties file (as global variables).
        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_URL, DB_USER, DB_PASSWORD

    # Read properties file
    props = ConfigObj(filepath)

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

    # Extract PostgreSQL database URL
    try:
        DB_URL = props['db.url']
    except KeyError:
        raise KeyError('Key "db.url" is missing in properties file: ' + filepath)
    if DB_URL == '':
        raise KeyError('No "db.url" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database username
    try:
        DB_USER = props['db.user']
    except KeyError:
        raise KeyError('Key "db.user" is missing in properties file: ' + filepath)
    if DB_USER == '':
        raise KeyError('No "db.user" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database password
    try:
        DB_PASSWORD = props['db.password']
    except KeyError:
        raise KeyError('Key "db.password" is missing in properties file: ' + filepath)
    if DB_PASSWORD == '':
        raise KeyError('No "db.password" value has been provided in properties file: ' + filepath)


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

# Run when module is imported
read_properties_file(PROPERTIES_FILE)
