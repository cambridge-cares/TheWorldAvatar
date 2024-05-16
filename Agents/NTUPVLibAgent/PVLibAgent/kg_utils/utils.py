# The purpose of this module is to provide settings and functions relevant to
# both 1) instantiating and also 2) retrieving time series objects to/from KG
# ===============================================================================
import os

from configobj import ConfigObj
from pathlib import Path

from PVLibAgent.kg_utils.jpsSingletons import jpsBaseLibGW

# Initialise global variables to be read from properties file
global QUERY_ENDPOINT, UPDATE_ENDPOINT
global DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD
global IRI, AIR_TEMP_IRI, WIND_SPEED_IRI, IRRADIANCE_IRI


# Define format of time series time entries: Year-Month-Day T hour:minute:second Z
TIME_FORMAT = '%Y-%m-%dT%H:%M:%SZ'


# Create Java classes for all time series data
jpsBaseLibView = jpsBaseLibGW.createModuleView()
# Time entries (Instant)
Instant = jpsBaseLibView.java.time.Instant
TIMECLASS = Instant.now().getClass()

# Data class (i.e. all data as double)
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
# Namespaces for used ontologies
PREFIXES = {
        'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        'rdfs': 'http://www.w3.org/2000/01/rdf-schema#',
        'ts': 'https://www.theworldavatar.com/kg/ontotimeseries/',
        'xsd': 'http://www.w3.org/2001/XMLSchema#',
        'ontoems': 'https://www.theworldavatar.com/kg/ontoems/',
        'om': 'http://www.ontology-of-units-of-measure.org/resource/om-2/',
        'saref': 'https://saref.etsi.org/core/',
        'geo': 'https://www.w3.org/2003/01/geo/wgs84_pos#',
        's3n': 'https://w3id.org/s3n/',
        'powsys': 'http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#',
        'powreal': 'http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#',
        'ontocape': 'http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#'
}


# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "resources", "ts_client.properties"))


def read_properties_file(filepath):
    """
        Reads SPARQL endpoints and output directory from properties file (as global variables).

        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, DB_UPDATE_URL, DB_UPDATE_USER,\
        DB_UPDATE_PASSWORD, IRI, AIR_TEMP_IRI, WIND_SPEED_IRI, IRRADIANCE_IRI

    # Read properties file
    props = ConfigObj(filepath)

    # Extract SPARQL Query endpoint of KG
    try:
        QUERY_ENDPOINT = props['sparql.query.endpoint']
    except KeyError:
        raise KeyError('Key "sparql.query.endpoint" is missing in properties file: ' + filepath)
    if QUERY_ENDPOINT == '':
        raise KeyError('No "sparql.query.endpoint" value has been provided in properties file: ' + filepath)

    try:
        UPDATE_ENDPOINT = props['sparql.update.endpoint']
    except KeyError:
        raise KeyError('Key "sparql.update.endpoint" is missing in properties file: ' + filepath)
    if UPDATE_ENDPOINT == '':
        raise KeyError('No "sparql.update.endpoint" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database URL for query
    try:
        DB_QUERY_URL = props['db.query.url']
    except KeyError:
        raise KeyError('Key "db.query.url" is missing in properties file: ' + filepath)
    if DB_QUERY_URL == '':
        raise KeyError('No "db.query.url" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database username for query
    try:
        DB_QUERY_USER = props['db.query.user']
    except KeyError:
        raise KeyError('Key "db.query.user" is missing in properties file: ' + filepath)
    if DB_QUERY_USER == '':
        raise KeyError('No "db.query.user" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database password for query
    try:
        DB_QUERY_PASSWORD = props['db.query.password']
    except KeyError:
        raise KeyError('Key "db.query.password" is missing in properties file: ' + filepath)
    if DB_QUERY_PASSWORD == '':
        raise KeyError('No "db.query.password" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database URL for update
    try:
        DB_UPDATE_URL = props['db.update.url']
    except KeyError:
        raise KeyError('Key "db.update.url" is missing in properties file: ' + filepath)
    if DB_UPDATE_URL == '':
        raise KeyError('No "db.update.url" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database username for update
    try:
        DB_UPDATE_USER = props['db.update.user']
    except KeyError:
        raise KeyError('Key "db.update.user" is missing in properties file: ' + filepath)
    if DB_UPDATE_USER == '':
        raise KeyError('No "db.update.user" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database password for update
    try:
        DB_UPDATE_PASSWORD = props['db.update.password']
    except KeyError:
        raise KeyError('Key "db.update.password" is missing in properties file: ' + filepath)
    if DB_UPDATE_PASSWORD == '':
        raise KeyError('No "db.update.password" value has been provided in properties file: ' + filepath)

    # Extract IRI
    try:
        IRI = props['iri']
    except KeyError:
        raise KeyError('Key "iri" is missing in properties file: ' + filepath)

    # Extract air temperature IRI
    try:
        AIR_TEMP_IRI = props['air.temperature.iri']
    except KeyError:
        raise KeyError('Key "air.temperature.iri" is missing in properties file: ' + filepath)

    # Extract wind speed IRI
    try:
        WIND_SPEED_IRI = props['wind.speed.iri']
    except KeyError:
        raise KeyError('Key "wind.speed.iri" is missing in properties file: ' + filepath)

    # Extract Irradiance IRI
    try:
        IRRADIANCE_IRI = props['irradiance.iri']
    except KeyError:
        raise KeyError('Key "irradiance.iri" is missing in properties file: ' + filepath)


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
