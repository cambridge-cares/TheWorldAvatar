# The purpose of this module is to provide settings and functions relevant to
# both 1) instantiating and also 2) retrieving time series objects to/from KG
# ===============================================================================
import json
import os
from pathlib import Path
from configobj import ConfigObj


# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "resources", "ts_example.properties"))

# Initialise global variables to be read from properties file
global QUERY_ENDPOINT, UPDATE_ENDPOINT, OUTPUT_DIR, MAPBOX_APIKEY

# Define format of time series time entries: Year-Month-Day T hour:minute:second Z
FORMAT = '%Y-%m-%dT%H:%M:%SZ'

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    # Namespace for this example data
    'ex':  'http://www.theworldavatar.com/kb/ts_example/',
    'tsa': 'http://www.theworldavatar.com/kb/ontotimeseries/',
    # Namespaces for used ontologies
    'rdf':   'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    'rdfs':  'http://www.w3.org/2000/01/rdf-schema#',
    'ts':    'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#',
    'xsd':   'http://www.w3.org/2001/XMLSchema#',
    'geolit':   'http://www.bigdata.com/rdf/geospatial/literals/v1#',
    'geo':   'http://www.bigdata.com/rdf/geospatial#>'
}


def read_properties_file(filepath):
    """
        Reads SPARQL endpoints and output directory from properties file (as global variables).

        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global OUTPUT_DIR, QUERY_ENDPOINT, UPDATE_ENDPOINT, MAPBOX_APIKEY

    # Read properties file
    props = ConfigObj(filepath)

    # Extract output directory for JSON file containing retrieved time series data from KG
    try:
        OUTPUT_DIR = props['output.directory']
    except KeyError:
        raise KeyError('Key "output.directory" is missing in properties file: ' + filepath)
    if OUTPUT_DIR == '':
        raise KeyError('No "output.directory" value has been provided in properties file: ' + filepath)

    # Extract Mapbox API key (required for visualisation using DTVF)
    try:
        MAPBOX_APIKEY = props['mapbox.apiKey']
    except KeyError:
        raise KeyError('Key "mapbox.apiKey" is missing in properties file: ' + filepath)
    if MAPBOX_APIKEY == '':
        raise KeyError('No "mapbox.apiKey" value has been provided in properties file: ' + filepath)

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


def set_mapbox_apikey():
    """
        Populates Mapbox API key field in 'overall-meta.json' file within the visualisation data folder
        to API key provided in properties file
    """

    # Define global scope for global variables
    global MAPBOX_APIKEY, OUTPUT_DIR

    # Get filepath to "overall-meta.json" file in DTVF data folder
    fp = Path.joinpath(Path(OUTPUT_DIR).parent, 'overall-meta.json')

    # Read current overall meta data information
    with open(fp, 'r') as f:
        meta = json.load(f)

    # Update API key information with key from properties file
    meta['apiKey'] = MAPBOX_APIKEY

    # Write updated overall meta data information
    with open(fp, 'w') as f:
        json.dump(meta, f, indent=4)


# Run when module is imported
read_properties_file(PROPERTIES_FILE)
