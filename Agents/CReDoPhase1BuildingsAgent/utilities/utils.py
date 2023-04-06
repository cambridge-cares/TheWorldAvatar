# The purpose of this module is to provide settings and functions relevant to
# several modules of the KingsLynnAgent
# ===============================================================================
import os
import re

from configobj import ConfigObj
from pathlib import Path


# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent.parent, 'resources', 'properties.properties'))

# Initialise global variables to be read from properties file
global OUTPUT_DIR, QUERY_ENDPOINT, UPDATE_ENDPOINT, MAPBOX_APIKEY

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {'ocgml': 'http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#',
            'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
            'rdfs': 'http://www.w3.org/2000/01/rdf-schema#',
            'xsd': 'http://www.w3.org/2001/XMLSchema#',
            'geolit': 'http://www.bigdata.com/rdf/geospatial/literals/v1#',
            'geo': 'http://www.bigdata.com/rdf/geospatial#>'}

# Define full coordinate reference systems (CRS) for pyproj
CRSs = {'epsg_27700': 'urn:ogc:def:crs:EPSG::27700',
        'epsg_4326': 'urn:ogc:def:crs:EPSG::4326',
        'crs_84': 'urn:ogc:def:crs:OGC::CRS84',
        'crs_1.3_84': 'urn:ogc:def:crs:OGC:1.3:CRS84'}


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
        OUTPUT_DIR = os.path.abspath(os.path.join(filepath, '..', OUTPUT_DIR))
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


# Run when module is imported
read_properties_file(PROPERTIES_FILE)
