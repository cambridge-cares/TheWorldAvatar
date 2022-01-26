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
global OUTPUT_DIR, QUERY_ENDPOINT, NOOFBUILDINGS

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

def read_env_var(env_name):
    # Extract environmental value
    try:
        env_value = os.environ[env_name]
    except KeyError:
        errorstr = 'Key ' + env_name + ' is missing in environmental variable'
        raise KeyError(errorstr)
    if env_value == '':
        errorstr = 'No ' + env_name + ' value has been provided in environmental variable'
        raise KeyError(errorstr)
    return env_value

def read_properties_file(filepath):
    """
        Reads SPARQL endpoints and output directory from properties file (as global variables).
        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global OUTPUT_DIR, QUERY_ENDPOINT, NOOFBUILDINGS

    # Extract no. of building from environmental variables (# This is for testing purpose will be removed in production version)
    NOOFBUILDINGS = read_env_var('NoOfQueryBuildings')

    # Extract output directory for JSON file containing retrieved time series data from KG
    OUTPUT_DIR = read_env_var('OUTPUT_DIR')
    OUTPUT_DIR = os.path.abspath(os.path.join(filepath, '..', OUTPUT_DIR))

    # Extract environmental variable for KG
    KG_HOST = read_env_var('KG_HOST')
    KG_PATH = read_env_var('KG_PATH')
    KG_PROTOCOL = read_env_var('KG_PROTOCOL')
    
    # Check if the address contain port number:
    if 'KG_PORT' in os.environ and os.environ['KG_PORT'] != '':
        KG_PORT = os.environ['KG_PORT']
        QUERY_ENDPOINT = KG_PROTOCOL + '://' + KG_HOST + ':' + KG_PORT +  '/' + KG_PATH
    else:
        QUERY_ENDPOINT = KG_PROTOCOL + '://' + KG_HOST + '/' + KG_PATH

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