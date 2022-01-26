# The purpose of this module is to provide settings and functions relevant to
# several modules of the KingsLynnAgent for container workflow
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


def read_properties_file(filepath):
    """
        Reads SPARQL endpoints and output directory from properties file (as global variables).
        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global OUTPUT_DIR, QUERY_ENDPOINT, NOOFBUILDINGS

    # Read properties file
    props = ConfigObj(filepath)

    # Extract no. of building from environmental variables (# This is for testing purpose will be removed in production version)
    try:
        NOOFBUILDINGS = os.environ['NoOfQueryBuildings']
    except KeyError:
        raise KeyError('Key "NoOfQueryBuildings" is missing in environmental variable')
    if NOOFBUILDINGS == '':
        raise KeyError('No "NoOfQueryBuildings" value has been provided in environmental variable')

    # Extract output directory for JSON file containing retrieved time series data from KG
    try:
        OUTPUT_DIR = os.environ['OUTPUT_DIR']
        OUTPUT_DIR = os.path.abspath(os.path.join(filepath, '..', OUTPUT_DIR))
    except KeyError:
        raise KeyError('Key "OUTPUT_DIR" is missing in environmental variable')
    if OUTPUT_DIR == '':
        raise KeyError('No "OUTPUT_DIR" value has been provided in properties file')

    # Extract SPARQL BLAZEGRAPH HOST of KG
    try:
        BLAZEGRAPH_HOST = os.environ['BLAZEGRAPH_HOST']
    except KeyError:
        raise KeyError('Key "BLAZEGRAPH_HOST" is missing in environmental variable')
    if BLAZEGRAPH_HOST == '':
        raise KeyError('No "BLAZEGRAPH_HOST" value has been provided in environmental variable')

    # Extract SPARQL NAMESPACE of KG
    try:
        NAMESPACE = os.environ['NAMESPACE']
    except KeyError:
        raise KeyError('Key "NAMESPACE" is missing in environmental variable')
    if NAMESPACE == '':
        raise KeyError('No "NAMESPACE" value has been provided in environmental variable')

    # Construct the query end point
    try:
        QUERY_ENDPOINT = 'https://' + BLAZEGRAPH_HOST + '/namespace/' + NAMESPACE + '/sparql'
    except KeyError:
        raise KeyError('Error in constructing the query endpoint')
    if QUERY_ENDPOINT == '':
        raise KeyError('No query endpoint specified')

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