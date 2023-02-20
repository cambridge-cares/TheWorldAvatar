################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 27 Apr 2022                            #
################################################

# The purpose of this module is to provide functionality which eases handling
# of IRIs and PREFIXES while creating SPARQL queries

#import agentlogging
from airquality.datamodel.iris import *

# Initialise logger
#logger = agentlogging.get_logger("prod")


# Define PREFIXES for SPARQL queries
PREFIXES = {
    'rdf':  RDF,
    'rdfs': RDFS,
    'xsd':  XSD,
    'geo':  GEO,
    'geolit': GEOLIT,
    'om' :  OM,
    'owl':  OWL,
    'm3l':  M3L,
    'weather': WEATHER,
    'sio':  SIO,
    'ems':  EMS,
    'ts':   TS,
    'uom':  UOM,
    'kb':   KB
}


def create_sparql_prefix(abbreviation):
    """
        Constructs proper SPARQL Prefix String for given namespace abbreviation.
        
        Arguments:
            abbreviation - namespace abbreviation.
        Returns:
            SPARQL query prefix string in the form "PREFIX ns: <full IRI>".
    """

    # Define global scope for global variables
    global PREFIXES

    # Raise key error if given namespace abbreviation has not been specified
    if abbreviation not in PREFIXES.keys():
        #logger.error('Prefix: "' + abbreviation + '" has not been specified')
        raise KeyError('Prefix: "' + abbreviation + '" has not been specified')

    # Get full IRI from pre-specified prefixes dictionary
    iri = PREFIXES[abbreviation]

    if not iri.startswith('<'):
        iri = '<' + iri
    if not iri.endswith('>'):
        iri = iri + '>'

    return 'PREFIX ' + abbreviation + ': ' + iri + ' '
