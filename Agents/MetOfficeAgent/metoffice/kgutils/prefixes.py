###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 31 Mar 2022                           #
###############################################

# The purpose of this module is to provide a function which eases
# the inclusion of prefixes into SPARQL queries


# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    # External ontologies
    'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    'rdfs': 'http://www.w3.org/2000/01/rdf-schema#',
    'xsd': 'http://www.w3.org/2001/XMLSchema#',
    'geo': 'http://www.bigdata.com/rdf/geospatial#',
    'geolit': 'http://www.bigdata.com/rdf/geospatial/literals/v1#',
    'om' : 'http://www.ontology-of-units-of-measure.org/resource/om-2/',
    'owl': 'http://www.w3.org/2002/07/owl#',
    'm3l': 'http://purl.org/iot/vocab/m3-lite#',
    'weather': 'https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#',
    'sio': 'http://semanticscience.org/resource/',
    # CoMo / CARES ontologies
    'ems': 'http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl#',
    # TODO update IRI
    'ts': 'https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#',
    # Knowledge base
    'kb': 'http://www.theworldavatar.com/kb/ontoems/',
    'tsa': 'http://www.theworldavatar.com/kb/ontotimeseries/',
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
        raise KeyError('Prefix: "' + abbreviation + '" has not been specified')

    # Get full IRI from pre-specified prefixes dictionary
    iri = PREFIXES[abbreviation]

    if not iri.startswith('<'):
        iri = '<' + iri
    if not iri.endswith('>'):
        iri = iri + '>'

    return 'PREFIX ' + abbreviation + ': ' + iri + ' '
