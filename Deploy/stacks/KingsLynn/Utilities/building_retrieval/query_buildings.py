################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 02 Sep 2022                            #
################################################

# This module retrieves building geometries from the KG and creates .geojson files
# for visualisation. Primarily to check whether buildings with and without UPRN
# data added by the UPRN agent differ in some relevant aspect which justifies missing
# UPRN information

from SPARQLWrapper import SPARQLWrapper, JSON, SPARQLExceptions
import pyproj

import geojson_formatter

# Specify (local) Blazegraph properties
query_endpoint = 'http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql'
graph_prefix='http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/'

# Specify number of buildings to retrieve (set to None in order to retrieve ALL buildings)
n = None

# Specify whether UPRN info shall be missing or not
missing_uprn = False

# Specify output coordinate reference system (CRS)
# Our Mapbox plotting framework uses EPSG:4326 (https://epsg.io/4326)
target_crs = 'urn:ogc:def:crs:EPSG::4326'

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    'ocgl': 'http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#',
    'xsd': 'http://www.w3.org/2001/XMLSchema#',
    'osid': 'http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoOSID.owl#'
}


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


def get_buildings(number=None, without_uprns=None, graph=graph_prefix):
    """
        Create SPARQL query to retrieve buildings and associated (surface) geometries

        Arguments:
            number - Number of buildings to retrieve data for

        Returns:
            SPARQL query to pass to Blazegraph
    """

    # Create subquery to limit number of buildings for which to retrieve surface geometries
    if number:
        limit = f'LIMIT {number}'
    else:
        limit = ''
    
    # Create subquery for UPRNs
    if isinstance(without_uprns, bool):
        if without_uprns:
            subquery='FILTER NOT EXISTS { ?cityobj ^osid:intersectsFeature/osid:hasValue ?uprn }'
        else:
            subquery='?cityobj ^osid:intersectsFeature ?uprn '
    else:
        subquery=''

    # Construct query
    # Consider only surface polygons with provided geometries/polygons (some only refer to "told blank nodes")
    query = create_sparql_prefix('ocgl') + \
            create_sparql_prefix('xsd') + \
            create_sparql_prefix('osid') + \
            f'''SELECT ?bldg ?cityobj ?surf ?geom ?uprn \
               WHERE {{ \
                  GRAPH <{graph}building/> \
                    {{ ?bldg ocgl:objectClassId 26 . \
                       BIND(IRI(REPLACE(str(?bldg), "building", "cityobject")) AS ?cityobj) \
                    }} \
                    ?bldg ocgl:lod0FootprintId ?surf . \
                    ?surf ^ocgl:parentId/ocgl:GeometryType ?geom . ''' + \
                    subquery + \
               '''} \
               ORDER BY ?bldg ''' + \
            limit

    return query


def get_crs():
    """
        Create SPARQL query to retrieve coordinate reference system of triple store

        Returns:
            SPARQL query to pass to Blazegraph
    """

    # Construct query
    query = create_sparql_prefix('ocgl') + \
            '''SELECT ?crs \
               WHERE { ?s ocgl:srsname ?crs . }'''

    return query


def execute_query(query, query_endpoint):
    '''
        Executes provided SPARQL query and returns results in JSON format

        Arguments:
            query - SPARQL query to execute.
            query_endpoint - SPARQl endpoint to execute query on.

        Returns:
            SPARQL query results in JSON format.
    '''

    # Initialise SPARQL wrapper and set endpoint and return format
    sparql = SPARQLWrapper(query_endpoint)
    sparql.setReturnFormat(JSON)

    # Set query and execute
    sparql.setQuery(query)
    results = sparql.query().convert()

    return results


def get_coordinates(polygon_data, input_crs, target_crs):
    '''
        Extracts and transforms polygon coordinates as retrieved from Blazegraph
        to suit GeoJSON polygon requirements (and target CRS)

        Arguments:
            polygon_data - polygon data as returned from Blazegraph
            input_crs - CRS of input data (as pyproj CRS object)
            target_crs - CRS of output data (as pyproj CRS object)

        Returns:
            List of polygon coordinates as required for GeoJSON objects
            Maximum height of polygon surface
    '''

    # Initialise CRS transformer
    proj = pyproj.Transformer.from_crs(input_crs, target_crs, always_xy=True)

    # Initialise output and input coordinate collections
    coordinates = []
    coordinate_str = polygon_data.split("#")
    coordinate_str = [float(c) for c in coordinate_str]

    # If all input coordinates have proper (X,Y,Z) values ...
    if len(coordinate_str) % 3 == 0:

        # Initialise maximum height of polygon
        zmax = 0.0

        # Iterate through all polygon points
        nodes = int(len(coordinate_str) / 3)
        for i in range(nodes):
            node = i * 3
            # Transform (x,y) values and append (x,y,z) to output list
            x, y = proj.transform(coordinate_str[node], coordinate_str[node + 1])
            z = coordinate_str[node + 2]
            coordinates.append([x, y, z])

            # Update max height
            if z > zmax:
                zmax = z

        # Check if first and last polygon vertices are equivalent and fix if necessary
        if coordinates[0] != coordinates[-1]:
            print('Surface polygon did not close properly! Geometry has been fixed.')
            coordinates.append(coordinates[0])

    # ... otherwise print error
    else:
        print('Erroneous polygon coordinates:', coordinate_str)

    return coordinates, zmax


if __name__ == '__main__':

    # Retrieve SPARQL results from Blazegraph
    try:
        kg_buildings = execute_query(get_buildings(n, missing_uprn, graph_prefix), 
                                     query_endpoint)
        kg_crs = execute_query(get_crs(), query_endpoint)
    except SPARQLExceptions.EndPointNotFound as e:
        print('\nERROR: SPARQL query endpoint not found! Please ensure correct namespace and reachable triple store.\n')
        raise e

    # Unpack CRS SPARQL result to extract coordinate reference system
    try:
        crs = kg_crs['results']['bindings'][0][kg_crs['head']['vars'][0]]['value']
    except IndexError as e:
        print('\nERROR: No CRS could be retrieved from specified triple store namespace.\n')
        raise Exception('No CRS could be retrieved from specified triple store namespace.')

    # Unpack buildings SPARQL results into dictionary in format {surface_IRI: [building_IRI, polygon_data]}
    footprints = {}
    for s in kg_buildings["results"]["bindings"]:
        footprints[s['surf']['value']] = [s['bldg']['value'], s["geom"]["value"]]

    # Initialise pyproj CRS objects
    crs_in = pyproj.CRS.from_string(crs)
    crs_out = pyproj.CRS.from_string(target_crs)

    # Start GeoJSON output file
    output = geojson_formatter.start_output(target_crs)

    # Iterate through all geospatial features
    features = list(footprints.keys())
    total_features = len(features)
    for i in range(total_features):

        # Extract and transform coordinates from String polygon returned by SPARQL query
        feature = str(features[i])
        coords, zmax = get_coordinates(footprints[feature][1], crs_in, crs_out)

        # Specify feature properties to consider (beyond coordinates)
        props = {
            'building': footprints[feature][0],
            'max_height': round(zmax, 3)
        }

        if i == total_features - 1:
            body = geojson_formatter.write_feature(feature, props, coords, 1)
        else:
            body = geojson_formatter.write_feature(feature, props, coords, 0)

        # Append feature data to GeoJSON output
        output += body

    # Finalise GeoJSON output
    output += geojson_formatter.end_output()

    # Write output to file
    postfix = 'without_UPRNs' if missing_uprn else 'with_UPRNs'
    file_name = f'data/outputs/Buildings_{postfix}.geojson'
    with open(file_name, 'w') as f:
        f.write(output)
