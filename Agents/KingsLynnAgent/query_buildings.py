import json

from SPARQLWrapper import SPARQLWrapper, JSON, SPARQLExceptions
from geojson_rewind import rewind
import pyproj

import geojson_formatter

# Specify (local) Blazegraph properties
server = "localhost"
port = "9999"
namespace = "kings-lynn"

# Specify number of buildings to retrieve (set to None in order to retrieve ALL buildings)
n = 10

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    'ocgl': 'http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#',
    'xsd': 'http://www.w3.org/2001/XMLSchema#'
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


def get_buildings(number=None):
    """
        Create SPARQL query to retrieve buildings and associated (surface) geometries

        Arguments:
            number - Number of buildings to retrieve data for

        Returns:
            SPARQL query to pass to Blazegraph
    """

    # Create subquery to limit number of buildings for which to retrieve surface geometries
    if number:
        subquery = '''{ SELECT distinct ?bldg \
                        WHERE { ?surf ocgl:cityObjectId ?bldg ; \
                                      ocgl:GeometryType ?geom . } \
                        LIMIT %i \
                        }''' % number
    else:
        subquery = ''

    # Construct query
    # Consider only surface polygons with provided geometries/polygons (some only refer to "told blank nodes")
    query = create_sparql_prefix('ocgl') + \
            create_sparql_prefix('xsd') + \
            '''SELECT ?bldg ?surf ?geom \
               WHERE { ?surf ocgl:cityObjectId ?bldg ; \
       		                 ocgl:GeometryType ?geom . \
       		   FILTER (!isBlank(?geom)) ''' + \
            subquery + \
       		'''} \
       		   ORDER BY ?bldg'''

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

    # Construct full SPARQL endpoint URL
    query_endpoint = "http://" + server + ':' + port + '/blazegraph/namespace/' + namespace + "/sparql"

    # Retrieve SPARQL results from Blazegraph
    try:
        kg_buildings = execute_query(get_buildings(n), query_endpoint)
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
    surfaces = {}
    for s in kg_buildings["results"]["bindings"]:
        surfaces[s['surf']['value']] = [s['bldg']['value'], s["geom"]["value"]]

    # Initialise pyproj coordinate reference system (CRS) objects
    crs_in = pyproj.CRS.from_string(crs)
    # Specify output CRS: Both GeoJSON and the TWA Mapbox plotting framework require "OGC::CRS84", see
    # GeoJSON: https://datatracker.ietf.org/doc/html/rfc7946#section-4
    # Mapbox: https://docs.mapbox.com/help/glossary/projection/
    target_crs = 'urn:ogc:def:crs:OGC::CRS84'
    crs_out = pyproj.CRS.from_string(target_crs)

    # Start GeoJSON output file
    output = geojson_formatter.start_output(target_crs)

    # Iterate through all geospatial features
    features = list(surfaces.keys())
    total_features = len(features)
    for i in range(total_features):

        # Extract and transform coordinates from String polygon returned by SPARQL query
        feature = str(features[i])
        coords, zmax = get_coordinates(surfaces[feature][1], crs_in, crs_out)

        # Specify feature properties to consider (beyond coordinates)
        props = {
            'building': surfaces[feature][0],
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

    # Ensure that ALL linear rings follow the right-hand rule, i.e. exterior rings specified counterclockwise
    # as required per: https://datatracker.ietf.org/doc/html/rfc7946#section-3.1.6
    rewound = rewind(output)
    # Restore json dictionary from returned String by rewind method
    output = json.loads(rewound)

    # Write output to file
    file_name = 'Buildings_.geojson'
    with open(file_name, 'w') as f:
        json.dump(output, indent=4, fp=f)
