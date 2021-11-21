import json

from geojson_rewind import rewind
import numpy as np
import pandas as pd
import pyproj
from SPARQLWrapper import SPARQLWrapper, JSON, SPARQLExceptions

import geojson_creator
from custom_errors import *


###   SPECIFY INPUTS   ###

# Specify (local) Blazegraph properties
server = 'localhost'
port = '9999'
namespace = 'kings-lynn'
#namespace = "geospatial_offset_analysis_epsg27700"
#namespace = 'geospatial_offset_analysis_ocg-crs84'

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    'ocgl': 'http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#',
    'xsd': 'http://www.w3.org/2001/XMLSchema#'
}

# Define full coordinate reference systems (CRS) for pyproj
CRSs = {
    'epsg_27700': 'urn:ogc:def:crs:EPSG::27700',
    'epsg_4326': 'urn:ogc:def:crs:EPSG::4326',
    'crs_84': 'urn:ogc:def:crs:OGC::CRS84',
    'crs_1.3_84': 'urn:ogc:def:crs:OGC:1.3:CRS84'
}

# Specify number of buildings to retrieve (set to None in order to retrieve ALL buildings)
n = None

# Specify output CRS: Both GeoJSON and the TWA Mapbox plotting framework require "OGC::CRS84", see
# GeoJSON: https://datatracker.ietf.org/doc/html/rfc7946#section-4
# Mapbox: https://docs.mapbox.com/help/glossary/projection/
target_crs = CRSs['crs_84']


###   FUNCTIONS   ###

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


def get_coordinates(polygon_data, transformation, dimensions=3):
    '''
        Extracts and transforms polygon coordinates as retrieved from Blazegraph
        to suit GeoJSON polygon requirements (and target CRS)

        Arguments:
            polygon_data - polygon data as returned from Blazegraph
            transformation - pyproj transformation object
            dimensions - number of dimension of output data as integer

        Returns:
            List of polygon coordinates as required for GeoJSON objects
            Minimum elevation (Z value) of polygon surface
            Maximum elevation (Z value) of polygon surface
    '''

    # Initialise output and input coordinate collections
    coordinates = []
    coordinate_str = polygon_data.split("#")
    coordinate_str = [float(c) for c in coordinate_str]

    # If all input coordinates have proper (X,Y,Z) values ...
    if len(coordinate_str) % 3 == 0:

        # Initialise minimum and maximum elevation of polygon
        z_min = np.inf
        z_max = -np.inf

        # Iterate through all polygon points
        nodes = int(len(coordinate_str) / 3)
        for i in range(nodes):
            node = i * 3
            # Transform (x,y) values and append (x,y,z) to output list
            x, y = transformation.transform(coordinate_str[node], coordinate_str[node + 1])
            #x = coordinate_str[node]
            #y = coordinate_str[node + 1]
            z = coordinate_str[node + 2]
            coordinates.append([x, y, z])

        # Check if first and last polygon vertices are equivalent and fix if necessary
        if coordinates[0] != coordinates[-1]:
            print('Surface polygon did not close properly! Geometry has been fixed.')
            coordinates.append(coordinates[0])

        # Convert to numpy array
        coordinates = np.array(coordinates)
        # Extract min and max Z values of polygon
        z_min = min(coordinates[:, 2])
        z_max = max(coordinates[:, 2])

        # Potentially trim dimensions from 3D to 2D by dropping Z value
        coordinates = coordinates[:, :dimensions]

        # Convert coordinates back to regular list
        coordinates = coordinates.tolist()

    # ... otherwise print error
    else:
        print('Erroneous polygon coordinates:', coordinate_str)

    return coordinates, z_min, z_max


if __name__ == '__main__':

    # Construct full SPARQL endpoint URL
    query_endpoint = "http://" + server + ':' + port + '/blazegraph/namespace/' + namespace + "/sparql"

    # Retrieve SPARQL results from Blazegraph
    try:
        kg_buildings = execute_query(get_buildings(n), query_endpoint)
        kg_crs = execute_query(get_crs(), query_endpoint)
    except Exception as e:
        raise QueryError('Error while executing SPARQL query on specified endpoint: ' + e.__class__.__name__).\
              with_traceback(e.__traceback__)

    # Unpack queried CRS result to extract coordinate reference system
    if len(kg_crs['results']['bindings']) != 1:
        raise ResultsError('No or multiple CRS detected in SPARQL query result!')
    else:
        crs = kg_crs['results']['bindings'][0][kg_crs['head']['vars'][0]]['value']

    # Unpack queried buildings results into pandas DataFrame
    rows_list = []
    for s in kg_buildings['results']['bindings']:
        # Create own dictionary for each surface geometry (to form own row in later DataFrame)
        row = {'building': s['bldg']['value'],
               'surface': s['surf']['value'],
               'geometry': s["geom"]["value"]}
        rows_list.append(row)
    # Create DataFrame from dictionary list
    results = pd.DataFrame(rows_list)

    # Initialise pyproj coordinate reference system (CRS) objects
    crs_in = pyproj.CRS.from_string(crs)
    crs_out = pyproj.CRS.from_string(target_crs)

    # Initialise pyproj CRS transformer
    tg = pyproj.transformer.TransformerGroup(crs_in, crs_out)
    # Ensure that most accurate transformation is available
    if not tg.best_available:
        tg.download_grids(verbose=True)
        # Update transformer to take effect after download
        tg = pyproj.transformer.TransformerGroup(crs_in, crs_out)
        if not tg.best_available:
            print('WARNING: Best transformer for specified CRS transformation not available. Results may be inaccurate.')
    # Initialise actual transformer to use
    trans = pyproj.Transformer.from_crs(crs_in, crs_out, always_xy=True)

    # Initialise GeoJSON output dictionaries
    output_3d = geojson_creator.initialise_geojson(target_crs)
    output_2d = geojson_creator.initialise_geojson(target_crs)

    # Initialise output metadata
    metadata = []

    # Initialise unique feature/building IDs
    feature_id = 0

    # Iterate through all buildings (each building represents one geospatial feature)
    for b in results['building'].unique():
        # Update feature_id
        feature_id += 1
        # Extract all surface geometries for this building
        surf = results[results['building'] == b]
        # Initialise list of surface geometry coordinates (polygons)
        all_polygons = []
        # Initialise minimum and maximum elevation of building
        zmin = np.inf
        zmax = -np.inf
        # Iterate through all surface geometries
        for s in surf['surface'].unique():
            # Transform coordinates for surface geometry
            coords, z_min, z_max = get_coordinates(surf[surf['surface'] == s]['geometry'].values[0], trans)
            # Append transformed polygon coordinate list as sublist to overall list for building
            all_polygons.append(coords)
            # Potentially update min and max elevation
            if z_min < zmin:
                zmin = z_min
            if z_max > zmax:
                zmax = z_max

        # Prepare GeoJSON as required by Digital Twin Visualisation Framework (DTVF):
        # Each building is represented by 2D base polygon only (ground and building elevation only properties)
        for p in all_polygons:
            poly = np.array(p)
            # Define small uncertainty range to account for conversion inaccuracies
            eps = 0.001
            # Check if all Z values of polygon are the same and (aprrox.) equal to building elevation
            if (poly[:, 2] == poly[:, 2][0]).all() and \
               ((poly[:, 2][0] >= (zmin - eps)) and (poly[:, 2][0] <= (zmin + eps))):
                # Trim dimensions from 3D to 2D by dropping Z value
                poly = poly[:, :2]
                # Create list to allow for composite ground surfaces
                base_polygon = [poly.tolist()]

        # Specify building/feature properties to consider (beyond coordinates)
        geojson_props = {'displayName': 'Building {}'.format(feature_id),
                         'description': str(b),
                         'fill-extrusion-color': '#666666',
                         'fill-extrusion-opacity': 0.66,
                         # Building ground elevation
                         'fill-extrusion-base': round(zmin, 3),
                         # Building height
                         'fill-extrusion-height': round(zmax-zmin, 3)
                         }

        # Specify metadata properties to consider
        metadata_props = {'id': feature_id,
                          'Building': str(b),
                          'Ground elevation (m)': round(zmin, 3),
                          'Building height (m)': round(zmax-zmin, 3)
                          }

        # Append building/feature to GeoJSON FeatureCollection
        output_3d['features'].append(geojson_creator.add_feature(feature_id, geojson_props, all_polygons))
        output_2d['features'].append(geojson_creator.add_feature(feature_id, geojson_props, base_polygon))
        metadata.append(metadata_props)

    # Ensure that ALL linear rings follow the right-hand rule, i.e. exterior rings specified counterclockwise
    # as required per standard: https://datatracker.ietf.org/doc/html/rfc7946#section-3.1.6
    rewound_3d = rewind(output_3d)
    rewound_2d = rewind(output_2d)
    # Restore json dictionary from returned String by rewind method
    if type(rewound_3d) is str:
        output_3d = json.loads(rewound_3d)
    if type(rewound_2d) is str:
        output_2d = json.loads(rewound_2d)

    # Write GeoJSON dictionary nicely formatted to file
    file_name = 'Buildings_3D.geojson'
    with open(file_name, 'w') as f:
        json.dump(output_3d, indent=4, fp=f)
    file_name = 'Buildings_2D.geojson'
    with open(file_name, 'w') as f:
        json.dump(output_2d, indent=4, fp=f)
    file_name = 'Buildings_2D-meta.json'
    with open(file_name, 'w') as f:
        json.dump(metadata, indent=4, fp=f)
