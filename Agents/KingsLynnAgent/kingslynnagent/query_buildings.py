import json
import os.path

import numpy as np
import pandas as pd
import pyproj
import re

from geojson_rewind import rewind
from SPARQLWrapper import SPARQLWrapper, JSON

# get settings and functions from kg_utils module
from utilities import utils
from utilities import geojson_creator
from utilities.custom_errors import *


###   SPECIFY INPUTS   ###

# Specify number of buildings to retrieve (set to None in order to retrieve ALL buildings)
n = 100

# Specify required output dimension (although DTVF is "only" capable of plotting extruded 2D data,
# 3D data is required to identify the ground polygon of buildings to be visualised)
output_dimension = 3

# Specify output CRS: Both GeoJSON and the TWA Mapbox plotting framework require "OGC::CRS84", see
# GeoJSON: https://datatracker.ietf.org/doc/html/rfc7946#section-4
# Mapbox: https://docs.mapbox.com/help/glossary/projection/
target_crs = utils.CRSs['crs_84']


###   FUNCTIONS   ###

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
    # Consider only surface polygons with provided geometries/polygon data (some only refer to "told blank nodes")
    query = utils.create_sparql_prefix('ocgl') + \
            utils.create_sparql_prefix('xsd') + \
            '''SELECT DISTINCT ?bldg ?surf (DATATYPE(?geom) as ?datatype) ?geom \
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
    query = utils.create_sparql_prefix('ocgl') + \
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


def get_coordinates(polygon_data, polygon_datatype, transformation, dimensions=3):
    '''
        Extracts and transforms polygon coordinates as retrieved from Blazegraph
        to suit GeoJSON polygon requirements (and target CRS)
        - lon, lat are transformed to specified target CRS
        - elevation remains in original CRS

        Arguments:
            polygon_data - list of all polygon coordinates as retrieved from Blazegraph, i.e. with coordinates of
                           potential linear rings simply appended to coordinates of exterior rings
            polygon_datatype - data type of 'polygon_data' as retrieved from Blazegraph
            transformation - pyproj transformation object
            dimensions - number of dimension of output data as integer [2 or 3]

        Returns:
            List of polygon coordinates as required for GeoJSON objects (incl. interior rings)
            Minimum elevation (Z value) of polygon surface
            Maximum elevation (Z value) of polygon surface
    '''

    # Initialise output coordinate collection
    polygon = []
    z_min, z_max = None, None

    # Retrieve list of polygon's linear rings
    linear_rings, available_dimensions = split_polygon_data(polygon_data, polygon_datatype)

    # Check whether required output dimension for data is covered in available data
    if available_dimensions < dimensions:
        raise ValueError('Specified dimension of coordinates exceeds native format of stored data.')

    # Iterate through all linear rings
    for ring in linear_rings:
        # Initialise ring's coordinate list
        coordinates = []

        # Iterate through all polygon points
        nodes = int(len(ring) / available_dimensions)
        for i in range(nodes):
            node = i * available_dimensions
            # Transform (x,y) values - required for correct plotting in Mapbox
            x, y = transformation.transform(ring[node], ring[node + 1])
            if available_dimensions == 2:
                # Append (x,y) to output list
                coordinates.append([x, y])
            elif available_dimensions == 3:
                # Keep z value as Ordnance Survey Newlyn height - more tangible value for flooding analysis, etc.
                z = ring[node + 2]
                # Append (x,y,z) to output list
                coordinates.append([x, y, z])

        # Check if first and last polygon vertices are equivalent and fix if necessary
        if coordinates[0] != coordinates[-1]:
            print('Surface polygon did not close properly! Geometry has been fixed.')
            coordinates.append(coordinates[0])

        # Convert to numpy array
        coordinates = np.array(coordinates)

        if not z_min and not z_max and dimensions == 3:
            # Extract min and max Z values of polygon
            z_min = min(coordinates[:, 2])
            z_max = max(coordinates[:, 2])

        # Potentially trim dimensions from 3D to 2D by dropping Z value
        coordinates = coordinates[:, :dimensions]

        # Convert coordinates back to regular list
        coordinates = coordinates.tolist()

        # Append linear ring to polygon
        polygon.append(coordinates)

    return polygon, z_min, z_max


def split_polygon_data(polygon_data, polygon_datatype):
    '''
        Transforms coordinate string describing the polygon (as retrieved from Blazegraph) into a list of linear rings
        - exterior ring as first list element
        - potential interior rings as further list elements

        Arguments:
            polygon_data - list of all polygon coordinates as retrieved from Blazegraph, i.e. with coordinates of
                           potential linear rings simply appended to coordinates of exterior rings
            polygon_datatype - data type of 'polygon_data' as retrieved from Blazegraph

        Returns:
            List of linear rings to describe polygon [[exterior ring], [interior ring1], [interior ring2], ... ]
            dimension of polygon coordinates data
    '''

    # Initialise list of linear rings
    rings = []

    # Check whether polygon_datatype indicates any interior rings, i.e. a datatype like
    # "...\POLYGON-3-45" indicates 3 coordinates per point and an exterior ring consisting of (45/3=)15 points
    # "...\POLYGON-3-45-15" indicates a polygon with an interior ring consisting of (15/3=)5 points and an exterior
    #                       ring consisting of (15-5=)10 points
    # "...\POLYGON-3-45-15-15" indicates a polygon with two interior rings consisting of (15/3=)5 points each and an
    #                          exterior ring consisting of (15-5-5=)5 points
    match = re.search('\w+-\d+-\d+-\d+$', polygon_datatype)

    while match:
        # Extract number of points in (last) interior ring
        match = match.group()
        m = match.rfind('-')
        n = len(match)
        inner = int(match[m + 1:n])
        # Extract respective part of the coordinate string
        switch = [m.start() for m in re.finditer('#', polygon_data)][-inner]

        # Append string representing (last) interior ring to overall rings list
        rings.append(polygon_data[switch + 1:])

        # Update coordinate string and data type for next iteration (strip already extracted interior ring data)
        polygon_datatype = polygon_datatype[:-(n-m)]
        polygon_data = polygon_data[:switch]
        match = re.search('\w+-\d+-\d+-\d+$', polygon_datatype)

    # Add exterior linear ring and reverse element order to start with linear ring
    rings.append(polygon_data)
    rings = rings[::-1]

    # Extract dimension from polygon datatype
    match = re.search('\w+-\d+-\d+$', polygon_datatype)
    match = match.group()
    m = match.find('-')
    n = match.rfind('-')
    dimension = int(match[m + 1:n])

    # Split coordinate strings into number lists and check alignment with specified dimension
    for i in range(len(rings)):
        split = rings[i].split("#")
        if len(split) % 3 != 0:
            raise IndexError('Number of linear ring coordinates does not match specified dimension.')
        rings[i] = [float(c) for c in split]

    return rings, dimension


if __name__ == '__main__':

    # Retrieve SPARQL results from Blazegraph
    try:
        kg_buildings = execute_query(get_buildings(n), utils.QUERY_ENDPOINT)
        kg_crs = execute_query(get_crs(), utils.QUERY_ENDPOINT)
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
               'datatype': s['datatype']['value'],
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
        base_elevation = np.inf
        top_elevation = -np.inf

        # Iterate through all surface geometries
        for s in surf['surface'].unique():
            # Extract list of linear rings forming this surface polygon
            polytype = surf[surf['surface'] == s]['datatype'].values[0]
            polydata = surf[surf['surface'] == s]['geometry'].values[0]

            # Transform coordinates for surface geometry
            polygon, zmin, zmax = get_coordinates(polydata, polytype, trans, output_dimension)
            # Append transformed polygon coordinate list as sublist to overall list for building
            all_polygons.append(polygon)
            # Potentially update min and max elevation
            if zmin < base_elevation:
                base_elevation = zmin
            if zmax > top_elevation:
                top_elevation = zmax

        # Prepare GeoJSON as required by Digital Twin Visualisation Framework (DTVF):
        # Each building is represented by 2D base polygon only (ground and building elevation only properties)
        for p in all_polygons:
            # Extract only exterior ring per polygon to check for base polygon
            poly = np.array(p[0])
            # Define small uncertainty range to account for conversion inaccuracies
            eps = 0.001
            # Check if all Z values of polygon are the same and (approx.) equal to building elevation
            if (poly[:, 2] == poly[:, 2][0]).all() and \
               ((poly[:, 2][0] >= (zmin - eps)) and (poly[:, 2][0] <= (zmin + eps))):
                # Once found, get entire base polygon (incl. interior rings) and convert to 2D
                base_polygon = [[]]
                for poly in p:
                    # Trim dimensions from 3D to 2D by dropping Z value
                    poly = np.array(poly)[:, :2]
                    # Create list to allow for composite ground surfaces
                    base_polygon[0].append(poly.tolist())

        # Specify building/feature properties to consider (beyond coordinates)
        geojson_props = {'displayName': 'Building {}'.format(feature_id),
                         'description': str(b),
                         'fill-extrusion-color': '#666666',
                         'fill-extrusion-opacity': 0.66,
                         # Building ground elevation
                         'fill-extrusion-base': 0, #round(zmin, 3)
                         # Building (absolute) height, i.e. NOT relative height above base
                         'fill-extrusion-height': round(zmax-zmin, 3) #round(zmax, 3)
                         }

        # Specify metadata properties to consider
        metadata_props = {'id': feature_id,
                          'Building': str(b),
                          'Ground elevation (m)': round(zmin, 3),
                          'Building height (m)': round(zmax, 3)
                          }

        # Append building/feature to GeoJSON FeatureCollection
        #output_3d['features'].append(geojson_creator.add_feature(feature_id, geojson_props, all_polygons))
        output_2d['features'].append(geojson_creator.add_feature(feature_id, geojson_props, base_polygon))
        metadata.append(metadata_props)

    # Ensure that ALL linear rings follow the right-hand rule, i.e. exterior rings specified counterclockwise
    # as required per standard: https://datatracker.ietf.org/doc/html/rfc7946#section-3.1.6
    #rewound_3d = rewind(output_3d)
    rewound_2d = rewind(output_2d)
    # Restore json dictionary from returned String by rewind method
    # if type(rewound_3d) is str:
    #     output_3d = json.loads(rewound_3d)
    if type(rewound_2d) is str:
        output_2d = json.loads(rewound_2d)

    # Write GeoJSON dictionary nicely formatted to file
    # file_name = 'buildings_3d.geojson'
    # with open(file_name, 'w') as f:
    #     json.dump(output_3d, indent=4, fp=f)
    file_name = os.path.join(utils.OUTPUT_DIR, 'built_environment', 'buildings_2d.geojson')
    with open(file_name, 'w') as f:
        json.dump(output_2d, indent=4, fp=f)
    file_name = os.path.join(utils.OUTPUT_DIR, 'built_environment', 'buildings_2d-meta.json')
    with open(file_name, 'w') as f:
        json.dump(metadata, indent=4, fp=f)
