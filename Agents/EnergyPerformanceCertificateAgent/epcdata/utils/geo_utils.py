################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 26 Sep 2022                            #
################################################

# The purpose of this module is to provide several utility 
# functions to handle geospatial data (retrieved from OntoCityGml)

import re
import pyproj
import numpy as np

import agentlogging


# Initialise logger
logger = agentlogging.get_logger("prod")


def initialise_pyproj_transformer(current_crs: str, target_crs: str):
    """
        Returns pyproj transformer object for specified coordinate reference systems
    """

    # Initialise pyproj coordinate reference system (CRS) objects
    crs_in = pyproj.CRS.from_string(current_crs)
    crs_out = pyproj.CRS.from_string(target_crs)

    # Initialise pyproj CRS transformer
    tg = pyproj.transformer.TransformerGroup(crs_in, crs_out)
    # Ensure that most accurate transformation is available
    if not tg.best_available:
        tg.download_grids(verbose=True)
        # Update transformer to take effect after download
        tg = pyproj.transformer.TransformerGroup(crs_in, crs_out)
        if not tg.best_available:
            logger.warn('Best transformer for specified CRS transformation not available. Results may be inaccurate.')
    
    # Initialise actual transformer to use
    trans = pyproj.Transformer.from_crs(crs_in, crs_out, always_xy=True)

    return trans


def get_coordinates(polygon_data, polygon_datatype, transformation, dimensions=3):
    '''
        Extracts and transforms OntoCityGml polygon coordinates as retrieved from Blazegraph
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
    z_min = None

    # Retrieve list of polygon's linear rings
    linear_rings, available_dimensions = split_polygon_data(polygon_data, polygon_datatype)

    # Check whether required output dimension for data is covered in available data
    if available_dimensions < dimensions:
        logger.error('Specified dimension of coordinates exceeds native format of stored data.')
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
            logger.info('Surface polygon did not close properly! Geometry has been fixed.')
            coordinates.append(coordinates[0])

        # Convert to numpy array
        coordinates = np.array(coordinates)

        if not z_min and dimensions == 3:
            # Extract min Z values from exterior polygon ring
            z_min = min(coordinates[:, 2])

        # Potentially trim dimensions from 3D to 2D by dropping Z value
        coordinates = coordinates[:, :dimensions]

        # Convert coordinates back to regular list
        coordinates = coordinates.tolist()

        # Append linear ring to polygon
        polygon.append(coordinates)

    return polygon, z_min


def split_polygon_data(polygon_data, polygon_datatype):
    '''
        Transforms OntoCityGml coordinate string describing the polygon (as retrieved 
        from Blazegraph) into a list of linear rings
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


def add_geometry(coordinates):
    '''
        Returns geometry member (Polygon, Multipolygon) for single GeoJSON feature

        Arguments:
            coordinates - list of polygon coordinates in the form [[[x1,y1,z1],...], [[x2,y2,z2],...], ...]

        Returns:
            GeoJSON Geometry object as dictionary
    '''

    if len(coordinates) == 1:
        # Add single Polygon geometry member if coordinates list contains only coordinates for one surface geometry
        geometry = {'type': 'Polygon',
                    'coordinates': coordinates[0]
                    }
    else:
        # Add MultiPolygon geometry member if coordinates list contains coordinates for multiple surface geometries
        geometry = {'type': 'MultiPolygon',
                    'coordinates': coordinates
                    }

    return geometry


def create_geojson_feature(polygon_coordinates, properties=None,
                           crs_name='urn:ogc:def:crs:OGC::CRS84'):
    '''
        Creates dictionary for GeoJSON output
        According to latest standard, all GeoJSON coordinates SHALL be in "urn:ogc:def:crs:OGC::CRS84" (default)
        The use of alternative CRS is still possible, but STRONGLY advices against

        Arguments:
            properties - feature properties as dictionary
            coordinates - list of polygon coordinates (incl. interior rings) in the form
                          [ [ [[x1,y1,z1], ... ],     poly1 exterior ring
                              [[x1,y1,z1], ... ],     poly1 interior ring1
                              ... ],
                            [ [[x2,y2,z2], ... ],     poly2 exterior ring
                              ... ],                  poly2 interior ring1
                          ]

        Returns:
            GeoJSON Feature object as dictionary
    '''

    feature = {'type': 'Feature',
               'geometry': add_geometry(polygon_coordinates)
               }
    if properties: feature['properties'] = properties

    # Add CRS member if required
    if crs_name not in ['urn:ogc:def:crs:OGC::CRS84', 'urn:ogc:def:crs:OGC:1.3:CRS84']:
        feature['crs'] = {'type': 'name',
                          'properties': {'name': str(crs_name)}
                          }

    return feature
