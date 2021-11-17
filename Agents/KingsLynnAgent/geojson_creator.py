# Creates dictionary for GeoJSON output

# TODO: include proper handling for interior rings

def initialise_geojson(crs_name='urn:ogc:def:crs:OGC::CRS84'):
    '''
        Initialises dictionary for GeoJSON output
        According to latest standard, all GeoJSON coordinates SHALL be in "urn:ogc:def:crs:OGC::CRS84" (default)
        The use of alternative CRS is still possible, but STRONGLY advices against

        Arguments:
            crs_name - coordinate reference system as String

        Returns:
            Empty GeoJSON FeatureCollection dictionary
    '''

    # Start GeoJSON FeatureCollection
    geojson = {'type': 'FeatureCollection',
               'features': []
               }

    # Add CRS member if required
    if crs_name not in ['urn:ogc:def:crs:OGC::CRS84', 'urn:ogc:def:crs:OGC:1.3:CRS84']:
        geojson['crs'] = {'type': 'name',
                          'properties': {'name': str(crs_name)}
                          }

    return geojson


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
                    'coordinates': coordinates
                    }
    else:
        # Add MultiPolygon geometry member if coordinates list contains coordinates for multiple surface geometries
        geometry = {'type': 'MultiPolygon',
                    'coordinates': [coordinates]
                    }

    return geometry


def add_feature(feature_id, properties, coordinates):
    '''
        Returns single GeoJSON feature for overall GeoJSON FeatureCollection

        Arguments:
            feature_id - numerical feature identifier
            properties - feature properties as dictionary
            coordinates - list of coordinates in the form [[[x1,y1,z1],...], [[x2,y2,z2],...], ...]

        Returns:
            GeoJSON Feature object as dictionary
    '''

    feature = {'type': 'Feature',
               'id': int(feature_id),
               'properties': properties,
               'geometry': add_geometry(coordinates)
               }

    return feature
