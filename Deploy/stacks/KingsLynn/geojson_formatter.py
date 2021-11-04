# Define GeoJSON writer formats

def write_type():
    out = '{' + '\n' \
          + '	' + '"type": "FeatureCollection",' + '\n' \
          + '	' + '"features": [' + '\n'
    return out


def write_end():
    out = '	' + ']' + '\n' \
          + '}'
    return out


def end_feature(argument):
    switcher = {
        0: "},",
        1: "}"}
    return switcher.get(argument, "Invalid switch")


def write_properties(height):
    out = '			' + '"properties": {' + '\n' \
          + '				' + '"height": ' + str(height) + '\n' \
          + '			' + '},' + '\n'
    return out


def write_geometry(coordinates):
    out = '			' + '"geometry": {' + '\n' \
          + '			  ' + '"coordinates": [' + '\n' \
          + '			  ' + str(coordinates) + '\n' \
          + '			  ' + '],' + '\n' \
          + '			  ' + '"type": "Polygon"' + '\n' \
          + '			' + '}' + '\n'
    return out


def write_feature(uuid, height, coordinates, end_idx):
    out = '		' + '{' + '\n' \
          + '			' + '"type": "Feature",' + '\n' \
          + '			' + '"id": ' + str(uuid) + ',' + '\n' \
          + write_properties(height) \
          + write_geometry(coordinates) \
          + '		' + end_feature(end_idx) + '\n'

    return out
