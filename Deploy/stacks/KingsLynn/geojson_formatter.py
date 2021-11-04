# Define GeoJSON writer formats

def start_output(crs_name):
    # Returns properly formatted start of GeoJSON output incl. CRS definition (file, etc.)
    out = '{' + '\n' \
          + '	' + '"type": "FeatureCollection",' + '\n' \
          + '	' + '"crs": {' + '\n' \
          + '	    ' + '"type": "name",' + '\n' \
          + '	    ' + '"properties": {' + '\n' \
          + '	        ' + '"name": "' + str(crs_name) + '"' + '\n' \
          + '	    ' + '}' + '\n' \
          + '	' + '},' + '\n' \
          + '	' + '"features": [' + '\n'

    return out


def end_output():
    # Returns properly formatted end of GeoJSON output (file, etc.)
    out = '	' + ']' + '\n' \
          + '}'

    return out


def end_feature(argument):
    # Closes a single GeoJSON feature
    switcher = {
        0: "},",
        1: "}"}

    return switcher.get(argument, "Invalid switch")


def write_properties(props):
    # Writes properties (provided as DICTIONARY) for single GeoJSON feature
    out = '			' + '"properties": {' + '\n'

    # Add all properties contained in dictionary
    keys = list(props.keys())
    total_props = len(keys)
    for p in range(total_props):
        if p != total_props - 1:
            out += '				"' + keys[p] + '": "' + str(props[keys[p]]) + '",\n'
        else:
            out += '				"' + keys[p] + '": "' + str(props[keys[p]]) + '"\n'

    out += '			' + '},' + '\n'

    return out


def write_polygon(coordinates):
    # Writes polygon for single GeoJSON feature
    out = '			' + '"geometry": {' + '\n' \
          + '				' + '"coordinates": [' + '\n' \
          + '				' + str(coordinates) + '\n' \
          + '				' + '],' + '\n' \
          + '				' + '"type": "Polygon"' + '\n' \
          + '			' + '}' + '\n'

    return out


def write_feature(uuid, props, coordinates, end_idx):
    # Write single GeoJSON feature to (open) FeatureCollection
    out = '		' + '{' + '\n' \
          + '			' + '"type": "Feature",' + '\n' \
          + '			' + '"id": "' + str(uuid) + '",' + '\n' \
          + write_properties(props) \
          + write_polygon(coordinates) \
          + '		' + end_feature(end_idx) + '\n'

    return out
