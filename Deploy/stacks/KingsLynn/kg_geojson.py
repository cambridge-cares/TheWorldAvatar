# GeoJSON Write Formate
def WriteType():
    Out = '{' + '\n' \
        + '	' + '"type": "FeatureCollection",' + '\n' \
        + '	' + '"features": [' + '\n'
    return Out

def WriteEnd():
    Out = '	' + ']' + '\n' \
        + '}'
    return Out

def EndFeatureOP(argument):
    switcher = {
        0: "},",
        1: "}"}
    return switcher.get(argument, "Invalid switch")

def WriteProperties(height):
    out = '			' +  '"properties": {' + '\n' \
        + '				' + '"height": ' + str(height) + '\n' \
        + '			' + '},' + '\n'
    return out

def WriteGeometry(Coordinate):
    out = '			' +  '"geometry": {' + '\n' \
        + '			  ' + '"coordinates": [' + '\n' \
        + '			  ' + str(Coordinate) + '\n' \
        + '			  ' + '],'+'\n' \
        + '			  ' + '"type": "Polygon"' + '\n' \
        + '			' + '}' + '\n'
    return out

def WriteFeature(UUID,height,Coordinate,EndIdx):
    Out = '		' + '{' + '\n' \
        + '			' + '"type": "Feature",' + '\n' \
        + '			' + '"id": ' + str(UUID) + ',' + '\n' \
        + WriteProperties(height) \
        + WriteGeometry(Coordinate) \
        + '		' + EndFeatureOP(EndIdx) + '\n'

    return Out