import geojson
import random

# x1, x2, y1, y2
bounds = [0.12908935546875,  0.6536865234375, 52.921323415263046,  52.57551537552513]

# layer names
layers = ["alpha", "beta", "gamma"]

# icons for each layer
layerIcons = ["alpha", "beta", "gamma"]


for j in range(0, 3):
    features = []
    layer = layers[j]
    layerIcon = layerIcons[j]

    for i in range(0, 100):
        x = random.uniform(bounds[0], bounds[1])
        y = random.uniform(bounds[2], bounds[3])

        feature = {
            "type": "Feature",
            "id": (i + 1),
            "properties": {
                "displayName": "Location " + layer + "-" + str(i),
                "icon-size": 1.0,
                "icon-image": layerIcon
            },
            "geometry": {
                "type": "Point",
                "coordinates": [x, y]
            }
        }
        features.append(feature)

    geometries = {
        'type': 'FeatureCollection',
        'features': features,
    }
    outputFile = "./sccee/" + layer + ".geojson"
    with open(outputFile, 'w') as f:
      geojson.dump(geometries, f)
