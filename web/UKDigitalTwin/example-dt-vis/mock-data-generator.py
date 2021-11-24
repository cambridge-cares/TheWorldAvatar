#
# This script is not part of the visualisation framework, or the example visualisation
# implementation. It only exists to generate some mock data for use during development.
# Run/edit it at your own risk.
#

import geojson
import random

# x1, x2, y1, y2
bounds = [0.12908935546875,  0.6536865234375, 52.921323415263046,  52.57551537552513]

for j in range(0, 3):
    lineFeatures = []
    pointFeatures = []
    pointIndex = 1

    for i in range(0, 50):
        x1 = random.uniform(bounds[0], bounds[1])
        y1 = random.uniform(bounds[2], bounds[3])

        x2 = random.uniform(bounds[0], bounds[1])
        y2 = random.uniform(bounds[2], bounds[3])

        lineFeatures.append({
            "type": "Feature",
            "id": (i + 1),
            "properties": {
                "line-color": "#00FF00"
            },
            "geometry": {
                "type": "LineString",
                "coordinates": [[x1, y1], [x2, y2]]
            }
        })

        pointFeatures.append({
            "type": "Point",
            "id": pointIndex,
            "properties": {
                "displayName": "Location " + str(pointIndex),
                "circle-color": "#FF0000",
                'circle-stroke-color':  "#000000"
            },
            "geometry": {
                "type": "Point",
                "coordinates": [x1, y1]
            }
        })
        pointIndex += 1

        pointFeatures.append({
            "type": "Feature",
            "id": pointIndex,
            "properties": {
                "displayName": "Location " + str(pointIndex),
                "circle-color": "#0000FF",
                'circle-stroke-color':  "#000000"
            },
            "geometry": {
                "type": "Point",
                "coordinates": [x2, y2]
            }
        })
        pointIndex += 1

    with open("./lines.geojson", 'w') as f:
        geojson.dump({
            'type': 'FeatureCollection',
            'features': lineFeatures,
        }, f)

    with open("./points.geojson", 'w') as f:
        geojson.dump({
            'type': 'FeatureCollection',
            'features': pointFeatures,
        }, f)
