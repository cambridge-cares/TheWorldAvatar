# This module reads the initially created geojson files and adjusts them slightly
# to enhance DTVF visualisations

from pathlib import Path
import json
import os


scaler = 1.1

# Specify paths to original geojson files as well as scaler values with which to
# multiply property values to enhance visualisation
geojsons = {
    'affected_buildings_first_round.geojson': 1.0,
    'affected_buildings_second_round.geojson': 1.1
}


for g in geojsons:
    # Read original geojson file
    fpath = os.path.join(Path(__file__).parent, 'visualisation','data', g)
    # To ensure correct formatting of GBP symbol suppress ascii encoding and use UTF-8
    with open(fpath, encoding='utf-8') as fp:
        data = json.load(fp)

    # Extract all features
    features_orig = data['features']
    # Sort features by IRI to ensure consistent plotting sequence between derivation cascades
    features_amended = sorted(features_orig, key=lambda x: x['properties']['IRI'])
    for feature in features_amended:
        # Scale property values to improve visualisation
        v = int(feature['properties']['Property market value (£)'] * geojsons[g])
        feature['properties']['Property market value (£)'] = v
    
    # Write amended features into geojson
    data['features'] = features_amended
    
    # Save amended geojson
    fpath = os.path.join(Path(__file__).parent, 'visualisation','data', g.replace('.geojson', '_adj.geojson'))
    # To ensure correct formatting of GBP symbol suppress ascii encoding and use UTF-8
    with open(fpath, 'w', encoding='utf-8') as fp:
        fp.write(json.dumps(data, ensure_ascii=False, indent=4))
