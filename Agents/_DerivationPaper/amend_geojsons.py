# This module reads the initially created geojson files and adjusts them slightly
# to enhance DTVF visualisations

from pathlib import Path
import json
import os


# Specify paths to original geojson files as well as scaler values with which to
# multiply property values to enhance visualisation
buildings = {
    'affected_buildings_first_round.geojson': (1.0, 'flood-areas_first_round.geojson'),
    'affected_buildings_second_round.geojson': (1.1, 'flood-areas_second_round.geojson')
}

# 
for b in buildings:
    # Read original geojson file
    fpath = os.path.join(Path(__file__).parent, 'visualisation','data', b)
    # To ensure correct formatting of GBP symbol suppress ascii encoding and use UTF-8
    with open(fpath, encoding='utf-8') as fp:
        data = json.load(fp)

    # Extract all features
    features_orig = data['features']
    # Sort features by IRI to ensure consistent plotting sequence between derivation cascades
    features_amended = sorted(features_orig, key=lambda x: x['properties']['IRI'])

    # Initialise list of all property value estimates
    vlst = []
    for feature in features_amended:
        # Scale property values to improve visualisation
        v = int(feature['properties']['Property market value (£)'] * buildings[b][0])
        vlst.append(v)
        feature['properties']['Property market value (£)'] = v
    
    # Write amended features into geojson
    data['features'] = features_amended
    
    # Save amended geojson
    fpath = os.path.join(Path(__file__).parent, 'visualisation','data', b.replace('.geojson', '_adj.geojson'))
    # To ensure correct formatting of GBP symbol suppress ascii encoding and use UTF-8
    with open(fpath, 'w', encoding='utf-8') as fp:
        fp.write(json.dumps(data, ensure_ascii=False, indent=4))


    # Update evaluation for entire flood polygon
    # Read original geojson file
    fpath = os.path.join(Path(__file__).parent, 'visualisation','data', buildings[b][1])
    # To ensure correct formatting of GBP symbol suppress ascii encoding and use UTF-8
    with open(fpath, encoding='utf-8') as fp:
        area = json.load(fp)
    
    # Amend risk assessment
    assessment = area['features'][0]['properties']['Buildings at risk']
    assessment['Number of buildings'] = len(vlst)
    assessment['Estimated market value (£m)'] = round(sum(vlst)/1e6,1)
    area['features'][0]['properties']['Buildings at risk'] = assessment

    # Save amended geojson
    fpath = os.path.join(Path(__file__).parent, 'visualisation','data', buildings[b][1].replace('.geojson', '_adj.geojson'))
    # To ensure correct formatting of GBP symbol suppress ascii encoding and use UTF-8
    with open(fpath, 'w', encoding='utf-8') as fp:
        fp.write(json.dumps(area, ensure_ascii=False, indent=4))


# Create updated colorbar
import create_colorlegend
