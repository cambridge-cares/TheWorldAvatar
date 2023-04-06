################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 09 Apr 2022                            #
################################################

import pytest
import pandas as pd

# Import modules under test from gasgridagent
from airquality.utils.output_formatting import create_geojson_output


def test_output_data():

    # Define test station data as returned by "get_all_stations_with_details"
    test_data = '{"stationID":{"0":"Leeds Centre_53.80378#-1.546472","1":"Auchencorth Moss_55.79216#-3.2429"},"label":{"0":"Leeds Centre","1":"Auchencorth Moss"},"station":{"0":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/ReportingStation_7c895dab-0150-47cb-87e4-0706814ca67f","1":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/ReportingStation_8558c992-ce06-4b03-b199-f0c403199d43"},"label":{"0":"Leeds Centre","1":"Auchencorth Moss"},"latlon":{"0":"53.80378#-1.54647","1":"55.79216#-3.2429"},"elevation":{"0":null,"1":null},"dataIRI":{"0":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/Measure_ce48f068-90eb-47ad-8b75-88a99401a37a","1":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/Measure_61c3ebd5-bd3b-4a01-8c65-595e66f33f25"},"dtvf_id":{"0":"0","1":"1"}}'
    test_df = pd.read_json(test_data)
    # Define expected proper GeoJSON format
    expected_geojson = {
        "type": "FeatureCollection",
        "features": [
            {
                "type": "Feature",
                "id": 0,
                "properties": {
                    "displayName": "Leeds Centre",
                    "circle-color": "#C0392B",
                    "circle-stroke-width": 1,
                    "circle-stroke-color": "#000000",
                    "circle-stroke-opacity": 0.75,
                    "circle-opacity": 0.66
                },
                "geometry": {
                    "type": "Point",
                    "coordinates": [
                        -1.54647,
                        53.80378
                    ]
                }
            },
            {
                "type": "Feature",
                "id": 1,
                "properties": {
                    "displayName": "Auchencorth Moss",
                    "circle-color": "#C0392B",
                    "circle-stroke-width": 1,
                    "circle-stroke-color": "#000000",
                    "circle-stroke-opacity": 0.75,
                    "circle-opacity": 0.66
                },
                "geometry": {
                    "type": "Point",
                    "coordinates": [
                        -3.2429,
                        55.79216
                    ]
                }
            }
        ]
    }

    geojson = create_geojson_output(test_df)
    # Assert sorted geojson files
    assert sorted(geojson.items()) == sorted(expected_geojson.items())
