################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 09 Apr 2022                            #
################################################

import pytest
import pandas as pd

# Import module(s) under test from agent
from agent.utils.output_formatting import create_geojson_output


def test_output_data():
    # Test correct creation of GeoJSON output for MetOffice station data
    
    # Define test station data as returned by "get_all_stations_with_details"
    test_data = '{"station":{"0":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/ReportingStation_e30228ab-1c5e-47c2-be5e-345447b4fb5c","1":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/ReportingStation_421b72c6-e43a-48f8-893e-9367bb58279d","2":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/ReportingStation_6adc072f-83a0-4aec-9cc6-2345e55f5ff8"},"label":{"0":"Scatsta","1":"Humberside Airport","2":"Carlisle Airport"},"latlon":{"0":"60.4322#-1.2992","1":"53.5797#-0.3472","2":"54.9375#-2.8092"},"elevation":{"0":25,"1":24,"2":50},"dataIRI":{"0":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/Forecast_0d34c13b-7db1-4282-a88f-b9c3e5a6bf4f","1":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/Forecast_61a360eb-8de0-438e-a82e-5631eefc58c5","2":"https:\\/\\/www.theworldavatar.com\\/kg\\/ontoems\\/Forecast_b90ef70e-2d45-4242-afa7-8cad5cb01f82"},"dtvf_id":{"0":0,"1":1,"2":2}}'
    test_df = pd.read_json(test_data)
    # Define expected proper GeoJSON format
    expected_geojson = {
        "type": "FeatureCollection",
        "features": [
            {
                "type": "Feature",
                "id": 0,
                "properties": {
                    "displayName": "Scatsta",
                    "circle-color": "#C0392B",
                    "circle-stroke-width": 1,
                    "circle-stroke-color": "#000000",
                    "circle-stroke-opacity": 0.75,
                    "circle-opacity": 0.66
                },
                "geometry": {
                    "type": "Point",
                    "coordinates": [
                        -1.2992,
                        60.4322
                    ]
                }
            },
            {
                "type": "Feature",
                "id": 1,
                "properties": {
                    "displayName": "Humberside Airport",
                    "circle-color": "#C0392B",
                    "circle-stroke-width": 1,
                    "circle-stroke-color": "#000000",
                    "circle-stroke-opacity": 0.75,
                    "circle-opacity": 0.66
                },
                "geometry": {
                    "type": "Point",
                    "coordinates": [
                        -0.3472,
                        53.5797
                    ]
                }
            },
            {
                "type": "Feature",
                "id": 2,
                "properties": {
                    "displayName": "Carlisle Airport",
                    "circle-color": "#C0392B",
                    "circle-stroke-width": 1,
                    "circle-stroke-color": "#000000",
                    "circle-stroke-opacity": 0.75,
                    "circle-opacity": 0.66
                },
                "geometry": {
                    "type": "Point",
                    "coordinates": [
                        -2.8092,
                        54.9375
                    ]
                }
            }
        ]
    }

    geojson = create_geojson_output(test_df)
    # Assert sorted geojson files
    assert sorted(geojson.items()) == sorted(expected_geojson.items())
