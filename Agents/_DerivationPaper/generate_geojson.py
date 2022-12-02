# This module creates geojson output files for visualisation of (affected) buildings

from pathlib import Path
from typing import List
import json
import os

from pyderivationagent.data_model import iris as pda_iris
from pyderivationagent import PySparqlClient
import chemistry_and_robots.kg_operations.dict_and_list as dal

from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
import iris

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# Specify name of csv with affected properties (determined using QGIS)
affected = 'affected_property_iris.csv'

# Specify geojson output file name
bldgs_geojson = 'affected_buildings.geojson'

class BuildingPoint:
    def __init__(self, lat, long, iri):
        self.lat = lat
        self.long = long
        self.iri = iri


def generate_geojson(bldg_pts: List[BuildingPoint], filepath: str):
    geojson_str = """{ "type": "FeatureCollection", "features": ["""
    for pt in bldg_pts:
        feature = f"""{{
            "type": "Feature",
            "properties": {{
                "IRI": "{pt.iri}"
            }},
            "geometry": {{
                "type": "Point",
                "coordinates": [
                    {pt.long},
                    {pt.lat}
                ]
            }}
        }},"""
        # adding new line 
        geojson_str += '\n'+feature

    # removing last comma as is last line
    geojson_str = geojson_str[:-1]
    # finishing file end 
    end_geojson = """]}"""
    geojson_str += end_geojson
    with open(filepath, 'w') as f:
        f.write(json.dumps(json.loads(geojson_str), indent=4))
    print(f'Geojson file created at {filepath}')


def get_the_affected_buildings(input_csv):
    """
    Get the list of buildings that are affected by the flood event.
    To do it perooperly, the area should be queried from the polygon of the flood event.
    """
    # Extract IRIs from csv file
    with open(input_csv, 'r') as f:
        iris = f.read()
    iris = iris.split('\n')
    iris = iris[1:-1]
    return iris


def retrieve_affected_property_location(sparql_client: PySparqlClient, affected_property_iris: list):
    # Construct query to retrieve below information for each affected property from KG
    # NOTE for this iteration, we assume that the property value estimation is already computed if the derivation is created
    # - market value (mv) IRI
    query = f"""{pda_iris.PREFIX_RDF} {pda_iris.PREFIX_RDFS}
            SELECT DISTINCT ?property ?lat_long
            WHERE {{
                VALUES ?property {{ <{'> <'.join(affected_property_iris)}> }}
                ?property <{iris.OBE_HASWGS84LATITUDELONGITUDE}> ?lat_long.
            }}"""

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    logger.info(f"Query to retrieve property info: {query}")
    response = sparql_client.performQuery(query)
    logger.info(f"Length of response: {len(response)}")

    # Rearrange response into a dictionary where the information for each property are grouped together
    # Target format of the dictionary:
    # {
    #     "property": {
    #         'lat_long': lat_long,
    #     },
    #     ... (other properties)
    # }
    property_info_dct = {}
    property_lst = dal.get_unique_values_in_list_of_dict(response, 'property')
    for property_iri in property_lst:
        property_info_dct[property_iri] = {}
        sub_response = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'property', property_iri)
        # it will be assigned as None if the key is not found
        property_info_dct[property_iri]['lat_long'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'lat_long')

    return property_info_dct


if __name__ == '__main__':
    # Create a PySparqlClient instance
    sparql_client = PySparqlClient(
        query_endpoint=SPARQL_QUERY_ENDPOINT,
        update_endpoint=SPARQL_UPDATE_ENDPOINT,
    )

    # TODO here we mocked the affected buildings, should be done by querying the flood event polygon from ontop
    # Get the list of buildings that are affected by the flood event
    affected_building_iris = get_the_affected_buildings(os.path.join(Path(__file__).parent, 'data', affected))

    # Retrieve all affected building info
    property_location_dct = retrieve_affected_property_location(sparql_client, affected_building_iris)

    # Construct a list of BuildingPoint objects
    bldg_pts = [
        BuildingPoint(
            lat=float(property_location_dct[iri]['lat_long'].split('#')[0]),
            long=float(property_location_dct[iri]['lat_long'].split('#')[1]),
            iri=iri,
        ) for iri in affected_building_iris
    ]
    # Generate geojson file
    fp = os.path.join(Path(__file__).parent, 'visualisation','data', bldgs_geojson)
    generate_geojson(bldg_pts, fp)
