# This module creates geojson output files for visualisation of (affected) buildings

from pathlib import Path
from typing import List
import json
import os

from pyderivationagent import PySparqlClient
import chemistry_and_robots.kg_operations.dict_and_list as dal

from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
import iris

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# Specify queries and geojson output file names
# NOTE Here we mocked the affected buildings, i.e. determined beforehand and got label
#      "affected"; this should finally be done by querying the flood event polygon from ontop
# NOTE For this iteration, we assume that the property value estimation is already 
#      computed if the derivation is created
query_affected = f"""
    SELECT DISTINCT ?property ?lat_long ?value
    WHERE {{
        ?property <{iris.RDF_TYPE}> <{iris.OBE_BUILDING}> ; 
                  <{iris.RDFS_LABEL}> "affected" ; 
                  <{iris.OBE_HASWGS84LATITUDELONGITUDE}> ?lat_long ; 
                  <{iris.OBE_HASMARKETVALUE}>/<{iris.OM_HAS_VALUE}>/<{iris.OM_HAS_NUMERICAL_VALUE}> ?value
    }}"""
query_not_affected = f"""
    SELECT DISTINCT ?property ?lat_long ?value
    WHERE {{
        ?property <{iris.RDF_TYPE}> <{iris.OBE_BUILDING}> ; 
                  <{iris.OBE_HASWGS84LATITUDELONGITUDE}> ?lat_long . 
        OPTIONAL {{ ?property <{iris.OBE_HASMARKETVALUE}>/<{iris.OM_HAS_VALUE}>/<{iris.OM_HAS_NUMERICAL_VALUE}> ?value }}
        FILTER NOT EXISTS {{ ?property <{iris.RDFS_LABEL}> "affected" }}
    }}"""
    
geojson_affected = 'affected_buildings.geojson'
geojson_not_affected = 'not_affected_buildings.geojson'

# Specify filepath to flood area geojson file
flood_area = 'flood-areas.geojson'
flood_original = os.path.join(Path(__file__).parent, 'data', flood_area)
flood_amended = os.path.join(Path(__file__).parent, 'visualisation', 'data', flood_area)


class BuildingPoint:
    def __init__(self, lat, long, iri, value=None):
        self.lat = lat
        self.long = long
        self.iri = iri
        self.value = value


def generate_geojson(bldg_pts: List[BuildingPoint], filepath: str):
    geojson_str = """{ "type": "FeatureCollection", "features": ["""
    for pt in bldg_pts:
        # Include market value node if available
        if pt.value:
            properties_node = f""""properties": {{
                "IRI": "{pt.iri}",
                "Property market value (£)": {pt.value}
            }}"""
        else:
            properties_node = f""""properties": {{
                "IRI": "{pt.iri}"
            }}"""
        feature = f"""{{
            "type": "Feature",
            {properties_node},
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
    # To ensure correct formatting of GBP symbol suppress ascii encoding
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(json.dumps(json.loads(geojson_str), ensure_ascii=False, indent=4))
    print(f'Geojson file created at {filepath}')


def retrieve_property_locations(sparql_client: PySparqlClient, query: str):
    # Retrieve building data from KG (both for affected and not affected buildings)

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
    #         'value': value
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
        property_info_dct[property_iri]['value'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'value')

    return property_info_dct


def get_assessment_for_area(individual_properties: dict):
    # Summarises values of affected buildings into total for area
    # `individual_properties` is dict as returned by `retrieve_property_locations`

    n = len(individual_properties)
    vals = [v['value'] for k,v in individual_properties.items()]
    total_value = 0
    for v in vals:
        try:
            v = int(v)
            total_value += v
        except TypeError:
            pass
    
    return n, total_value


if __name__ == '__main__':
    
    # Create a PySparqlClient instance
    sparql_client = PySparqlClient(
        query_endpoint=SPARQL_QUERY_ENDPOINT,
        update_endpoint=SPARQL_UPDATE_ENDPOINT,
    )
    
    # Initialise areal assessment
    total_bldgs, total_value = 0, 0
    
    buildings = [(query_affected, geojson_affected), 
                 (query_not_affected, geojson_not_affected)]

    for query, geojson in buildings:
        # Retrieve all affected building info
        property_location_dct = retrieve_property_locations(sparql_client, query)

        if geojson == geojson_affected:
            total_bldgs, total_value = get_assessment_for_area(property_location_dct)

        # Construct a list of BuildingPoint objects
        bldg_pts = [
            BuildingPoint(
                lat=float(property_location_dct[iri]['lat_long'].split('#')[0]),
                long=float(property_location_dct[iri]['lat_long'].split('#')[1]),
                value=None if not property_location_dct[iri]['value'] else int(property_location_dct[iri]['value']),
                iri=iri
            ) for iri in property_location_dct.keys()
        ]

        # Generate geojson file
        fp = os.path.join(Path(__file__).parent, 'visualisation','data', geojson)
        generate_geojson(bldg_pts, fp)

    # Create amended flood area geojson with updated assessment
    # To ensure correct formatting of GBP symbol suppress ascii encoding and use UTF-8
    with open(flood_original, encoding='utf-8') as f:
        area = json.load(f)
    total_value = round(total_value/1000000,2)
    area['features'][0]['properties']['Buildings at risk']['Number of buildings'] = total_bldgs
    area['features'][0]['properties']['Buildings at risk']['Estimated market value (£m)'] = total_value
    # To ensure correct formatting of GBP symbol suppress ascii encoding
    with open(flood_amended, 'w', encoding='utf-8') as f:
        f.write(json.dumps(area, ensure_ascii=False, indent=4))
    
