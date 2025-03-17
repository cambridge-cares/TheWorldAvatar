from typing import cast
from flask import current_app
from agent.stack.postgis_client import PostGISClient


def intersect(points_table_name: str):
    postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
    with open("script/city_building_intersect.sql", "r") as file:
        postgis_client.execute_query(file.read(), {"point_table_name": points_table_name})
        
    with open("script/culture_intersect.sql", "r") as file:
        postgis_client.execute_query(file.read(), {"point_table_name": points_table_name})
        
    with open("script/osm_intersect.sql", "r") as file:
        postgis_client.execute_query(file.read(), {"point_table_name": points_table_name})