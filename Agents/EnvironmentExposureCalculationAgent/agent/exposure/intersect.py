import os
from typing import cast
from flask import current_app
import pandas as pd
from agent.stack.postgis_client import PostGISClient


def intersect(points_table_name: str):
    postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
    script_dir = os.path.dirname(os.path.abspath(__file__))
    res = pd.DataFrame()
    
    city_building_intersect_sql_path = os.path.join(script_dir, "script_sql", "city_building_intersect.sql")
    with open(city_building_intersect_sql_path, "r") as file:
        res = pd.concat([res, postgis_client.execute_query(file.read(), {"point_table_name": points_table_name})], axis=0, ignore_index=True)
        
    culture_intersect_sql_path = os.path.join(script_dir, "script_sql", "culture_intersect.sql")
    with open(culture_intersect_sql_path, "r") as file:
        res = pd.concat([res, postgis_client.execute_query(file.read(), {"point_table_name": points_table_name})], axis=0, ignore_index=True)
    
    osm_intersect_sql_path = os.path.join(script_dir, "script_sql", "osm_intersect.sql")
    with open(osm_intersect_sql_path, "r") as file:
        res = pd.concat([res, postgis_client.execute_query(file.read(), {"point_table_name": points_table_name})], axis=0, ignore_index=True)