import os
from typing import cast
from flask import current_app

from agent.stack.postgis_client import PostGISClient

def create_ponits_table_self_defined_area(lng_start: float, lng_end: float, lng_step: float,
                        lat_start: float, lat_end: float, lat_step: float, point_table_name: str):
    postgis_client = cast(
        PostGISClient, current_app.extensions['postgis_client'])
    params = {
        "lng_start": lng_start,
        "lng_end": lng_end,
        "lng_step": lng_step,
        "lat_start": lat_start,
        "lat_end": lat_end,
        "lat_step": lat_step,
    }
    
    table_mapping = {"point_table_name": point_table_name}

    script_dir = os.path.dirname(os.path.abspath(__file__))
    regularly_sample_sql_path = os.path.join(script_dir, "script", "regularly_sample.sql")
    with open(regularly_sample_sql_path, "r") as file:
        postgis_client.execute_update(file.read(), table_mapping, params)


def create_points_table_provided_list(lng: float, lat: float) -> str:
    create_ponits_table_self_defined_area(lng_start=lng, lng_end=lng, lng_step=0.1,
                        lat_start=lat, lat_end=lat, lat_step=0.1)
