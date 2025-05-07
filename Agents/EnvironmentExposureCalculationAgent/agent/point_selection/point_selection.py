import os
from typing import cast
from flask import current_app

from agent.stack.postgis_client import PostGISClient


script_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "script")


def create_ponits_table_self_defined_area(lng_start: float, lng_end: float, lng_step: float,
                                          lat_start: float, lat_end: float, lat_step: float, query_id: str):
    params = {
        "lng_start": lng_start,
        "lng_end": lng_end,
        "lng_step": lng_step,
        "lat_start": lat_start,
        "lat_end": lat_end,
        "lat_step": lat_step,
    }

    table_mapping = {"query_id": query_id}

    sql_path = os.path.join(script_dir, "regularly_sample.sql")
    with open(sql_path, "r") as file:
        postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
        postgis_client.execute_update(file.read(), table_mapping, params)


def create_points_table_provided_list(lng: float, lat: float) -> str:
    pass


def create_points_table_postal_code(query_id: str):
    sql_path = os.path.join(script_dir, "postal_code.sql")
    with open(sql_path, "r") as file:
        postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
        postgis_client.execute_update(file.read(), val_params={"query_id": query_id})
