import os
from typing import cast
from flask import current_app

from agent.stack.postgis_client import PostGISClient


def create_buffer_around_points(point_table_name: str, buffer_radius: float):
    postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
    
    script_dir = os.path.dirname(os.path.abspath(__file__))
    buffer_sql_path = os.path.join(script_dir, "script", "buffer.sql")
    with open(buffer_sql_path, "r") as file:
        postgis_client.execute_update(file.read(), {"point_table_name": point_table_name}, {"buffer_radius": buffer_radius})