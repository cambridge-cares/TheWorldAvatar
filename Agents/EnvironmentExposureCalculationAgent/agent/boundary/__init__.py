from typing import cast
from flask import current_app

from agent.stack.postgis_client import PostGISClient


def create_buffer_around_points(point_table_name: str, buffer_radius: float):
    postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
    with open("script/buffer.sql", "r") as file:
        postgis_client.execute_query(file.read(), {"buffer_radius": buffer_radius, "point_table_name": point_table_name})