import os
from typing import cast
from flask import current_app
from agent.stack.postgis_client import PostGISClient


def create_buffer_around_points(query_id: str, buffer_radius: float):
    """
    Create buffer around input points for the query_id.
    Store the result as column in "env_exposure"."point_table" for the input points.
    
    Parameters
    ----------
    query_id : str
        The query_id used to filter the input points relevant to the query in `env_exposure.point_table`.

    buffer_radius : float
        The radius (in meters or appropriate spatial units) to use for the buffer geometry
    
    """
    postgis_client = cast(
        PostGISClient, current_app.extensions['postgis_client'])

    script_dir = os.path.dirname(os.path.abspath(__file__))
    buffer_sql_path = os.path.join(script_dir, "script", "buffer.sql")
    with open(buffer_sql_path, "r") as file:
        postgis_client.execute_update(
            file.read(), None, {"buffer_radius": buffer_radius, "query_id": query_id})
