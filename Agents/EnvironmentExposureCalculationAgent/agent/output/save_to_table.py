import os
from typing import cast
from flask import current_app

from agent.stack.postgis_client import PostGISClient

def save_to_table(query_id: str,):
    postgis = cast(PostGISClient, current_app.extensions['postgis_client'])
    
    # save area calculation results
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "script_sql", "area_summary.sql")
    with open(path) as file:
        postgis.execute_update(file.read(), val_params={'query_id': query_id})
        
    # TODO: save points result
    