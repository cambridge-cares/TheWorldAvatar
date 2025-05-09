import os
from typing import cast
from flask import current_app
import numpy as np
import pandas as pd

from agent.stack.postgis_client import PostGISClient
from agent.utils.table_name_helper import QueryIdHelper

def save_to_table(table_name_helper: QueryIdHelper,):
    postgis = cast(PostGISClient, current_app.extensions['postgis_client'])
    
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "script_sql", "area_summary.sql")
    with open(path) as file:
        postgis.execute_update(file.read(), val_params={'query_id': table_name_helper.get_query_id()})
    