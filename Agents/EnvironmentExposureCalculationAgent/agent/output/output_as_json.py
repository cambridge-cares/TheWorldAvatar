from typing import cast
from flask import current_app
import pandas as pd

from agent.stack.postgis_client import PostGISClient
from agent.utils.table_name_helper import TableNameHelper

def output_as_json(res: pd.DataFrame, table_name_helper: TableNameHelper,):
    postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
    
    summary_table = f'select * from "env_exposure"."%(point_table_name)s"'
    summary = postgis_client.execute_query(summary_table, {"point_table_name": table_name_helper.get_points_table_name()}).drop(columns=['buffer_geom'])
    
    return {"summary": summary.to_dict(orient='records'), "exposures": res.to_dict(orient='records')}