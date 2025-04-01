import os
from typing import cast
from flask import current_app
import numpy as np
import pandas as pd

from agent.stack.postgis_client import PostGISClient
from agent.utils.table_name_helper import TableNameHelper

def save_to_table(res: pd.DataFrame, table_name_helper: TableNameHelper,):
    postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
    
    counts = res.groupby(['pointid', 'type']).size().reset_index(name='count')
    counts.rename(columns={"pointid": "id"}, inplace=True)
    counts['type'] = counts['type'].map(lambda s: s.split('/')[-1].lower())
    add_columns = ' '.join([f'ALTER TABLE "env_exposure"."%(point_table_name)s" ADD COLUMN IF NOT EXISTS "{type}" INTEGER DEFAULT 0;' for type in counts['type'].unique()])
    
    counts_pivot = counts.pivot(index="id", columns="type", values="count").fillna(0).astype(int).reset_index()
    columns = list(counts_pivot.columns)
    update_columns = [f'"{col}" = EXCLUDED."{col}"' for col in columns if col != "id"]
    insert_sql = f"""
        INSERT INTO "env_exposure"."%(point_table_name)s" ({', '.join(f'"{col}"' for col in columns)}) 
        VALUES ({', '.join(['%s'] * len(columns))})
        ON CONFLICT (id) 
        DO UPDATE SET {', '.join(update_columns)};
        """
        
    vals = [
            tuple(int(val) if isinstance(val, np.int64) else val for val in row)
            for row in counts_pivot.to_records(index=False)
    ]
    postgis_client.execute_updatemany(add_columns + insert_sql, 
                                    {"point_table_name": table_name_helper.get_points_table_name()}, 
                                    vals)