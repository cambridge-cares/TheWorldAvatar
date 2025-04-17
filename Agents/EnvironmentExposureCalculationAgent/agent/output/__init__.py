from flask import abort
import pandas as pd
from agent.output.output_as_json import output_as_json
from agent.utils.table_name_helper import TableNameHelper


def get_output(output_format: str, res:pd.DataFrame, table_name_helper:TableNameHelper):
    if output_format == 'csv':
        pass
    elif output_format == 'json':
        return output_as_json(res, table_name_helper)
    else:
        abort(400, description="Unsupported output format")