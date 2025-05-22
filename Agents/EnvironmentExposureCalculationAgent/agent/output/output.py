import io
import os
from typing import cast
import zipfile
from flask import abort, current_app
import pandas as pd
from agent.config.params import OutputFormatParam, PointSelectionParam
from agent.stack.postgis_client import PostGISClient


def get_output(output_format: str, point_select: str, query_id: str) -> str:
    if not output_format:
        return query_id

    summary_df = get_summary_df(point_select, query_id)
    exposure_details = get_exposure_details()

    if output_format == OutputFormatParam.CSV.value:
        return output_as_zip_of_csv(summary_df, exposure_details)
    elif output_format == OutputFormatParam.JSON.value:
        return output_as_json()
    else:
        abort(400, description="Unsupported output format")


def get_summary_df(point_selection: str, query_id: str) -> pd.DataFrame:
    postgis = cast(PostGISClient, current_app.extensions['postgis_client'])

    def get_query() -> str:
        # Get input points information from the original table and combine with the aggregated results
        # Since each type of input data are retrieved differently, will need scripts for different types
        # TODO: ideally construct the summary retrieval SQL dynamically, so it is (input points information retrieval) + (aggregated result retrieval). (aggregated result retrieval) can be shared by different types, and reduce duplication in each script file.
        if point_selection == PointSelectionParam.PostalCode.value:
            path = os.path.join(os.path.dirname(
                os.path.abspath(__file__)), 'script_sql', 'retrieve_postal_code_result.sql')
            with open(path) as file:
                return file.read()
        else:
            return 'select * from "env_exposure"."point_table";'

    query = get_query()
    return postgis.execute_query(query, table_mappings=None, val_params={'query_id': query_id})


def get_exposure_details() -> pd.DataFrame:
    # TODO: get the details (eg. name, description, address) of intersected objects for each input point
    return pd.DataFrame()


def output_as_json(summary_df: pd.DataFrame, exposure_details: pd.DataFrame):
    return {"summary": summary_df.to_dict(orient='records'), "exposures": exposure_details.to_dict(orient='records')}


def output_as_zip_of_csv(summary_df: pd.DataFrame, exposure_details: pd.DataFrame):
    summary_file = os.path.join(os.path.dirname(
        os.path.abspath(__file__)), 'output_files', 'summary.csv')
    summary_df.to_csv(summary_file, index=False)

    exposure_details_file = os.path.join(os.path.dirname(
        os.path.abspath(__file__)), 'output_files', 'exposure_details.csv')
    exposure_details.to_csv(exposure_details_file, index=False)

    zip_buffer = io.BytesIO()
    with zipfile.ZipFile(zip_buffer, 'w') as zip_file:
        zip_file.write(summary_file, arcname='summary.csv')
        zip_file.write(exposure_details_file, arcname='exposure_details.csv')

    zip_buffer.seek(0)
    return zip_buffer
