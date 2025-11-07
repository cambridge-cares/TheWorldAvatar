import os
from typing import cast
from flask import current_app
import pandas as pd
from agent.exposure.constants import KGScript
from agent.stack.ontop_client import OntopClient
from agent.stack.postgis_client import PostGISClient

# TODO: this is template file for getting culture env feature. Should be redesigned after a proper ontology is done.
def fetch_env_features(scripts: list[KGScript] = None, **kwargs):
    """
    Fetch environment features of requested object classes from knowledge graph with predefined scripts.
    Each SPARQL query is expected to return records containing `iri`, `geom`, and `type_label`.
    
    Parameters
    ----------
    scripts : list of Script, optional
        List of script enums to run (e.g., [ScriptName.Greenspace]). If None, all
        available scripts will be executed.

    **kwargs :
        save_feature_table : bool, optional
            If True, the function will save the results to a PostGIS table.
        feature_table : FeatureTableEnum or str, required if `save_feature_table` is True
            The name or enum value of the table where results should be saved.
    
    Notes
    -----
    - SPARQL scripts are expected to be stored in the `script_kg` directory.
    - The function uses the `ontop_client` to perform SPARQL queries.
    - Saving to the database assumes geometries are compatible with SRID 4326 (see `_save_to_table`).
    """

    if not scripts:
        scripts = list(KGScript)

    res = pd.DataFrame()
    for script in scripts:
        with open(script.value, "r") as file:
            ontop_client = cast(
                OntopClient, current_app.extensions['ontop_client'])
            res = pd.concat(
                [res, pd.DataFrame(ontop_client.performQuery(file.read()))])

    if kwargs.get('save_feature_table') == True:
        if not kwargs.get('save_feature_table'):
            return

        _save_to_table(res, kwargs.get('feature_table').value)
    return


def _save_to_table(res: pd.DataFrame, feature_table: str):
    """
    Saves geospatial features from a DataFrame to a PostGIS table.

    Reads a SQL insert script and bulk inserts rows into the specified table.
    Expects the DataFrame to include `iri`, `geom` (SRID 4326), and `type_label`.
    Does nothing if the DataFrame is empty.

    Parameters
    ----------
    res : pd.DataFrame
        DataFrame with `iri`, `geom`, and `type_label` columns.

    feature_table : str
        Name of the target PostGIS table.
    """
    
    script_name = os.path.join(os.path.dirname(os.path.abspath(
        __file__)), 'script_sql', 'save_to_temp_table.sql')
    with open(script_name) as file:
        if res.empty:
            return
        
        values = [tuple(row.values)
                  for _, row in res[['iri', 'geom', 'type_label']].iterrows()]
        query = file.read()

        postgis_client = cast(
            PostGISClient, current_app.extensions['postgis_client'])
        # TODO: assume all geom of SRID 4326
        postgis_client.execute_updatemany(query,
                                          feature_table,
                                          values,
                                          "(%s, %s, %s)"
                                          )
