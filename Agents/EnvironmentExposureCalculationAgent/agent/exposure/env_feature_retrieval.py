import os
from typing import cast
from flask import current_app
import pandas as pd
from agent.exposure.constants import ScriptName
from agent.stack.ontop_client import OntopClient
from agent.stack.postgis_client import PostGISClient

# NOTICE: this is template file for getting culture env feature. Should be redesigned after a proper ontology is done.
def fetch_env_features(obj_class: list = None, **kwargs):
    kg_script_dir = os.path.join(os.path.dirname(
        os.path.abspath(__file__)), 'script_kg')
    scripts = {
        ScriptName.Culture: os.path.join(kg_script_dir, 'culture.sparql'),
        # ScriptName.Facility: os.path.join(kg_script_dir, 'facility.sparql'),
        ScriptName.Greenspace: os.path.join(
            kg_script_dir, 'greenspace.sparql')
    }

    if not obj_class:
        obj_class = scripts.keys()

    res = pd.DataFrame()
    for script in [scripts[k] for k in obj_class]:
        with open(script, "r") as file:
            ontop_client = cast(
                OntopClient, current_app.extensions['ontop_client'])
            res = pd.concat(
                [res, pd.DataFrame(ontop_client.performQuery(file.read()))])

    if kwargs.get('save_feature_table') == True:
        if not kwargs.get('save_feature_table'):
            return res

        save_to_table(res, kwargs.get('feature_table').value)
    return res


def save_to_table(res: pd.DataFrame, feature_table: str):
    script_name = os.path.join(os.path.dirname(os.path.abspath(
        __file__)), 'script_sql', 'save_to_temp_table.sql')
    with open(script_name) as file:
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
