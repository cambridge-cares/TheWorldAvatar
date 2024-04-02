"""
# Author: qhouyee, picas9dan #

This file contains util functions for integration tests.
"""

# Standard library imports
import json
import os
from typing import Iterable

# Third-party imports
import pandas as pd
from pandas.testing import assert_frame_equal
import yaml

# Self imports
from agent.kgutils import KGClient
from tests.integration_test.testconsts import prefix
from . import testconsts as C


def assert_assets_present(assets: Iterable[str]):
    for asset in assets:
        asset_path = "./data/glb/" + asset + ".glb"
        assert os.path.isfile(asset_path)


def triples_to_insert_query(triples: Iterable[str]):
    return prefix + "INSERT DATA {\n" + '\n'.join(triples) + '}'


def init_kg_client(kg_client: KGClient, init_assets: Iterable[str]):
    triples = [C.SAMPLE_ONTOBIM_TRIPLESTORE[asset]
               for asset in init_assets
               if asset in C.SAMPLE_ONTOBIM_TRIPLESTORE]

    kg_client.execute_update(triples_to_insert_query(triples))


def _sort_and_reset_index(df: pd.DataFrame):
    return df.sort_values(by=df.columns.tolist()).reset_index(drop=True)


def assert_df_equal(left: pd.DataFrame, right: pd.DataFrame):
    return assert_frame_equal(_sort_and_reset_index(left), _sort_and_reset_index(right), check_dtype=False)


def overwrite_yaml(sample_bim_iri="", sample_bim_name="", sample_solar_iri="", 
                   sample_solar_name="", sample_sewage_iri="", sample_sewage_name=""):
    # Create a sample yaml file with required properties for retrieval
    yaml_path = C.SAMPLE_YAML_PATH
    data = dict(
        query_endpoint=C.KG_ENDPOINT,
        update_endpoint=C.KG_ENDPOINT,
        bim_tileset_iri=sample_bim_iri,
        bim_tileset_name=sample_bim_name,
        solar_panel_tileset_iri=sample_solar_iri,
        solar_panel_tileset_name=sample_solar_name,
        sewage_tileset_iri=sample_sewage_iri,
        sewage_tileset_name=sample_sewage_name
    )
    with open(yaml_path, 'w') as outfile:
        yaml.dump(data, outfile)


def read_json_file(json_filepath: str):
    with open(json_filepath, "r", encoding="utf-8") as f:
        content = json.load(f)
    return content
