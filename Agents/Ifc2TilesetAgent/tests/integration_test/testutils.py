# Standard import
import json
from typing import Iterable

# Third-party import
import pandas as pd
from pandas.testing import assert_frame_equal

# Self import
from agent.kgutils import KGClient
from tests.integration_test.testconsts import prefix
from . import testconsts as C


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


def read_json_file(json_filepath: str):
    with open(json_filepath, "r", encoding="utf-8") as f:
        content = json.load(f)
    return content
