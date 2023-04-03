"""
# Author: qhouyee #

A test suite for the agent.ifc2gltf.ifchelper submodule.
"""

# Third party import
import pandas as pd
import pytest

# Self import
from agent.ifc2gltf.ifchelper import get_filename_to_ifc_ids_mapping


@pytest.mark.parametrize(
    "df, expected", 
    [(
        pd.DataFrame(columns=["file", "uid", "iri", "name"]),
        {}
    ), (
        pd.DataFrame(data=dict(file=["furniture", "furniture", "asset1", "asset2"], uid=["id1", "id2", "id3", "id4"])),
        {"furniture": ["id1", "id2"], "asset1": ["id3"], "asset2": ["id4"]}
    )]
)
def test_get_filename_to_ifc_ids_mapping(df, expected):
    actual = get_filename_to_ifc_ids_mapping(df)
    assert actual == expected
