import json

import pandas as pd


def read_json(json_filepath: str):
    """
    A test function to read the contents of a tileset.json

    Argument:
    json_filepath - File path to the tileset.json
    Returns:
    The tileset's contents as a Python dictionary
    """
    # Read the results
    with open(json_filepath, "r", encoding="utf-8") as f:
        data = json.load(f)
    return data


def gen_sample_asset_df(test_range: int):
    """
    A test function to generate sample datafarame for the
    test of gen_tileset_assets()

    Argument:
    test_range - number of assets to generated for testing
    Returns:
    The sample dataframe
    """
    rows = [
        {
            "uid": "uid" + str(i),
            "name": "element" + str(i),
            "iri": "iri" + str(i),
            "file": "asset" + str(i)
        } for i in range(test_range)
    ]
    return pd.DataFrame(rows)