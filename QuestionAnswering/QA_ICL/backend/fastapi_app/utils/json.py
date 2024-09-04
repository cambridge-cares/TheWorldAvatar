import numpy as np
import pandas as pd


def _make_key(outer_key: str, inner_key: str):
    return "{outer_key}{sep}{inner_key}".format(
        outer_key=outer_key, sep="." if outer_key else "", inner_key=inner_key
    )


def flatten_dict(obj: dict):
    out = {}

    def flatten(x, key=""):
        if isinstance(x, dict):
            for inner_key in x.keys():
                flatten(x[inner_key], key=_make_key(outer_key=key, inner_key=inner_key))
        elif isinstance(x, list):
            for i, elem in enumerate(x):
                flatten(elem, key=_make_key(outer_key=key, inner_key=str(i)))
        else:
            out[key] = x

    flatten(obj)
    return out


def deep_pd_json_normalize(obj):
    if isinstance(obj, list) or isinstance(obj, tuple):
        return deep_pd_json_normalize_list(obj)
    elif isinstance(obj, dict):
        return deep_pd_json_normalize_dict(obj)
    else:
        return obj


def deep_pd_json_normalize_dict(doc: dict):
    if not doc:
        return {}
    return (
        pd.json_normalize({k: deep_pd_json_normalize(v) for k, v in doc.items()})
        .replace({np.nan: None})
        .to_dict("records")[0]
    )


def deep_pd_json_normalize_list(lst: list | tuple):
    lst = [deep_pd_json_normalize(x) for x in lst]
    if all(isinstance(x, dict) for x in lst):
        return pd.json_normalize(lst).replace({np.nan: None}).to_dict("records")
    else:
        return lst
