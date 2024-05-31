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
