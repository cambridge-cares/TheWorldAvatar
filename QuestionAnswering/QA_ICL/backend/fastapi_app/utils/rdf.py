from constants.namespace import PREFIX2URI


def extract_name(iri: str):
    if "#" in iri:
        iri = iri.rsplit("#", maxsplit=1)[-1]
    if "/" in iri:
        iri = iri.rsplit("/", maxsplit=1)[-1]
    return iri


def try_make_prefixed_iri(iri: str):
    for prefix, uri in PREFIX2URI.items():
        if iri.startswith(uri):
            return "{prefix}:{name}".format(prefix=prefix, name=iri[len(uri) :])
    return iri


def filter_deep_remove_iris(
    x: dict | list | tuple | str | float, iri_prefixes: list[str] | tuple[str]
):
    if isinstance(x, dict):
        return filter_deep_remove_iris_from_dict(x, iri_prefixes=iri_prefixes)
    elif isinstance(x, list) or isinstance(x, tuple):
        return filter_deep_remove_iris_from_list(x, iri_prefixes=iri_prefixes)
    else:
        return x


def filter_deep_remove_iris_from_list(
    lst: list | tuple, iri_prefixes: list[str] | tuple[str]
):
    return [filter_deep_remove_iris(x, iri_prefixes=iri_prefixes) for x in lst]


def filter_deep_remove_iris_from_dict(doc: dict, iri_prefixes: list[str] | tuple[str]):
    return {
        k: filter_deep_remove_iris(v, iri_prefixes=iri_prefixes)
        for k, v in doc.items()
        if not (
            isinstance(v, str) and any(v.startswith(prefix) for prefix in iri_prefixes)
        )
    }
