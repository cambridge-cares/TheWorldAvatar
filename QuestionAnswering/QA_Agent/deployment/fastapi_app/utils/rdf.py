from typing import List

from constants.prefixes import PREFIX_NAME2URI


def extract_name(iri: str):
    if "#" in iri:
        iri = iri.rsplit("#", maxsplit=1)[-1]
    if "/" in iri:
        iri = iri.rsplit("/", maxsplit=1)[-1]
    return iri


def try_make_prefixed_iri(iri: str):
    for prefix_name, prefix_uri in PREFIX_NAME2URI.items():
        if iri.startswith(prefix_uri):
            return "{prefix}:{name}".format(
                prefix=prefix_name, name=iri[len(prefix_uri) :]
            )
    return iri

def flatten_sparql_response(res: dict):
    vars: List[str] = res["head"]["vars"]
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in res["results"]["bindings"]
    ]
    return vars, bindings
