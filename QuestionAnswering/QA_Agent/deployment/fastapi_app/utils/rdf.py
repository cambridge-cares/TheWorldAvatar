from typing import List


def extract_name(iri: str):
    if "#" in iri:
        iri = iri.rsplit("#", maxsplit=1)[-1]
    if "/" in iri:
        iri = iri.rsplit("/", maxsplit=1)[-1]
    return iri


def flatten_sparql_response(res: dict):
    vars: List[str] = res["head"]["vars"]
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in res["results"]["bindings"]
    ]
    return vars, bindings
