from typing import Dict, List

from services.core.label_store import LabelStore


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


def add_label_to_sparql_resposne(
    label_store: LabelStore, vars: List[str], bindings: List[Dict[str, str]], label_header: str = "label"
):
    try:
        iri_idx = vars.index("IRI")
    except ValueError:
        raise ValueError("IRI must be present, found: " + str(vars))

    vars.insert(iri_idx + 1, label_header)
    for binding in bindings:
        if "IRI" not in binding:
            raise ValueError("IRI key not found in binding: " + binding)
        
        labels = label_store.lookup_labels(binding["IRI"])
        if labels:
            binding[label_header] = labels[0]
