from typing import List, Tuple


def extract_name(iri: str):
    if "#" in iri:
        iri = iri.rsplit("#", maxsplit=1)[-1]
    if "/" in iri:
        iri = iri.rsplit("/", maxsplit=1)[-1]
    return iri


def linearize_node(subj: str, tails: List[Tuple[str, str]]):
    linearized_tails = "\n".join(
        ["{p}: {o}".format(p=extract_name(p), o=extract_name(o)) for p, o in tails]
    )
    return "{subj}\n{tails}".format(subj=extract_name(subj), tails=linearized_tails)
