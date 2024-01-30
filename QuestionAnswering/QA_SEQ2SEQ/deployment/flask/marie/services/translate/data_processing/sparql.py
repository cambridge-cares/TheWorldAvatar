from .constants import T5_SPARQL_DECODINGS, T5_SPARQL_ENCODINGS
from .utils import replace_multi


def remove_prefixes(text: str):
    # PREFIX prefix: <iri>
    while True:
        idx = text.find("PREFIX")
        if idx < 0:
            break

        idx = text.find(">", idx)
        text = text[idx + 1 :]

    return text


def preprocess_sparql(text: str):
    text = remove_prefixes(text)
    text = replace_multi(text, T5_SPARQL_ENCODINGS)
    return text


def postprocess_sparql(text: str):
    text = replace_multi(text, T5_SPARQL_DECODINGS)
    return text
