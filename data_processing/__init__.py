from data_processing.compact2verbose import SparqlCompact2VerboseConverter
from utils import replace_multi


T5_INPUT_PREFIX = "translate to SPARQL: "
T5_NL_ENCODINGS = {
    "<": "&lt;",
    ">": "&gt;",
}
T5_INPUT_DECODINGS = {v: k for k, v in T5_NL_ENCODINGS.items()}


def t5_encode_nl_special_chars(text: str):
    return replace_multi(text, T5_NL_ENCODINGS)


def preprocess_nl(text: str):
    text = t5_encode_nl_special_chars(text)
    text = T5_INPUT_PREFIX + text
    return text


T5_SPARQL_ENCODINGS = {
    "?": "var_",
    "{": "&lcub;",
    "}": "&rcub;",
    "<": "&lt;",
    ">": "&gt;",
}
T5_OUTPUT_DECODINGS = {v: k for k, v in T5_SPARQL_ENCODINGS.items()}


MT0_OUTPUT_ENCODINGS = {"?": "var_"}
MT0_OUTPUT_DECODINGS = {v: k for k, v in MT0_OUTPUT_ENCODINGS.items()}


def remove_prefixes(text: str):
    # PREFIX prefix: <iri>
    while True:
        idx = text.find("PREFIX")
        if idx < 0:
            break
        
        idx = text.find(">", idx)
        text = text[idx + 1:]

    return text


def preprocess_sparql(text: str):
    text = remove_prefixes(text)
    text = replace_multi(text, T5_SPARQL_ENCODINGS)
    return text


def postprocess_sparql(text: str):
    text = replace_multi(text, T5_OUTPUT_DECODINGS)
    return text


converter = SparqlCompact2VerboseConverter()

def compact2verbose(text: str):
    return converter.convert(text)


def normalize_query(query: str):
    query = query.replace(".", " .").replace('("', '( "').replace('")', '" )')
    return " ".join(query.split())
