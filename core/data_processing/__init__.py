T5_PREFIX_NL2SPARQL = "translate to SPARQL: "
T5_PREFIX_CLSDOMAIN = "classify the domain of the query: "

T5_NL_ENCODINGS = {"<": "&lt;", "<=": "&le;", ">": "&gt;", ">=": "&ge;"}
T5_INPUT_DECODINGS = {v: k for k, v in T5_NL_ENCODINGS.items()}


def t5_encode_nl_special_chars(text: str):
    return replace_multi(text, T5_NL_ENCODINGS)


def preprocess_nl(text: str):
    text = t5_encode_nl_special_chars(text)
    return text


T5_SPARQL_ENCODINGS = {
    **T5_NL_ENCODINGS,
    **{
        "?": "var_",
        "{": "&lcub;",
        "}": "&rcub;",
    },
}
T5_OUTPUT_DECODINGS = {v: k for k, v in T5_SPARQL_ENCODINGS.items()}


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
    text = replace_multi(text, T5_OUTPUT_DECODINGS)
    return text


def normalize_query(query: str):
    query = query.replace(".", " .").replace('("', '( "').replace('")', '" )')
    return " ".join(query.split())


def replace_multi(text: str, mapper: dict):
    for k, v in mapper.items():
        text = text.replace(k, v)
    return text
