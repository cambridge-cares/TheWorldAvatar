from marie.utils import advance_idx_thru_space, advance_idx_to_kw


QUERY_ENCODINGS = {"{": " op_br ", "}": " cl_br ", "?": "var_"}
QUERY_DECODINGS = {v.strip(): k for k, v in QUERY_ENCODINGS.items()}


def replace_multi(text: str, mapper: dict):
    for k, v in mapper.items():
        text = text.replace(k, v)
    return text


def encode_special_chars(query: str):
    return replace_multi(query, QUERY_ENCODINGS)


def decode_special_chars(query: str):
    return replace_multi(query, QUERY_DECODINGS)


def remove_prefixes(query: str):
    idx = advance_idx_to_kw(query, "PREFIX")
    if idx == len(query):
        return query

    while query.startswith("PREFIX", idx):
        "PREFIX prefix: <iri>"
        idx += len("PREFIX")
        idx = advance_idx_to_kw(query, ">", idx)
        idx += len(">")
        idx = advance_idx_thru_space(query, idx)

    return query[idx:]


def preprocess_query(query: str):
    query = remove_prefixes(query)
    query = encode_special_chars(query)
    return query


def postprocess_query(query: str):
    query = decode_special_chars(query)
    return query
