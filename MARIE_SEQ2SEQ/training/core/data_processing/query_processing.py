from core.utils import advance_ptr_thru_space, advance_ptr_to_kw, advance_ptr_to_space
from core.data_processing.abstract_query_rep import AbstractQueryRep
from core.data_processing.utils import replace_multi


T5_QUERY_ENCODINGS = {
    "{": "op_br",
    "}": "cl_br",
    "?": "var_",
    "<": "ls_th",
    ">": "gr_th",
}
T5_QUERY_DECODINGS = {v: k for k, v in T5_QUERY_ENCODINGS.items()}


def t5_encode_query_special_chars(query: str):
    return replace_multi(query, T5_QUERY_ENCODINGS)


def t5_decode_query_special_chars(query: str):
    return replace_multi(query, T5_QUERY_DECODINGS)


def remove_prefixes(query: str):
    idx = advance_ptr_to_kw(query, "PREFIX")
    if idx == len(query):
        return query

    while query.startswith("PREFIX", idx):
        # PREFIX prefix: <iri>
        idx += len("PREFIX")
        idx = advance_ptr_to_kw(query, ">", idx)
        idx += len(">")
        idx = advance_ptr_thru_space(query, idx)

    return query[idx:]


def t5_preprocess_query(query: str):
    query = remove_prefixes(query)
    query = t5_encode_query_special_chars(query)
    return query


def postprocess_query(query: str, model_family: str):
    if model_family == "t5":
        query = t5_decode_query_special_chars(query)
    try:
        query = AbstractQueryRep.from_string(query).compact2verbose().to_query_string()
    except:
        query = None
    return query


def normalize_query(query: str):
    for c in [".", ",", "{", "}", "(", ")", "<", ">", "&&", "||"]:
        query = query.replace(c, f" {c} ")

    return " ".join(query.split())
