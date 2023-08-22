from marie.utils import advance_ptr_thru_space, advance_ptr_to_kw, advance_ptr_to_space
from marie.data_processing.abstract_query_rep import AbstractQueryRep
from marie.data_processing.utils import replace_multi


QUERY_ENCODINGS_BY_MODEL = dict(
    t5={
        "{": "op_br",
        "}": "cl_br",
        "?": "var_",
        "<": "ls_th",
        ">": "gr_th",
    },
    llama={},
)
QUERY_DECODINGS_BY_MODEL = {
    model: {v: k for k, v in encodings.items()}
    for model, encodings in QUERY_ENCODINGS_BY_MODEL.items()
}


def encode_query_special_chars(query: str, model_family: str):
    return replace_multi(query, QUERY_ENCODINGS_BY_MODEL[model_family])


def decode_query_special_chars(query: str, model_family: str):
    return replace_multi(query, QUERY_DECODINGS_BY_MODEL[model_family])


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


def preprocess_query(query: str, model_family: str):
    query = remove_prefixes(query)
    query = encode_query_special_chars(query, model_family=model_family)
    return query


def postprocess_query(query: str, model_family: str):
    query = decode_query_special_chars(query, model_family=model_family)
    try:
        query = AbstractQueryRep.from_string(query).compact2verbose().to_query_string()
    except:
        query = None
    return query


def normalize_query(query: str):
    for c in [".", ",", "{", "}", "(", ")", "<", ">", "&&", "||"]:
        query = query.replace(c, f" {c} ")

    return " ".join(query.split())
