from core.constants import FAMILY_CAUSAL
from core.data_processing.exceptions import InvalidCompactQueryError
from core.utils import advance_ptr_thru_space, advance_ptr_to_kw
from core.data_processing.abstract_query_rep import AbstractQueryRep
from core.data_processing.compact_query.compact_query_rep import CompactQueryRep
from core.data_processing.utils import replace_multi


CAUSALLM_RESPONSE_TEMPLATE = "### Response:\n"

T5_OUTPUT_ENCODINGS = {
    "?": "var_",
    "{": "&lcub;",
    "}": "&rcub;",
    "<": "&lt;",
    ">": "&gt;",
}
T5_OUTPUT_DECODINGS = {v: k for k, v in T5_OUTPUT_ENCODINGS.items()}


MT0_OUTPUT_ENCODINGS = {"?": "var_"}
MT0_OUTPUT_DECODINGS = {v: k for k, v in MT0_OUTPUT_ENCODINGS.items()}


def remove_prefixes(text: str):
    idx = advance_ptr_to_kw(text, "PREFIX")
    if idx == len(text):
        return text

    while text.startswith("PREFIX", idx):
        # PREFIX prefix: <iri>
        idx += len("PREFIX")
        idx = advance_ptr_to_kw(text, ">", idx)
        idx += len(">")
        idx = advance_ptr_thru_space(text, idx)

    return text[idx:]


def preprocess_output(text: str, model_family: str):
    text = remove_prefixes(text)
    if model_family == "t5":
        text = replace_multi(text, T5_OUTPUT_ENCODINGS)
    elif model_family == "mt0":
        text = replace_multi(text, MT0_OUTPUT_ENCODINGS)
    return text


def postprocess_output(text: str, model_family: str):
    if model_family == "t5":
        text = replace_multi(text, T5_OUTPUT_DECODINGS)
    elif model_family == "mt0":
        text = replace_multi(text, MT0_OUTPUT_DECODINGS)
    elif model_family in FAMILY_CAUSAL and CAUSALLM_RESPONSE_TEMPLATE in text:
        text = text.split(CAUSALLM_RESPONSE_TEMPLATE, maxsplit=1)[1]
    return text


def compact2verbose(text: str):
    try:
        # return AbstractQueryRep.from_string(text).compact2verbose().to_query_string()
        return CompactQueryRep.from_string(text).to_verbose()
    except Exception as e:
        if not isinstance(e, InvalidCompactQueryError):
            print("An unhandled error is encountered when parsing a compact query.")
            print(e)
        return None


def normalize_query(query: str):
    # for c in [".", ",", "{", "}", "(", ")", "<", ">", "&&", "||"]:
    #     query = query.replace(c, f" {c} ")
    query = query.replace(".", " .").replace('("', '( "').replace('")', '" )')
    return " ".join(query.split())
