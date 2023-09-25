from core.utils import advance_ptr_thru_space, advance_ptr_to_kw
from core.data_processing.abstract_query_rep import AbstractQueryRep
from core.data_processing.compact_query_rep import CompactQueryRep
from core.data_processing.utils import replace_multi


T5_OUTPUT_ENCODINGS = {
    "?": "var_",
    "{": "&lcub;",
    "}": "&rcub;",
    "<": "&lt;",
    ">": "&gt;",
}
T5_OUTPUT_DECODINGS = {v: k for k, v in T5_OUTPUT_ENCODINGS.items()}


def t5_encode_output_special_chars(text: str):
    return replace_multi(text, T5_OUTPUT_ENCODINGS)


def t5_decode_output_special_chars(text: str):
    return replace_multi(text, T5_OUTPUT_DECODINGS)


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
        text = t5_encode_output_special_chars(text)
    return text


def postprocess_output(text: str, model_family: str):
    if model_family == "t5":
        text = t5_decode_output_special_chars(text)
    elif model_family == "llama" and "\n\n###\n\n" in text:
            text = text.split("\n\n###\n\n", maxsplit=1)[1]
    return text

def compact2verbose(text: str):
    try:
        # return AbstractQueryRep.from_string(text).compact2verbose().to_query_string()
        return CompactQueryRep.from_string(text).to_verbose()
    except:
        return None


def normalize_query(query: str):
    for c in [".", ",", "{", "}", "(", ")", "<", ">", "&&", "||"]:
        query = query.replace(c, f" {c} ")

    return " ".join(query.split())
