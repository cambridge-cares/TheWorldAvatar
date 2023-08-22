from marie.data_processing.utils import replace_multi

LLAMA_PROMPT_TEMPLATE = "translate to SPARQL: {question}\n\n"
LLAMA_COMPLETION_TEMPLATE = "###\n\n"

T5_INPUT_PREFIX = "translate to SPARQL: "
T5_QN_ENCODINGS = {
    "<": "ls_th",
    ">": "gt_th",
}
T5_QN_DECODINGS = {v: k for k, v in T5_QN_ENCODINGS.items()}


def t5_encode_qn_special_chars(qn: str):
    return replace_multi(qn, T5_QN_ENCODINGS)


def t5_decode_qn_special_chars(query: str):
    return replace_multi(query, T5_QN_DECODINGS)


def t5_preprocess_qn(qn: str):
    # TODO: convert units
    qn = t5_encode_qn_special_chars(qn)
    qn = T5_INPUT_PREFIX + qn
    return qn
