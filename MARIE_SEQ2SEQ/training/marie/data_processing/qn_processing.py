from marie.data_processing.utils import replace_multi


TRANS_MODEL_INPUT_PREFIX = "translate to SPARQL: "

QN_ENCODINGS = {
    "<": "ls_th",
    ">": "gt_th",
}

QN_DECODINGS = {v: k for k, v in QN_ENCODINGS.items()}


def encode_qn_special_chars(query: str):
    return replace_multi(query, QN_ENCODINGS)


def decode_qn_special_chars(query: str):
    return replace_multi(query, QN_DECODINGS)


def preprocess_qn(qn: str):
    qn = TRANS_MODEL_INPUT_PREFIX + qn
    # convert units
    return qn
