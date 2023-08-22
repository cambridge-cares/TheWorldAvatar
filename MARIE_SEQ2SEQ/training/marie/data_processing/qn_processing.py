from marie.data_processing.utils import replace_multi


T5_INPUT_PREFIX = "translate to SPARQL: "

INPUT_TEMPLATE_BY_MODEL = dict(
    t5="translate to SPARQL: {input}",
    llama="translate to SPARQL: {input}\n\n###\n\n"
)

QN_ENCODINGS_BY_MODEL = dict(
    t5={
        "<": "ls_th",
        ">": "gt_th",
    },
    llama={},
)

QN_DECODINGS_BY_MODEL = {
    model: {v: k for k, v in encodings.items()}
    for model, encodings in QN_ENCODINGS_BY_MODEL.items()
}


def encode_qn_special_chars(query: str, model_family: str):
    return replace_multi(query, QN_ENCODINGS_BY_MODEL[model_family])


def decode_qn_special_chars(query: str, model_family: str):
    return replace_multi(query, QN_DECODINGS_BY_MODEL[model_family])


def preprocess_qn(qn: str, model_family: str):
    # TODO: convert units
    qn = encode_qn_special_chars(qn, model_family=model_family)
    qn = INPUT_TEMPLATE_BY_MODEL[model_family].format(input=qn)
    return qn
