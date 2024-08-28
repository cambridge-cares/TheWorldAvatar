from core.constants import FAMILY_CAUSAL
from core.data_processing.utils import replace_multi


CAUSALLM_INPUT_TEMPLATE = 'Translate the following input query to SPARQL.\n\n### Input:\n{question}\n\n### Response:\n'

T5_INPUT_PREFIX = "translate to SPARQL: "
T5_INPUT_ENCODINGS = {
    "<": "&lt;",
    ">": "&gt;",
}
T5_INPUT_DECODINGS = {v: k for k, v in T5_INPUT_ENCODINGS.items()}


def t5_encode_input_special_chars(text: str):
    return replace_multi(text, T5_INPUT_ENCODINGS)


def t5_decode_input_special_chars(text: str):
    return replace_multi(text, T5_INPUT_DECODINGS)


def preprocess_input(text: str, model_family: str):
    if model_family == "t5":
        text = t5_encode_input_special_chars(text)
        text = T5_INPUT_PREFIX + text
    elif model_family == "mt0":
        text = T5_INPUT_PREFIX + text
    elif model_family in FAMILY_CAUSAL:
        text = CAUSALLM_INPUT_TEMPLATE.format(question=text)
    return text
