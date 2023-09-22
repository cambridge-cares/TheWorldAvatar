from core.data_processing.utils import replace_multi

LLAMA_TEMPLATE = "translate to SPARQL: {question}\n\n###\n\n{sparql_query}" 
LLAMA_PROMPT_TEMPLATE = "translate to SPARQL: {question}\n\n###\n\n"
LLAMA_COMPLETION_TEMPLATE = "### \n\n"

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
    # TODO: convert units
    if model_family == "t5":
        text = t5_encode_input_special_chars(text)
        text = T5_INPUT_PREFIX + text
    elif model_family == "llama":
        text = LLAMA_PROMPT_TEMPLATE.format(question=text)
    return text
