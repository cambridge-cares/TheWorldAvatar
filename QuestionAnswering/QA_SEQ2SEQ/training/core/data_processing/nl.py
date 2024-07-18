from core.data_processing.constants import T5_NL_ENCODINGS
from core.data_processing.utils import replace_multi


def preprocess_nl(text: str):
    text = replace_multi(text, T5_NL_ENCODINGS)
    return text
