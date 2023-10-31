import os

from transformers import AutoModelForSeq2SeqLM, AutoTokenizer


def get_hf_model_and_tokenizer(model_path: str):
    tokenizer = AutoTokenizer.from_pretrained(model_path)

    # if we are in a distributed setting, we need to set the device map per device
    if os.environ.get("LOCAL_RANK") is not None:
        local_rank = int(os.environ.get("LOCAL_RANK", "0"))
        device_map = {"": local_rank}
    else:
        device_map = "auto"

    model = AutoModelForSeq2SeqLM.from_pretrained(model_path, device_map=device_map)

    return model, tokenizer
