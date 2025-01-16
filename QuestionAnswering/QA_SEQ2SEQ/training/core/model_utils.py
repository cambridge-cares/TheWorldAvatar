import os

import torch
from transformers import AutoModelForSeq2SeqLM, AutoTokenizer
from optimum.onnxruntime import ORTModelForSeq2SeqLM

from core.args_schema import ModelArguments


def get_hf_model_and_tokenizer(model_args: ModelArguments):
    tokenizer = AutoTokenizer.from_pretrained(model_args.model_path)

    if model_args.device_map:
        device_map = model_args.device_map
    # if we are in a distributed setting, we need to set the device map per device
    elif os.environ.get("LOCAL_RANK") is not None:
        local_rank = int(os.environ.get("LOCAL_RANK", "0"))
        device_map = {"": local_rank}
    else:
        device_map = "auto"

    model = AutoModelForSeq2SeqLM.from_pretrained(
        model_args.model_path, device_map=device_map
    )

    return model, tokenizer


def get_ort_model_and_tokenizer(model_args: ModelArguments):
    tokenizer = AutoTokenizer.from_pretrained(model_args.model_path)
    if model_args.device_map:
        device_map = model_args.device_map
    else:
        device_map = "auto"

    if device_map == "cpu" or (device_map == "auto" and not torch.cuda.is_available()):
        model = ORTModelForSeq2SeqLM.from_pretrained(model_args.model_path)
    elif device_map == "cuda" or (device_map == "auto" and torch.cuda.is_available()):
        assert torch.cuda.is_available(), "CUDA is not available to load model into"
        model = ORTModelForSeq2SeqLM.from_pretrained(
            model_args.model_path, provider="CUDAExecutionProvider"
        )
        assert model.providers == ["CUDAExecutionProvider", "CPUExecutionProvider"]
        print("-----ONNX Runtime model is loaded to CUDA-----")
    else:
        raise ValueError("Unexpected device_map: " + device_map)

    return model, tokenizer
