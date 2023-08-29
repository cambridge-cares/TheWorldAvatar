import os
import torch

import ctranslate2
from transformers import (
    BitsAndBytesConfig,
    AutoModelForSeq2SeqLM,
    LlamaForCausalLM,
    AutoTokenizer,
    AutoConfig,
)
from peft import (
    PeftModel,
    LoraConfig,
    TaskType,
    get_peft_model,
    prepare_model_for_kbit_training,
)
from optimum.onnxruntime import ORTModelForSeq2SeqLM, ORTModelForCausalLM
import pyonmttok

from core.arguments_schema import ModelArguments


TARGET_MODULES_BY_MODEL = dict(t5=["q", "v"], llama=["q_proj", "v_proj"])


def get_hf_model(model_args: ModelArguments, is_trainable: bool):
    if model_args.device_map:
        device_map = model_args.device_map
    # if we are in a distributed setting, we need to set the device map per device
    elif os.environ.get("LOCAL_RANK") is not None:
        local_rank = int(os.environ.get("LOCAL_RANK", "0"))
        device_map = {"": local_rank}
    else:
        device_map = "auto"

    if model_args.bits is not None:
        bnb_config = BitsAndBytesConfig(
            load_in_8bit=model_args.bits == 8,
            load_in_4bit=model_args.bits == 4,
            bnb_4bit_use_double_quant=True,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_compute_dtype=torch.bfloat16,
        )
    else:
        bnb_config = None

    model_load_kwargs = {
        k: v
        for k, v in dict(
            quantization_config=bnb_config,
            device_map=device_map,
            use_auth_token=os.environ.get("HF_ACCESS_TOKEN"),
        ).items()
        if v is not None
    }

    model_cls = (
        AutoModelForSeq2SeqLM if model_args.model_family == "t5" else LlamaForCausalLM
    )
    model = model_cls.from_pretrained(model_args.model_path, **model_load_kwargs)

    if getattr(model, "is_loaded_in_8bit", False) or getattr(
        model, "is_loaded_in_4bit", False
    ):
        model = prepare_model_for_kbit_training(model)

    if model_args.lora_path is not None:
        model = PeftModel.from_pretrained(
            model, model_args.lora_path, is_trainable=is_trainable
        )
    elif all(
        x is not None
        for x in (model_args.lora_r, model_args.lora_alpha, model_args.lora_dropout)
    ):
        task_type = (
            TaskType.SEQ_2_SEQ_LM
            if model_args.model_family == "t5"
            else TaskType.CAUSAL_LM
        )
        lora_config = LoraConfig(
            r=model_args.lora_r,
            lora_alpha=model_args.lora_alpha,
            lora_dropout=model_args.lora_dropout,
            bias="none",
            target_modules=TARGET_MODULES_BY_MODEL[model_args.model_family],
            task_type=task_type,
        )

        model = get_peft_model(model, lora_config)
        model.print_trainable_parameters()

    return model


def get_hf_tokenizer(model_args: ModelArguments):
    tokenizer = AutoTokenizer.from_pretrained(
        model_args.model_path,
        use_auth_token=os.environ.get("HF_ACCESS_TOKEN"),
    )

    if model_args.model_family == "llama":
        tokenizer.pad_token_id = tokenizer.unk_token_id

    return tokenizer


def get_hf_model_and_tokenizer(model_args: ModelArguments, is_trainable: bool):
    tokenizer = get_hf_tokenizer(model_args)
    model = get_hf_model(model_args, is_trainable=is_trainable)
    return model, tokenizer


def get_onmt_tokenizer(model_args: ModelArguments):
    if model_args.model_family != "t5":
        raise ValueError(
            f"Only T5 tokenizer has been tested and supported, while the model family is {model_args.model_family}."
        )
    return pyonmttok.SentencePieceTokenizer(
        os.path.join(model_args.model_path, "spiece.model")
    )


def get_onmt_model_and_tokenizer(model_args: ModelArguments):
    tokenizer = get_onmt_tokenizer(model_args)
    model = ctranslate2.Translator(
        model_args.model_path, device=model_args.device_map or "auto"
    )
    return model, tokenizer


def get_ort_model(model_args: ModelArguments):
    model_cls = (
        ORTModelForSeq2SeqLM if model_args.model_family == "t5" else ORTModelForCausalLM
    )
    model_load_kwargs = (
        dict()
        if model_args.device_map == "cpu"
        else dict(provider="CUDAExecutionProvider")
    )
    model = model_cls.from_pretrained(model_args.model_path, **model_load_kwargs)
    return model


def get_ort_model_and_tokenizer(model_args: ModelArguments):
    tokenizer = get_hf_tokenizer(model_args)
    model = get_ort_model(model_args)
    return model, tokenizer
