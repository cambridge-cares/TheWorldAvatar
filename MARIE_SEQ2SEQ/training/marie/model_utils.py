import os
import torch

from transformers import BitsAndBytesConfig, AutoModelForSeq2SeqLM, AutoTokenizer
from peft import LoraConfig, TaskType, get_peft_model

from marie.arguments_schema import ModelArguments


def get_model(model_args: ModelArguments):
    if model_args.bits is not None:
        bnb_config = BitsAndBytesConfig(
            load_in_8bit=model_args.bits == 8,
            load_in_4bit=model_args.bits == 4,
            bnb_4bit_use_double_quant=True,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_compute_dtype=torch.bfloat16,
        )
        device_map = {"": 0} # quantization only works in GPU
    else:
        bnb_config = None
        device_map = "auto"

    model = AutoModelForSeq2SeqLM.from_pretrained(
        model_args.model_path,
        quantization_config=bnb_config,
        device_map=device_map,
        use_auth_token=os.getenv("HF_ACCESS_TOKEN"),
    )

    if all(x is not None for x in (model_args.lora_r, model_args.lora_alpha, model_args.lora_dropout)): 
        lora_config = LoraConfig(
            r=model_args.lora_r,
            lora_alpha=model_args.lora_alpha,
            lora_dropout=model_args.lora_dropout,
            bias="none",
            target_modules=["q", "v"],
            task_type=TaskType.SEQ_2_SEQ_LM
        )

        model = get_peft_model(model, lora_config)
        model.print_trainable_parameters()

    return model


def get_model_and_tokenizer(model_args: ModelArguments):
    tokenizer = AutoTokenizer.from_pretrained(model_args.model_path)
    model = get_model(model_args)

    return model, tokenizer