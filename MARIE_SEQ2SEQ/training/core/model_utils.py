import os
import torch

import ctranslate2
from transformers import (
    BitsAndBytesConfig,
    AutoModelForSeq2SeqLM,
    LlamaForCausalLM,
    AutoTokenizer,
)
from peft import (
    PeftModel,
    LoraConfig,
    TaskType,
    get_peft_model,
    prepare_model_for_kbit_training,
)
from optimum.onnxruntime import ORTModelForSeq2SeqLM, ORTQuantizer
from optimum.onnxruntime.configuration import AutoQuantizationConfig
from optimum.intel import OVModelForSeq2SeqLM, OVModelForCausalLM
from optimum.utils.save_utils import maybe_load_preprocessors
from transformers.generation.configuration_utils import GenerationConfig
from transformers import PreTrainedTokenizer
import pyonmttok

from core.arguments_schema import ModelArguments
from core.data_processing.input_processing import preprocess_input


TARGET_MODULES_BY_MODEL = dict(t5=["q", "v"], llama=["q_proj", "v_proj"])

SHORT_INPUT_EXAMPLE = "What labels can be used to recognize 3-(2,5-diketoimidazolidin-4-yl)propionic acid?"
LONG_INPUT_EXAMPLE = "What are InChI=1S/C9H15N3O11P2/c10-5-1-2-12(9(15)11-5)8-7(14)6(13)4(22-8)3-21-25(19,20)23-24(16,17)18/h1-2,4,6-8,13-14H,3H2,(H,19,20)(H2,10,11,15)(H2,16,17,18)'s tautomer count, melting point, and tautomer count?"


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


def get_ov_model_and_tokenizer(model_args: ModelArguments, max_input_tokens: int = 256):
    model_cls = (
        OVModelForSeq2SeqLM if model_args.model_family == "t5" else OVModelForCausalLM
    )
    model = model_cls.from_pretrained(model_args.model_path)

    model.reshape(1, max_input_tokens)
    model.compile()

    tokenizer = get_hf_tokenizer(model_args)
    return model, tokenizer


def load_ort_model_seq2seq_quantized_cpu(model_args: ModelArguments):
    quantized_models_dir = os.path.join(model_args.model_path, "quantized_models")
    quantized_encoder_path = os.path.join(
        quantized_models_dir, "encoder_model_quantized.onnx"
    )
    quantized_decoder_path = os.path.join(
        quantized_models_dir, "decoder_model_quantized.onnx"
    )
    quantized_decoder_wp_path = os.path.join(
        quantized_models_dir, "decoder_with_past_model_quantized.onnx"
    )

    if all(
        os.path.exists(x)
        for x in [
            quantized_models_dir,
            quantized_encoder_path,
            quantized_decoder_path,
            quantized_decoder_wp_path,
        ]
    ):
        print("-----A cache of quantized model is found-----")
    else:
        print("-----No cache of quantized model is found-----")
        print("-----Performing avx512_vnni quantization-----")

        encoder_quanitizer = ORTQuantizer.from_pretrained(
            model_args.model_path, file_name="encoder_model.onnx"
        )
        decoder_quantizer = ORTQuantizer.from_pretrained(
            model_args.model_path, file_name="decoder_model.onnx"
        )
        decoder_wp_quantizer = ORTQuantizer.from_pretrained(
            model_args.model_path, file_name="decoder_with_past_model.onnx"
        )

        quantizers = [
            encoder_quanitizer,
            decoder_quantizer,
            decoder_wp_quantizer,
        ]
        dqconfig = AutoQuantizationConfig.avx512_vnni(
            is_static=False, per_channel=False
        )

        for q in quantizers:
            q.quantize(
                save_dir=quantized_models_dir,
                quantization_config=dqconfig,
            )

        print("-----Quantization done-----")

    (
        encoder_session,
        decoder_session,
        decoder_wp_session,
    ) = ORTModelForSeq2SeqLM.load_model(
        encoder_path=quantized_encoder_path,
        decoder_path=quantized_decoder_path,
        decoder_with_past_path=quantized_decoder_wp_path,
    )
    config = ORTModelForSeq2SeqLM._load_config(quantized_models_dir)
    generation_config = GenerationConfig.from_pretrained(model_args.model_path)
    preprocessors = maybe_load_preprocessors(model_args.model_path)

    return ORTModelForSeq2SeqLM(
        encoder_session=encoder_session,
        decoder_session=decoder_session,
        config=config,
        onnx_paths=[
            quantized_encoder_path,
            quantized_decoder_path,
            quantized_decoder_wp_path,
        ],
        decoder_with_past_session=decoder_wp_session,
        model_save_dir=model_args.model_path,
        preprocessors=preprocessors,
        generation_config=generation_config,
    )


def load_ort_model_with_tensorrt(model_args: ModelArguments, tokenizer: PreTrainedTokenizer):
    trt_engine_cache_path = os.path.join(model_args.model_path, "trt_cache")
    os.makedirs(trt_engine_cache_path, exist_ok=True)

    model = ORTModelForSeq2SeqLM.from_pretrained(
        model_args.model_path,
        provider="TensorrtExecutionProvider",
        provider_options=dict(
            trt_engine_cache_enable=True,
            trt_engine_cache_path=trt_engine_cache_path,
        ),
    )

    assert model.providers == [
        "TensorrtExecutionProvider",
        "CUDAExecutionProvider",
        "CPUExecutionProvider",
    ]

    if any(f.endswith(".engine") for f in os.listdir(trt_engine_cache_path)):
        print("-----TensorRT engine cache found-----")
    else:
        print("-----TensorRT engine cache not found-----")
        print("-----Building TensorRT engine-----")
        for example in [SHORT_INPUT_EXAMPLE, LONG_INPUT_EXAMPLE]:
            input_ids = tokenizer(
                preprocess_input(example, model_family=model_args.model_family),
                return_tensors="pt",
            ).input_ids.to("cuda")
            model(input_ids)

    print("-----Performing engine warm-up-----")
    input_ids = tokenizer(
        preprocess_input(SHORT_INPUT_EXAMPLE, model_family=model_args.model_family),
        return_tensors="pt",
    ).input_ids.to("cuda")

    for _ in range(3):
        model.generate(input_ids=input_ids, max_new_tokens=256)
    
    return model


def get_ort_model_and_tokenizer(model_args: ModelArguments):
    if model_args.model_family != "t5":
        raise ValueError("Function not implemented for :" + model_args.model_family)

    tokenizer = get_hf_tokenizer(model_args)

    if model_args.device_map == "cpu" or (
        model_args.device_map == "auto" and not torch.cuda.is_available()
    ):
        if model_args.bits is None or model_args.bits == 32:
            model = ORTModelForSeq2SeqLM.from_pretrained(model_args.model_path)
        elif model_args.bits == 8:
            print("-----Loading ONNX model in 8 bits-----")
            model = load_ort_model_seq2seq_quantized_cpu(model_args)
        else:
            raise ValueError(
                str(model_args.bits)
                + "-bit quantization is not supported for ONNX Runtime on CPU."
            )
        print("-----ONNX Runtime model is loaded to CPU-----")
    elif not model_args.use_tensorrt:
        model = ORTModelForSeq2SeqLM.from_pretrained(
            model_args.model_path, provider="CUDAExecutionProvider"
        )
        assert model.providers == ["CUDAExecutionProvider", "CPUExecutionProvider"]

        print("-----ONNX Runtime model is loaded to CUDA-----")
    else:
        model = load_ort_model_with_tensorrt(model_args, tokenizer=tokenizer)

    return model, tokenizer
