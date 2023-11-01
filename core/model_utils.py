import os

import torch
from transformers import AutoModelForSeq2SeqLM, AutoTokenizer
from transformers.generation.configuration_utils import GenerationConfig
from optimum.onnxruntime import ORTModelForSeq2SeqLM
from optimum.onnxruntime import ORTModelForSeq2SeqLM, ORTQuantizer
from optimum.onnxruntime.configuration import AutoQuantizationConfig
from optimum.utils.save_utils import maybe_load_preprocessors

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


def get_ort_model_and_tokenizer(model_args: ModelArguments):
    tokenizer = AutoTokenizer.from_pretrained(model_args.model_path)
    if model_args.device_map:
        device_map = model_args.device_map
    else:
        device_map = "auto"

    if device_map == "cpu" or (device_map == "auto" and not torch.cuda.is_available()):
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
    elif device_map == "cuda":
        assert torch.cuda.is_available(), "CUDA is not available to load model into"
        model = ORTModelForSeq2SeqLM.from_pretrained(
            model_args.model_path, provider="CUDAExecutionProvider"
        )
        assert model.providers == ["CUDAExecutionProvider", "CPUExecutionProvider"]
        print("-----ONNX Runtime model is loaded to CUDA-----")
    else:
        raise ValueError("Unexpected device_map: " + device_map)

    return model, tokenizer
