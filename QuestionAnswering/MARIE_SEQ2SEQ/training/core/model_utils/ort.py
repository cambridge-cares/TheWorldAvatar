import os

import torch
from transformers import PreTrainedTokenizer
from transformers.generation.configuration_utils import GenerationConfig

from core.arguments_schema import ModelArguments
from core.model_utils.hf import get_hf_tokenizer
from core.data_processing.input_processing import preprocess_input


SHORT_INPUT_EXAMPLE = "What labels can be used to recognize 3-(2,5-diketoimidazolidin-4-yl)propionic acid?"
LONG_INPUT_EXAMPLE = "What are InChI=1S/C9H15N3O11P2/c10-5-1-2-12(9(15)11-5)8-7(14)6(13)4(22-8)3-21-25(19,20)23-24(16,17)18/h1-2,4,6-8,13-14H,3H2,(H,19,20)(H2,10,11,15)(H2,16,17,18)'s tautomer count, melting point, and tautomer count?"


def load_ort_model_seq2seq_quantized_cpu(model_args: ModelArguments):
    from optimum.onnxruntime import ORTModelForSeq2SeqLM, ORTQuantizer
    from optimum.onnxruntime.configuration import AutoQuantizationConfig
    from optimum.utils.save_utils import maybe_load_preprocessors

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


def load_ort_model_with_tensorrt(
    model_args: ModelArguments, tokenizer: PreTrainedTokenizer
):
    from optimum.onnxruntime import ORTModelForSeq2SeqLM

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
            encoded_input = tokenizer(
                preprocess_input(example, model_family=model_args.model_family),
                return_tensors="pt",
            ).to("cuda")
            model(**encoded_input)

    print("-----Performing engine warm-up-----")
    encoded_inputs = tokenizer(
        preprocess_input(SHORT_INPUT_EXAMPLE, model_family=model_args.model_family),
        return_tensors="pt",
    ).to("cuda")

    for _ in range(3):
        model.generate(**encoded_inputs, max_new_tokens=256)

    return model


def get_ort_model_and_tokenizer(model_args: ModelArguments):
    from optimum.onnxruntime import ORTModelForSeq2SeqLM

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
