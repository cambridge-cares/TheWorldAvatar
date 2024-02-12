import argparse
import os
import shutil

from transformers import AutoTokenizer
from optimum.onnxruntime import ORTModelForSeq2SeqLM, ORTQuantizer
from optimum.onnxruntime.configuration import AutoQuantizationConfig


def pt2onnx(input_dir: str, output_dir: str):
    model = ORTModelForSeq2SeqLM.from_pretrained(input_dir, export=True)
    tokenizer = AutoTokenizer.from_pretrained(input_dir)
    model.save_pretrained(output_dir)
    tokenizer.save_pretrained(output_dir)


def quantize(input_dir: str, output_dir: str):
    print("-----Performing avx512_vnni quantization-----")
    encoder_quanitizer = ORTQuantizer.from_pretrained(
        input_dir, file_name="encoder_model.onnx"
    )
    decoder_quantizer = ORTQuantizer.from_pretrained(
        input_dir, file_name="decoder_model.onnx"
    )
    decoder_wp_quantizer = ORTQuantizer.from_pretrained(
        input_dir, file_name="decoder_with_past_model.onnx"
    )

    quantizers = [
        encoder_quanitizer,
        decoder_quantizer,
        decoder_wp_quantizer,
    ]
    dqconfig = AutoQuantizationConfig.avx512_vnni(is_static=False, per_channel=False)

    for q in quantizers:
        q.quantize(
            save_dir=output_dir,
            quantization_config=dqconfig,
        )

    shutil.copyfile(
        os.path.join(input_dir, "generation_config.json"),
        os.path.join(output_dir, "generation_config.json"),
    )


def convert():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=str)
    parser.add_argument("output_dir", type=str)
    parser.add_argument("--mode", choices=["to_onnx", "to_8bit"])
    args = parser.parse_args()

    if args.mode == "to_onnx":
        pt2onnx(args.input_dir, args.output_dir)
    elif args.mode == "to_8bit":
        quantize(args.input_dir, args.output_dir)


if __name__ == "__main__":
    convert()
