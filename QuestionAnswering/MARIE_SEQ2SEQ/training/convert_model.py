import argparse
import os
import shutil

from optimum.intel import OVModelForSeq2SeqLM
from optimum.onnxruntime import ORTModelForSeq2SeqLM
from transformers import AutoTokenizer


def convert():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=str)
    parser.add_argument("output_dir", type=str)
    parser.add_argument("--target_format", choices=["onmt", "ov", "ort"], type=str, required=True)
    args = parser.parse_args()

    if args.target_format == "onmt":  # OpenNMT
        os.system(
            f"ct2-transformers-converter --force --model {args.input_dir} --output_dir {args.output_dir}"
        )
        shutil.copyfile(
            os.path.join(args.input_dir, "spiece.model"),
            os.path.join(args.output_dir, "spiece.model"),
        )
    elif args.target_format == "ov":  # OpenVINO
        model = OVModelForSeq2SeqLM.from_pretrained(args.input_dir, export=True)
        tokenizer = AutoTokenizer.from_pretrained(args.input_dir)
        model.save_pretrained(args.output_dir)
        tokenizer.save_pretrained(args.output_dir)
    elif args.target_format == "ort":  # ONNX Runtime
        model = ORTModelForSeq2SeqLM.from_pretrained(args.input_dir, export=True)
        tokenizer = AutoTokenizer.from_pretrained(args.input_dir)
        model.save_pretrained(args.output_dir)
        tokenizer.save_pretrained(args.output_dir)
    else:
        raise ValueError("Unexpected target model format: " + args.target_format)

if __name__ == "__main__":
    convert()
