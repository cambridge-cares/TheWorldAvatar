import argparse

from transformers import AutoTokenizer
from optimum.onnxruntime import ORTModelForSeq2SeqLM


def convert():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=str)
    parser.add_argument("output_dir", type=str)
    args = parser.parse_args()

    model = ORTModelForSeq2SeqLM.from_pretrained(args.input_dir, export=True)
    tokenizer = AutoTokenizer.from_pretrained(args.input_dir)

    model.save_pretrained(args.output_dir)
    tokenizer.save_pretrained(args.output_dir)

if __name__ == "__main__":
    convert()
