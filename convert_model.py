import argparse

from optimum.onnxruntime import ORTModelForSeq2SeqLM


def convert():
    parser = argparse.ArgumentParser()
    parser.add_argument("model_dir", type=str)
    args = parser.parse_args()

    model = ORTModelForSeq2SeqLM.from_pretrained(args.model_dir, export=True)
    model.save_pretrained(args.model_dir)

if __name__ == "__main__":
    convert()
