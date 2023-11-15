import argparse
import json
from typing import List

from sklearn.metrics import accuracy_score
from sacrebleu.metrics import BLEU

from core.sparql import SparqlQuery

def normalize_query(query: str):
    try:
        return str(SparqlQuery.fromstring(query))
    except Exception:
        return ""

def get_bleu_metrics(refs: List[List[str]], sys: List[str]):
    bleu = BLEU()
    b = bleu.corpus_score(sys, refs)
    return dict(
        score=b.score,
        precisions=b.precisions,
        bp=b.bp,
        ratio=b.ratio,
        hyp_len=b.sys_len,
        ref_len=b.ref_len,
    )


def get_translation_metrics(data: List[dict]):
    groundtruths = [normalize_query(datum["groundtruth"]) for datum in data]
    predictions = [normalize_query(datum["prediction"]["decoded"]) for datum in data]

    return dict(
        bleu=get_bleu_metrics([groundtruths], predictions),
        accuracy=accuracy_score(groundtruths, predictions),
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_path", type=str)
    parser.add_argument("output_path", type=str)
    args = parser.parse_args()

    with open(args.input_path, "r") as f:
        data = json.load(f)

    print("***Computing translation metrics***")
    translation_metrics = get_translation_metrics(data)
    print("Translation metrics: ", translation_metrics)

    with open(args.output_path, "w") as f:
        json.dump(translation_metrics, f, indent=4)


if __name__ == "__main__":
    main()
