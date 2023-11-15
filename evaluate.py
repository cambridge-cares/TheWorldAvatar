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


def get_translation_metrics_singledomain(data: List[dict]):
    groundtruths = [normalize_query(datum["groundtruth"]["sparql"]) for datum in data]
    predictions = [
        normalize_query(datum["prediction"]["sparql"]["decoded"]) for datum in data
    ]

    return dict(
        bleu=get_bleu_metrics([groundtruths], predictions),
        accuracy=accuracy_score(groundtruths, predictions),
    )


def get_domaincls_metrics(data: List[dict]):
    groundtruths = [datum["groundtruth"]["domain"] for datum in data]
    predictions = [datum["prediction"]["domain"] for datum in data]

    return dict(accuracy=accuracy_score(groundtruths, predictions))


def get_translation_metrics_multidomain(data: List[dict]):
    domains = set([datum["groundtruth"]["domain"] for datum in data])
    domain2data = {
        domain: [datum for datum in data if datum["groundtruth"]["domain"] == domain]
        for domain in domains
    }
    domain2metrics = {
        domain: get_translation_metrics_singledomain(domain2data[domain])
        for domain in domains
    }
    agg_metrics = dict(
        micro=dict(
            bleu_score=sum(
                [
                    metric["bleu"]["score"] * len(domain2data[domain])
                    for domain, metric in domain2metrics.items()
                ]
            )
            / len(data),
            accuracy=sum(
                [
                    metric["accuracy"] * len(domain2data[domain])
                    for domain, metric in domain2metrics.items()
                ]
            )
            / len(data),
        ),
        macro=dict(
            bleu_score=sum(
                [metric["bleu"]["score"] for _, metric in domain2metrics.items()]
            )
            / len(domains),
            accuracy=sum([metric["accuracy"] for _, metric in domain2metrics.items()])
            / len(domains),
        ),
    )
    return {**domain2metrics, **{"aggregate": agg_metrics}}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_path", type=str)
    parser.add_argument("output_path", type=str)
    args = parser.parse_args()

    with open(args.input_path, "r") as f:
        data = json.load(f)

    print("***Computing evaluation metrics***")
    if "domain" in data[0]["prediction"]:
        eval_metrics = dict(
            domain_cls=get_domaincls_metrics(data),
            translation=get_translation_metrics_multidomain(data),
        )
    else:
        eval_metrics = get_translation_metrics_singledomain(data)
    print("Evaluation metrics: ", eval_metrics)

    with open(args.output_path, "w") as f:
        json.dump(eval_metrics, f, indent=4)


if __name__ == "__main__":
    main()
