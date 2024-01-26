import argparse
import json
from typing import List
import pandas as pd

from sklearn.metrics import accuracy_score
from sacrebleu.metrics import BLEU

from core.sparql.normalize import normalize_query


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
    groundtruths = [datum["groundtruth"]["sparql"] for datum in data]
    predictions = [datum["prediction"]["sparql"] for datum in data]

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
    args = parser.parse_args()

    with open(args.input_path, "r") as f:
        data = json.load(f)

    for datum in data:
        datum["groundtruth"]["sparql"] = normalize_query(datum["groundtruth"]["sparql"])
        datum["prediction"]["sparql"] = normalize_query(datum["prediction"]["sparql"]["decoded"])

    print("***Computing evaluation metrics***")
    if "domain" in data[0]["prediction"]:
        eval_metrics = dict(
            domain_cls=get_domaincls_metrics(data),
            translation=get_translation_metrics_multidomain(data),
            latency=sum([datum["latency"] for datum in data]) / len(data)
        )
    else:
        eval_metrics = get_translation_metrics_singledomain(data)
    print("Evaluation metrics: ", eval_metrics)

    filename = args.input_path.rsplit(".")[0]
    with open(filename + "_eval.json", "w") as f:
        json.dump(eval_metrics, f, indent=4)

    df_data = [
        dict(
            id=datum["id"],
            question=datum["question"],
            gt_domain=datum["groundtruth"]["domain"],
            gt_sparql=datum["groundtruth"]["sparql"],
            pred_domain=datum["prediction"]["domain"],
            pred_sparql=datum["prediction"]["sparql"],
            domain_match=1
            if datum["prediction"]["domain"] == datum["groundtruth"]["domain"]
            else 0,
            sparql_match=1
            if datum["prediction"]["sparql"] == datum["groundtruth"]["sparql"]
            else 0,
        )
        for datum in data
    ]
    df = pd.DataFrame(df_data)
    df.to_csv(filename + "_eval.csv", index=False)


if __name__ == "__main__":
    main()
