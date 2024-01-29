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


def get_translation_metrics_singledomain(df: pd.DataFrame):
    return dict(
        bleu=get_bleu_metrics([[x] for x in df["groundtruth_sparql"]], df["prediction_sparql"].tolist()),
        accuracy=accuracy_score(df["groundtruth_sparql"], df["prediction_sparql"]),
    )

def get_translation_metrics_multidomain(df: pd.DataFrame):
    domain2metrics = df.groupby("groundtruth_domain").apply(get_translation_metrics_singledomain).to_dict()
    agg_metrics = dict(
        micro=dict(
            bleu_score=sum(
                [
                    metric["bleu"]["score"] * sum(df["groundtruth_domain"] == domain)
                    for domain, metric in domain2metrics.items()
                ]
            )
            / len(df),
            accuracy=sum(
                [
                    metric["accuracy"] * sum(df["groundtruth_domain"] == domain)
                    for domain, metric in domain2metrics.items()
                ]
            )
            / len(df),
        ),
        macro=dict(
            bleu_score=sum(
                [metric["bleu"]["score"] for _, metric in domain2metrics.items()]
            )
            / len(domain2metrics),
            accuracy=sum([metric["accuracy"] for _, metric in domain2metrics.items()])
            / len(domain2metrics),
        ),
    )
    return {**domain2metrics, **{"aggregate": agg_metrics}}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_path", type=str)
    parser.add_argument("--multi_domain", action="store_true", default=False)
    args = parser.parse_args()

    df = pd.read_csv(args.input_path)
    df["groundtruth_sparql"] = df["groundtruth_sparql"].apply(normalize_query)
    df["prediction_sparql"] = df["prediction_sparql"].apply(normalize_query)

    print("***Computing evaluation metrics***")
    eval_metrics = dict(
        domain_cls=dict(accuracy=accuracy_score(df["groundtruth_domain"], df["prediction_domain"])) if args.multi_domain else None,
        translation=(get_translation_metrics_multidomain if args.multi_domain else get_translation_metrics_singledomain)(df),
        latency=df["latency"].mean()
    )
    print("Evaluation metrics: ", eval_metrics)

    filename = args.input_path.rsplit(".")[0]
    with open(filename + "_eval.json", "w") as f:
        json.dump(eval_metrics, f, indent=4)

    df["domain_match"] = df["prediction_domain"] == df["groundtruth_domain"]
    df["sparql_match"] = df["prediction_sparql"] == df["groundtruth_sparql"]
    df.to_csv(filename + "_eval.csv", index=False)


if __name__ == "__main__":
    main()
