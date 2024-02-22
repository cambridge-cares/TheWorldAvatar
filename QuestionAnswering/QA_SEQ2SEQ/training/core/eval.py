from typing import List
import pandas as pd

from sklearn.metrics import accuracy_score
from sacrebleu.metrics import BLEU

from core.sparql.normalize import normalize_query

bleu = BLEU()


def get_bleu_score(refs: List[List[str]], sys: List[str]):
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
        bleu=get_bleu_score(
            [[x] for x in df["groundtruth_sparql"]], df["prediction_sparql"].tolist()
        ),
        accuracy=accuracy_score(df["groundtruth_sparql"], df["prediction_sparql"]),
    )


def get_translation_metrics_multidomain(df: pd.DataFrame):
    domain2metrics = (
        df.groupby("groundtruth_domain")
        .apply(get_translation_metrics_singledomain)
        .to_dict()
    )
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


def compute_eval_metrics(df: pd.DataFrame, multi_domain: bool):
    """Computes evaluation metrics.
    Compusolory headers: `groundtruth_domain`, `groundtruth_sparql`, `prediction_domain`, `prediction_sparql`.
    """
    df["groundtruth_sparql"] = df["groundtruth_sparql"].apply(normalize_query)
    df["prediction_sparql"] = df["prediction_sparql"].apply(normalize_query)

    if multi_domain:
        domain_cls = dict(
            accuracy=accuracy_score(df["groundtruth_domain"], df["prediction_domain"])
        )
        translation = get_translation_metrics_multidomain(df)
    else:
        domain_cls = None
        translation = get_translation_metrics_singledomain(df)

    return dict(
        domain_cls=domain_cls,
        translation=translation,
        latency=df["latency"].mean(),
    )
