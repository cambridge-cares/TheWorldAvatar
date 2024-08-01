from typing import Dict, List, Optional
from sacrebleu.metrics import BLEU
from sklearn.metrics import precision_recall_fscore_support, accuracy_score
from sklearn.preprocessing import MultiLabelBinarizer


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


def convert_kg_results_to_hashable(results: Optional[List[Dict[str, Dict[str, str]]]]):
    if results is None:
        return tuple()

    # [
    #     Dict[
    #         str,
    #         {
    #             "datatype": NotRequired[str],
    #             "type": str
    #             "value" str
    #         }
    #     ]
    # ]
    _results = [[(k, v["value"]) for k, v in row.items()] for row in results]

    for row in _results:
        row.sort(key=lambda x: x[0])
    _results.sort(key=lambda row: tuple(x[1] for x in row))

    return tuple(tuple(row) for row in _results)


def get_retrieval_performance_metrics(
    gt_list: List[list], predictions_list: List[list]
):
    mlb = MultiLabelBinarizer()
    multilabel_encodings = mlb.fit_transform(gt_list + predictions_list)
    gt_encodings = multilabel_encodings[: len(gt_list)]
    pred_encodings = multilabel_encodings[len(gt_list) :]

    accuracy = accuracy_score(gt_encodings, pred_encodings)

    micro_precision, micro_recall, micro_f1, _ = precision_recall_fscore_support(
        gt_encodings, pred_encodings, average="micro"
    )
    macro_precision, macro_recall, macro_f1, _ = precision_recall_fscore_support(
        gt_encodings, pred_encodings, average="macro"
    )

    return dict(
        accuracy=accuracy,
        micro_precision=micro_precision,
        micro_recall=micro_recall,
        micro_f1=micro_f1,
        macro_precision=macro_precision,
        macro_recall=macro_recall,
        macro_f1=macro_f1,
    )
