from typing import Dict, List, Optional
from sacrebleu.metrics import BLEU
from sklearn.metrics import precision_recall_fscore_support, accuracy_score
from sklearn.preprocessing import MultiLabelBinarizer
from core.data_processing.compact_query_rep import CompactQueryRep
from core.data_processing.exceptions import InvalidCompactQueryError

from core.data_processing.output_processing import normalize_query, remove_prefixes


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


def get_translation_metrics(
    data: List[dict], do_correct_spans: bool, do_correct_relations: bool
):
    def _get_pred(datum: dict):
        if datum["prediction_postprocessed"] is None:
            return ""
        try:
            if do_correct_spans:
                if do_correct_relations:
                    if datum.get("prediction_corrected") is not None:
                        pred = datum["prediction_corrected"]
                    else:
                        pred = (
                            CompactQueryRep.from_string(
                                datum["prediction_postprocessed"]
                            )
                            .correct_spans(nlq=datum["question"])
                            .correct_relations()
                            .to_string()
                        )
                else:
                    if datum.get("prediction_spancorrected") is not None:
                        pred = datum["prediction_spancorrected"]
                    else:
                        pred = (
                            CompactQueryRep.from_string(
                                datum["prediction_postprocessed"]
                            )
                            .correct_spans(nlq=datum["question"])
                            .to_string()
                        )
            elif do_correct_relations:
                if datum.get("prediction_relationcorrected") is not None:
                    pred = datum["prediction_relationcorrected"]
                else:
                    pred = (
                        CompactQueryRep.from_string(
                            datum["prediction_postprocessed"]
                        )
                        .correct_relations()
                        .to_string()
                    )
            else:
                pred = datum["prediction_postprocessed"]
        except Exception as e:
            if not isinstance(e, InvalidCompactQueryError):
                print(
                    "An unhandled error is encountered when parsing a compact query."
                )
                print(datum)
                print(e)
            return ""
        return normalize_query(pred)

    queries = [normalize_query(remove_prefixes(datum["gt_compact"])) for datum in data]
    predictions = [_get_pred(datum) for datum in data]

    return dict(
        bleu=get_bleu_metrics([queries], predictions),
        accuracy=accuracy_score(queries, predictions),
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
