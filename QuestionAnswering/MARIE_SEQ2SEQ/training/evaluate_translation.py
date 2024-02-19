import argparse
import json
from typing import List, Optional

from sklearn.metrics import accuracy_score
from core.data_processing.compact_query.compact_query_rep import CompactQueryRep

from core.data_processing.compact_query.correct_relations import RelationCorrector
from core.data_processing.compact_query.correct_spans import SpanCorrector
from core.data_processing.exceptions import InvalidCompactQueryError
from core.data_processing.output_processing import normalize_query, remove_prefixes
from core.eval import get_bleu_metrics


def get_translation_metrics(
    data: List[dict], do_correct_spans: bool, do_correct_relations: bool
):
    span_corrector = SpanCorrector()
    relation_corrector = RelationCorrector()

    def _get_corrected_query(
        text: str,
        nlq: Optional[str] = None,
        do_correct_spans: bool = True,
        do_correct_relations: bool = True,
    ):
        if not do_correct_spans and not do_correct_relations:
            return text
        query = CompactQueryRep.from_string(text)
        if do_correct_spans:
            assert nlq is not None
            query = span_corrector.correct(query, nlq=nlq)
        if do_correct_relations:
            query = relation_corrector.correct(query)
        return query.to_string()

    def _get_pred(datum: dict):
        if datum["prediction_postprocessed"] is None:
            return ""

        if do_correct_spans:
            if do_correct_relations:
                pred_key = "prediction_corrected"
            else:
                pred_key = "prediction_spancorrected"
        elif do_correct_relations:
            pred_key = "prediction_relationcorrected"
        else:
            pred_key = "prediction_postprocessed"

        try:
            pred = datum.get(
                pred_key,
                _get_corrected_query(
                    datum["prediction_postprocessed"],
                    nlq=datum["question"],
                    do_correct_spans=do_correct_spans,
                    do_correct_relations=do_correct_relations,
                ),
            )
        except Exception as e:
            if not isinstance(e, InvalidCompactQueryError):
                print("An unhandled error is encountered when parsing a compact query.")
                print(datum)
                print(e)
            return ""
        if pred is None:
            return ""
        return normalize_query(pred)

    queries = [normalize_query(remove_prefixes(datum["gt_compact"])) for datum in data]
    predictions = [_get_pred(datum) for datum in data]

    return dict(
        bleu=get_bleu_metrics([queries], predictions),
        accuracy=accuracy_score(queries, predictions),
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_path", type=str)
    parser.add_argument("output_path", type=str)
    parser.add_argument("--do_correct_spans", action="store_true")
    parser.add_argument("--do_correct_relations", action="store_true")
    args = parser.parse_args()

    with open(args.input_path, "r") as f:
        data = json.load(f)

    print("***Computing translation metrics***")
    translation_metrics = get_translation_metrics(
        data,
        do_correct_spans=args.do_correct_spans,
        do_correct_relations=args.do_correct_relations,
    )
    print("Translation metrics: ", translation_metrics)

    with open(args.output_path, "w") as f:
        json.dump(translation_metrics, f, indent=4)


if __name__ == "__main__":
    main()
