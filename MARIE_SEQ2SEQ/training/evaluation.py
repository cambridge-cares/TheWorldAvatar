import argparse
import json
from typing import Dict, List, Optional, TypedDict

from SPARQLWrapper import SPARQLExceptions
from tqdm import tqdm
from marie.eval import (
    convert_kg_results_to_hashable,
    get_retrieval_performance_metrics,
    get_translation_metrics,
)

from marie.kg_client import KgClient


SPARQL_ENDPOINT = "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql"


class AnswerDatum(TypedDict):
    answer: List[dict]
    is_query_malformed: bool


def get_answer(kg_client: KgClient, query: Optional[str]):
    answer = None
    is_query_malformed = False

    if query is None:
        is_query_malformed = True
    else:
        try:
            answer = kg_client.query(query)["results"]["bindings"]
        except SPARQLExceptions.QueryBadFormed:
            is_query_malformed = True

    return AnswerDatum(answer=answer, is_query_malformed=is_query_malformed)


def get_answer_data(data: dict):
    kg_client = KgClient(SPARQL_ENDPOINT)

    print("***Obtaining ground-truth answers***")
    gt_answers = [get_answer(kg_client, datum["gt"]) for datum in tqdm(data)]
    print("***Finished obtaining ground-truth answers***")

    print("***Obtaining predicted answers***")
    predicted_answers = [
        get_answer(kg_client, datum["prediction_postprocessed"]) for datum in tqdm(data)
    ]
    print("***Finished obtaining predicted answers***")

    return gt_answers, predicted_answers


def get_answer_metrics(
    gt_answers_data: List[AnswerDatum], predicted_answers_data: List[AnswerDatum]
):
    assert len(gt_answers_data) == len(predicted_answers_data)

    gt_malform_rate = sum(
        datum["is_query_malformed"] for datum in gt_answers_data
    ) / len(gt_answers_data)
    prediction_malform_rate = sum(
        datum["is_query_malformed"] for datum in predicted_answers_data
    ) / len(predicted_answers_data)

    retrieval_performance_metrics = get_retrieval_performance_metrics(
        [convert_kg_results_to_hashable(datum["answer"]) for datum in gt_answers_data],
        [
            convert_kg_results_to_hashable(datum["answer"])
            for datum in predicted_answers_data
        ],
    )

    return {
        "gt_malform_rate": gt_malform_rate,
        "prediction_malform_rate": prediction_malform_rate,
        **retrieval_performance_metrics,
    }


def eval():
    parser = argparse.ArgumentParser()
    parser.add_argument("--data_path", type=str, required=True)
    parser.add_argument("--output_file", type=str, required=True)
    args = parser.parse_args()

    with open(args.data_path, "r") as f:
        data = json.load(f)

    print("***Computing translation metrics***")
    translation_metrics = get_translation_metrics(data)
    print("Translation metrics: ", translation_metrics)

    gt_answers, predicted_answers = get_answer_data(data)

    print("***Computing answer metrics***")
    answer_metrics = get_answer_metrics(gt_answers, predicted_answers)
    print("***Finished computing answer metrics***")

    eval_results = dict(
        metrics=dict(
            translation_metrics=translation_metrics, answer_metrics=answer_metrics
        ),
        data=[
            {
                **datum,
                "is_gt_malformed": gt["is_query_malformed"],
                "is_prediction_malformed": pred["is_query_malformed"],
            }
            for datum, gt, pred in zip(data, gt_answers, predicted_answers)
        ],
    )

    with open(args.output_file, "w") as f:
        json.dump(eval_results, f)


if __name__ == "__main__":
    eval()
