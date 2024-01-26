import csv
import json
import os
import time
import pandas as pd

import transformers
from tqdm import tqdm

from core.args_schema import DatasetArguments, InferenceArguments, ModelArguments
from core.translate import Translator

HEADER = [
    "id",
    "question",
    "groundtruth_domain",
    "groundtruth_sparql",
    "prediction_domain",
    "prediction_sparql",
    "latency",
]


def infer():
    hfparser = transformers.HfArgumentParser(
        (ModelArguments, DatasetArguments, InferenceArguments)
    )
    model_args, data_args, infer_args = hfparser.parse_args_into_dataclasses()

    os.makedirs(os.path.dirname(infer_args.out_file), exist_ok=True)
    trans_model = Translator(
        model_args,
        domain=None if data_args.domain == "multi" else data_args.domain,
        max_new_tokens=infer_args.max_new_tokens,
    )

    if data_args.eval_data_path.endswith("json"):
        with open(data_args.eval_data_path, "r") as f:
            data = json.load(f)
    elif data_args.eval_data_path.endswith("csv"):
        df = pd.read_csv(data_args.eval_data_path)
        data = df.apply(
            lambda row: dict(
                id=row["id"],
                domain=row["domain"],
                question=row["nlq"],
                query=dict(sparql=row["sparql"]),
            ),
            axis=1,
        )
    else:
        raise ValueError("Unrecognised data format: " + data_args.eval_data_path)

    if not os.path.exists(infer_args.out_file):
        os.makedirs(os.path.dirname(infer_args.out_file), exist_ok=True)
        f = open(infer_args.out_file, "w")
        writer = csv.writer(f)
        writer.writerow(HEADER)
    else:
        df = pd.read_csv(infer_args.out_file)
        data = [datum for datum in data if datum["id"] not in df["id"]]

        f = open(infer_args.out_file, "w")
        writer = csv.writer(f)

    def task():
        for datum in tqdm(data):
            t_start = time.time()
            pred = trans_model.nl2sparql(datum["question"])
            t_end = time.time()

            domain = (
                data_args.domain if data_args.domain != "multi" else datum["domain"]
            )
            writer.writerow(
                [
                    datum["id"],
                    datum["question"],
                    domain,
                    datum["query"]["sparql"],
                    pred["domain"],
                    pred["sparql"]["decoded"],
                    t_end - t_start,
                ]
            )

    if infer_args.do_profile:
        try:
            import memory_profiler

            mem_usage = memory_profiler.memory_usage(task)
        except ImportError:
            raise ValueError(
                "To enable memory profiling feature, please install memory_profiler i.e. `pip install memory_profiler`."
            )
    else:
        mem_usage = None
        task()

    if mem_usage is not None:
        memfile = infer_args.out_file.rsplit(".", maxsplit=1)[0] + "_mem.txt"
        with open(memfile, "w") as f:
            f.writelines("\n".join(str(x) for x in mem_usage))


if __name__ == "__main__":
    infer()
