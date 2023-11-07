import json
import time

import transformers
from tqdm import tqdm

from core.args_schema import DatasetArguments, InferenceArguments, ModelArguments
from core.translate import HfTranslator, OrtHfTranslator


def infer():
    hfparser = transformers.HfArgumentParser(
        (ModelArguments, DatasetArguments, InferenceArguments)
    )
    model_args, data_args, infer_args = hfparser.parse_args_into_dataclasses()

    if model_args.model_format == "hf":
        trans_model = HfTranslator(
            model_args,
            max_new_tokens=infer_args.max_new_tokens,
        )
    elif model_args.model_format == "ort":
        trans_model = OrtHfTranslator(
            model_args,
            max_new_tokens=infer_args.max_new_tokens,
        )
    else:
        raise ValueError("Unsupported model format: " + model_args.model_format)

    with open(data_args.eval_data_path, "r") as task:
        data = json.load(task)

    data_out = []

    def task():
        for datum in tqdm(data):
            t_start = time.time()
            pred = trans_model.nl2sparql(datum["question"])
            t_end = time.time()
            datum_out = dict(
                question=datum["question"],
                query_sparql=datum["query"]["sparql_compact"],
                prediction=pred,
                latency=t_end - -t_start,
            )
            data_out.append(datum_out)

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

    with open(infer_args.out_file, "w") as f:
        json.dump(data_out, f, indent=4)

    if mem_usage is not None:
        with open(
            infer_args.out_file.rsplit(".", maxsplit=1)[0] + "_mem.txt", "w"
        ) as f:
            f.writelines("\n".join(str(x) for x in mem_usage))


if __name__ == "__main__":
    infer()
