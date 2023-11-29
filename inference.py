import json
import os
import time

import transformers
from tqdm import tqdm

from core.args_schema import DatasetArguments, InferenceArguments, ModelArguments
from core.translate.multi_domain import MultiDomainTranslator
from core.translate.single_domain import SingleDomainTranslator


def infer():
    hfparser = transformers.HfArgumentParser(
        (ModelArguments, DatasetArguments, InferenceArguments)
    )
    model_args, data_args, infer_args = hfparser.parse_args_into_dataclasses()

    os.makedirs(os.path.dirname(infer_args.out_file), exist_ok=True)

    if data_args.domain == "multi":
        trans_model = MultiDomainTranslator(model_args, max_new_tokens=infer_args.max_new_tokens)
    else:
        trans_model = SingleDomainTranslator(model_args, domain=data_args.domain, max_new_tokens=infer_args.max_new_tokens)

    with open(data_args.eval_data_path, "r") as task:
        data = json.load(task)

    data_out = []

    def task():
        for datum in tqdm(data):
            t_start = time.time()
            pred = trans_model.nl2sparql(datum["question"])
            t_end = time.time()

            domain = data_args.domain if data_args.domain != "multi" else datum["domain"]
            datum_out = dict(
                id=datum["id"],
                question=datum["question"],
                groundtruth=dict(sparql=datum["query"]["sparql"], domain=domain),
                prediction=pred,
                latency=t_end - t_start,
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
        memfile = infer_args.out_file.rsplit(".", maxsplit=1)[0] + "_mem.txt"
        with open(memfile, "w") as f:
            f.writelines("\n".join(str(x) for x in mem_usage))


if __name__ == "__main__":
    infer()
