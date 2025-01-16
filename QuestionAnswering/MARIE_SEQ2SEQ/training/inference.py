import json
import time

import transformers
from tqdm import tqdm

from core.arguments_schema import DatasetArguments, InferenceArguments, ModelArguments
from core.translation import (
    ONmtTranslationModel,
    HfTranslationModel,
    OVHfTranslationModel,
    OrtHfTranslationModel,
)


def rename_dict_keys(d: dict, mappings: dict):
    return {mappings[k] if k in mappings else k: v for k, v in d.items()}


def infer():
    hfparser = transformers.HfArgumentParser(
        (ModelArguments, DatasetArguments, InferenceArguments)
    )
    model_args, data_args, infer_args = hfparser.parse_args_into_dataclasses()

    if model_args.model_format == "hf":
        trans_model = HfTranslationModel(
            model_args,
            max_new_tokens=infer_args.max_new_tokens,
            do_torch_compile=infer_args.do_torch_compile,
            do_correct=infer_args.do_correct,
        )
    elif model_args.model_format == "ov":
        trans_model = OVHfTranslationModel(
            model_args,
            max_new_tokens=infer_args.max_new_tokens,
            do_correct=infer_args.do_correct,
        )
    elif model_args.model_format == "ort":
        trans_model = OrtHfTranslationModel(
            model_args,
            max_new_tokens=infer_args.max_new_tokens,
            do_correct=infer_args.do_correct,
        )
    elif model_args.model_format == "onmt":
        trans_model = ONmtTranslationModel(
            model_args,
            max_new_tokens=infer_args.max_new_tokens,
            do_correct=infer_args.do_correct,
        )
    else:
        raise ValueError("Unsupported model format: " + model_args.model_format)

    with open(data_args.eval_data_path, "r") as f:
        data = json.load(f)

    preds = []

    def f():
        for datum in tqdm(data):
            t_start = time.time()
            pred = trans_model.nl2sparql(datum["question"])
            pred["latency"] = time.time() - t_start
            preds.append(pred)

    if infer_args.do_profile:
        import memory_profiler
        mem_usage = memory_profiler.memory_usage(f)
    else:
        mem_usage = None
        f()

    data_out = [
        {
            **rename_dict_keys(
                datum, {"sparql_query": "gt", "sparql_query_compact": "gt_compact"}
            ),
            **pred,
        }
        for datum, pred in zip(data, preds)
    ]
    with open(infer_args.out_file, "w") as f:
        json.dump(data_out, f, indent=4)
    
    if mem_usage is not None:
        with open(infer_args.out_file.rsplit(".", maxsplit=1)[0] + "_mem.txt", "w") as f:
            f.writelines("\n".join(str(x) for x in mem_usage))


if __name__ == "__main__":
    infer()
