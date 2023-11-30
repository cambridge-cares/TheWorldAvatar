import argparse
import ast
import json
import os
import random
import time

import pandas as pd


def sanitize(text: str):
    ptr = 0
    while ptr < len(text):
        if text.startswith("<entity>", ptr):
            ptr_end = text.find("</entity>", ptr)
            entity = text[ptr + len("<entity>") : ptr_end]
            text = text[:ptr] + entity + text[ptr_end + len("</entity>") :]
            ptr += len(entity)
        elif text[ptr] == "[":
            ptr_end = text.find("]", ptr)
            literal = text[ptr + 1 : ptr_end]
            text = text[:ptr] + literal + text[ptr_end + 1 :]
            ptr += len(literal)
        else:
            ptr += 1
    return text


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath_examples", type=str, required=True)
    parser.add_argument("--filepath_paraphrases", type=str, required=True)
    parser.add_argument("--dirpath_out", type=str, required=True)
    args = parser.parse_args()

    df = pd.read_csv(args.filepath_paraphrases, index_col="id")
    df["paraphrases"] = df["paraphrases"].apply(ast.literal_eval)
    df["sampling_pool"] = df.apply(
        lambda row: [row["verbalization"]] + row["paraphrases"], axis=1
    )

    with open(args.filepath_examples, "r") as f:
        data = json.load(f)

    data_out = []
    for datum in data:
        try:
            if datum["id"] not in df.index:
                continue
            row = df.loc[datum["id"]]
            datum["paraphrases"] = row["paraphrases"]
            datum["question"] = sanitize(random.choice(row["sampling_pool"]))
            data_out.append(datum)
        except Exception as e:
            print(datum)
            raise e

    ids = df.sample(frac=1).index.to_list()
    test_size = len(ids) // 10
    dev_size = len(ids) // 10

    ids = {
        "test": ids[:test_size],
        "dev": ids[test_size : test_size + dev_size],
        "train": ids[test_size + dev_size :],
    }

    os.makedirs(args.dirpath_out, exist_ok=True)

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    for split, _ids in ids.items():
        split_data = [x for x in data_out if x["id"] in _ids]

        filename = f"{time_label}_{split}.json"
        filepath = os.path.join(args.dirpath_out, filename)

        with open(filepath, "w") as f:
            json.dump(split_data, f, indent=4)


if __name__ == "__main__":
    main()
