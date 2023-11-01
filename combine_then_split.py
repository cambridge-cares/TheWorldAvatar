import ast
import json
import random
import time
from typing import List

import pandas as pd



def sanitize(texts: List[str]):
    def _sanitize(text: str):
        ptr = 0
        while ptr < len(text):
            if text.startswith("<entity>", ptr):
                ptr_end = text.find("</entity>", ptr)
                entity = text[ptr + len("<entity>"): ptr_end]
                text = text[:ptr] + entity + text[ptr_end + len("</entity>"):]
                ptr = ptr_end
            elif text[ptr] == "[":
                ptr_end = text.find("]", ptr)
                literal = text[ptr + 1: ptr_end]
                text = text[:ptr] + literal + text[ptr_end + 1:]
                ptr = ptr_end
            else:
                ptr += 1
        return text
    return [_sanitize(text) for text in texts]

def main():
    df = pd.read_csv("data/examples_2000_paraphrases.csv", index_col="id")
    df["paraphrases"] = df["paraphrases"].apply(ast.literal_eval)
    df["sampling_pool"] = df.apply(lambda row: [row["verbalization"]] + row["paraphrases"], axis=1)
    df["sampling_pool"] = df["sampling_pool"].apply(sanitize)

    with open("data/examples_2000.json", "r") as f:
        data = json.load(f)

    data_out = []
    for datum in data:
        try:
            if datum["id"] not in df.index:
                continue
            row = df.loc[datum["id"]]
            datum["paraphrases"] = row["paraphrases"]
            datum["question"] = random.choice(row["sampling_pool"])
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
        "train": ids[test_size + dev_size:]
    }

    time_label = time.strftime("%Y-%m-%d_%H:%M:%S")
    for split, _ids in ids.items():
        split_data = [x for x in data_out if x["id"] in _ids]
        with open(f"data/{time_label}_{split}.json", "w") as f:
            json.dump(split_data, f, indent=4)

if __name__ == "__main__":
    main()