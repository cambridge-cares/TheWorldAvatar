import argparse
import csv
import json
import os
from typing import Optional

from tqdm import tqdm
import pandas as pd

from .base import Paraphraser
from .ontospecies import OSParaphraser

HEADER = ["id", "verbalization", "paraphrases"]

def get_paraphraser(domain: Optional[str], endpoint, api_key, model):
    if domain == "ontospecies":
        return OSParaphraser(endpoint, api_key, model)
    else:
        return Paraphraser(endpoint, api_key, model)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("filepath")
    parser.add_argument("--domain", type=str, required=False)
    parser.add_argument("--endpoint", type=str, required=False)
    parser.add_argument("--api_key", type=str, required=False)
    parser.add_argument("--model", type=str, required=False)
    args = parser.parse_args()

    paraphraser = get_paraphraser(args.domain, args.endpoint, args.api_key, args.model)
    
    with open(args.filepath, "r") as f:
        data = json.load(f)

    filepath_out = args.filepath.rsplit(".", maxsplit=1)[0] + "_paraphrases.csv"
    print("Writing to file: ", filepath_out)
    if not os.path.exists(filepath_out):
        f = open(filepath_out, "w")
        writer = csv.writer(f)
        writer.writerow(HEADER)
    else:
        df = pd.read_csv(filepath_out)
        data = [datum for datum in data if datum["id"] not in df["id"]]

        f = open(filepath_out, "a")
        writer = csv.writer(f)

    try:
        for datum in tqdm(data):
            paraphrases = paraphraser.paraphrase(datum["verbalization"])
            writer.writerow([datum["id"], datum["verbalization"], paraphrases])
            f.flush()
    except Exception as e:
        f.close()
        raise e