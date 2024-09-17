from argparse import ArgumentParser
import json

import pandas as pd


def csvrow2jsonobj(row: pd.Series):
    if pd.isna(row["template"]) or pd.isna(row["quantities"]):
        prediction = None
    else:
        prediction = {
            "template": row["template"],
            "quantities": json.loads(row["quantities"]),
        }
    return {"text": row["text"], "prediction": prediction}


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("file", help="Path to CSV file")
    args = parser.parse_args()

    df = pd.read_csv(args.file, encoding="ISO-8859-1")
    examples = df.apply(csvrow2jsonobj, axis=1).to_list()
    with open(args.file.rsplit(".", maxsplit=1)[0] + ".json", "w") as f:
        json.dump(examples, f, indent=4)
