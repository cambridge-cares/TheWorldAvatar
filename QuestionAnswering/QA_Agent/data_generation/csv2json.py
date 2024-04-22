from argparse import ArgumentParser
import json

import pandas as pd


def csvrow2jsonobj(row: pd.Series):
    output = dict()
    if row["target"] == "sparql":
        output["sparql"] = dict(
            namespace=row["sparql_namespace"], query=row["sparql_query"]
        )
    elif row["target"] == "func":
        output["func"] = dict(name=row["func_name"], args=row["func_args"])
    else:
        raise ValueError(f'Unexpected target: {row["target"]}')
    return dict(input=row["question"], output=output)


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filename", help="CSV file containing examples")
    args = parser.parse_args()

    df = pd.read_csv(args.filename)
    examples = df.apply(csvrow2jsonobj, axis=1).to_list()
    with open(args.filename.rsplit(".", maxsplit=1)[0] + ".json", "w") as f:
        json.dump(examples, f, indent=4)
