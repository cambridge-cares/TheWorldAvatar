from argparse import ArgumentParser
import json

import pandas as pd


def csvrow2jsonobj(row: pd.Series):
    output = dict()
    if row["target"] == "sparql":
        try:
            bindings = (
                json.loads(row["sparql_bindings"])
                if not pd.isna(row["sparql_bindings"])
                else None
            )
        except Exception as e:
            print(row["sparql_bindings"])
            raise e

        try:
            nodes_to_augment = (
                json.loads(row["sparql_nodes_to_augment"])
                if not pd.isna(row["sparql_nodes_to_augment"])
                else None
            )
        except Exception as e:
            print(row["sparql_nodes_to_augment"])
            raise e

        output = {
            "action_type": "sparql",
            **{
                k: v
                for k, v in {
                    "namespace": row["sparql_namespace"],
                    "bindings": bindings,
                    "query": row["sparql_query"],
                    "nodes_to_augment": nodes_to_augment,
                }.items()
                if v
            },
        }
    elif row["target"] == "func":
        output = {
            "action_type": "func",
            "name": row["func_name"],
            "args": json.loads(row["func_args"]),
        }
    else:
        raise ValueError(f'Unexpected target: {row["target"]}')
    return {"nlq": row["question"], "action": output}


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filename", help="CSV file containing examples")
    args = parser.parse_args()

    df = pd.read_csv(args.filename, encoding="ISO-8859-1")
    examples = df.apply(csvrow2jsonobj, axis=1).to_list()
    with open(args.filename.rsplit(".", maxsplit=1)[0] + ".json", "w") as f:
        json.dump(examples, f, indent=4)
