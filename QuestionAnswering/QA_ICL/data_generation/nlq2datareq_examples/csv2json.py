from argparse import ArgumentParser
import json

import pandas as pd


def parse_json_if_not_na(obj):
    if pd.isna(obj):
        return None
    try:
        return json.loads(obj)
    except Exception as e:
        print("Invalid JSON: ")
        print(obj)
        raise e


def csvrow2jsonobj(row: pd.Series):
    entity_bindings = parse_json_if_not_na(row["entity_bindings"])
    const_bindings = parse_json_if_not_na(row["const_bindings"])

    if row["target"] == "sparql":
        res_map = parse_json_if_not_na(row["sparql_res_map"])

        req_form = {
            "type": "sparql",
            **{
                k: v
                for k, v in {
                    "namespace": row["sparql_namespace"],
                    "query": row["sparql_query"],
                    "res_map": res_map,
                }.items()
                if v
            },
        }
    elif row["target"] == "func":
        req_form = {
            "type": "func",
            "name": row["func_name"],
        }
    else:
        raise ValueError(f'Unexpected target: {row["target"]}')

    data_req = {
        k: v
        for k, v in {
            "entity_bindings": entity_bindings,
            "const_bindings": const_bindings,
            "req_form": req_form,
        }.items()
        if v
    }

    return {
        "nlq": row["question"],
        "data_req": data_req,
    }


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filename", help="CSV file containing examples")
    args = parser.parse_args()

    df = pd.read_csv(args.filename, encoding="ISO-8859-1")
    examples = df.apply(csvrow2jsonobj, axis=1).to_list()
    with open(args.filename.rsplit(".", maxsplit=1)[0] + ".json", "w") as f:
        json.dump(examples, f, indent=4)
