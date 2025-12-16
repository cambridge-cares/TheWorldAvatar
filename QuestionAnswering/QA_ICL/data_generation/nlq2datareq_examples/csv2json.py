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
    clses = parse_json_if_not_na(row["var2cls"])
    entity_bindings = parse_json_if_not_na(row["entity_bindings"])
    const_bindings = parse_json_if_not_na(row["const_bindings"])
    pkeys = parse_json_if_not_na(row["sparql_pkeys"])

    if row["target"] == "sparql":
        req_form = {
            "type": "sparql",
            **{
                k: v
                for k, v in {
                    "triplestore": row["sparql_triplestore"],
                    "query": row["sparql_query"].replace("\r", ""),
                    "pkeys": pkeys if pkeys else [],
                }.items()
                if v or len(v) == 0
            },
        }
    elif row["target"] == "func":
        req_form = {
            "type": "func",
            "name": row["func_name"],
        }
    else:
        req_form = None

    data_req = {
        k: v
        for k, v in {
            "var2cls": clses,
            "entity_bindings": entity_bindings,
            "const_bindings": const_bindings,
            "req_form": req_form,
            "visualise": parse_json_if_not_na(row.get("visualise")),
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

    df = pd.read_csv(args.filename, encoding="UTF-8")
    examples = df.apply(csvrow2jsonobj, axis=1).to_list()
    with open(args.filename.rsplit(".", maxsplit=1)[0] + ".json", "w", encoding="UTF-8") as f:
        json.dump(examples, f, indent=4, ensure_ascii=False)
