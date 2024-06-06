from argparse import ArgumentParser
import csv
import json
from typing import Optional

import pandas as pd


def json_dumps_if_not_none(data: Optional[dict]):
    if not data:
        return None
    return json.dumps(data, indent=2)


def jsonobj2csvrow(obj: dict):
    data_req = obj["data_req"]
    req_form = data_req["req_form"]

    return (
        obj["nlq"],
        req_form["type"],
        json_dumps_if_not_none(data_req.get("entity_bindings")),
        json_dumps_if_not_none(data_req.get("const_bindings")),
        req_form.get("namespace"),
        req_form.get("query"),
        json_dumps_if_not_none(req_form.get("res_map")),
        req_form.get("name"),
    )


HEADERS = (
    "question",
    "target",
    "entity_bindings",
    "const_bindings",
    "sparql_namcespace",
    "sparql_query",
    "sparql_res_map",
    "func_name",
)

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filename", help="JSON file containing examples")
    args = parser.parse_args()

    with open(args.filename, "r") as f:
        data = json.load(f)

    if not isinstance(data, list):
        raise ValueError("File content is not a JSON list")

    with open(args.filename.rsplit(".")[0] + ".csv", "w") as f:
        writer = csv.writer(f)
        writer.writerow(HEADERS)
        for datum in data:
            writer.writerow(jsonobj2csvrow(datum))
