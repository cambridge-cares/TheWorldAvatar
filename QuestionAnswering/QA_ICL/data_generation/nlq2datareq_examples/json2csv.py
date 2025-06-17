from argparse import ArgumentParser
import csv
import json
from typing import Optional


def json_dumps_if_not_none(data: Optional[dict]):
    if not data:
        return None
    return json.dumps(data, indent=2)


def jsonobj2csvrow(obj: dict):
    data_req = obj["data_req"]
    req_form = data_req.get("req_form")

    return (
        obj["nlq"],
        req_form.get("type") if req_form else None,
        json_dumps_if_not_none(data_req.get("var2cls")),
        json_dumps_if_not_none(data_req.get("entity_bindings")),
        json_dumps_if_not_none(data_req.get("const_bindings")),
        req_form.get("triplestore") if req_form else None,
        req_form.get("query") if req_form else None,
        json_dumps_if_not_none(req_form.get("pkeys") if req_form else None),
        req_form.get("name") if req_form else None,
        json_dumps_if_not_none(data_req.get("visualise")),
    )


HEADERS = (
    "question",
    "target",
    "var2cls",
    "entity_bindings",
    "const_bindings",
    "sparql_triplestore",
    "sparql_query",
    "sparql_pkeys",
    "func_name",
    "visualise"
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
