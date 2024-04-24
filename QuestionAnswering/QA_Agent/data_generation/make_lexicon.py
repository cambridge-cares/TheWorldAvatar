from argparse import ArgumentParser
import json
import os

from openai import OpenAI


PROMPT_TEMPLATE = """Below are some entity and relationship data:
{data}

Your task is to generate {k} surface forms for each entity and relationship. Do not provide any explanation and please formulate your response as a list of objects with keys "iri" and "surface_forms", in the JSON format exactly."""

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument(
        "filename", help="Path to JSON file containing IRI and any associated data"
    )
    parser.add_argument(
        "--chunk_size",
        type=int,
        default=5,
        help="Number of entities to be processed in a batch",
    )
    parser.add_argument(
        "--surface_form_num",
        type=int,
        default=10,
        help="Number of surface forms to generate for each entity or relationship",
    )
    parser.add_argument("--out", required=True, help="Path to output JSON file")
    parser.add_argument("--openai_base_url")
    parser.add_argument("--model")
    args = parser.parse_args()

    with open(args.filename, "r") as f:
        data = json.load(f)

    # If output JSONL file exists, filter input for those that have not been processed
    out_filename = args.out.rsplit(".", maxsplit=1)[0]
    out_jsonl = out_filename + ".jsonl"
    if os.path.exists(out_jsonl):
        with open(out_jsonl, "r") as f:
            iris = [json.loads(line)["iri"] for line in f.read().splitlines()]
        iris = set(iris)
        data = [datum for datum in data if datum["iri"] not in iris]

    if data:
        iri2label = {datum["iri"]: datum["data"]["label"] for datum in data}
        openai_client = OpenAI(base_url=args.openai_base_url)

        with open(args.out, "a+") as f:
            for i in range(0, len(data), args.chunk_size):
                chunk = data[i : i + args.chunk_size]
                prompt = PROMPT_TEMPLATE.format(
                    data=json.dumps(chunk, indent=4), k=args.surface_form_num
                )
                res = openai_client.chat.completions.create(
                    model=args.model, messages=[{"role": "user", "content": prompt}]
                )
                items = json.loads(res.choices[0].message.content)
                for item in items:
                    item = dict(
                        iri=item["iri"],
                        label=iri2label[item["iri"]],
                        surfaceForms=item["surface_forms"],
                    )
                    f.write(json.dumps(item))
                    f.write("\n")
                f.flush()

    # Convert JSONL to JSON
    with open(out_jsonl, "r") as f:
        data = [json.loads(line) for line in f.read().splitlines()]
    with open(args.out, "w") as f:
        json.dump(data, f, indent=4)