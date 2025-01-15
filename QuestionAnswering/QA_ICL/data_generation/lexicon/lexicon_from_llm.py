from argparse import ArgumentParser
import json
import os

from openai import OpenAI


PROMPT_TEMPLATE = """Here are information on entity classes:
{class_data}

Below are some entity instances:
{instance_data}

Your task is to generate {k} surface forms for each entity instance to train an entity linking model. Do not provide any explanation and please formulate your response as a list of objects with keys "iri" and "surface_forms", in the JSON format exactly."""

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument(
        "filename", help="Path to JSON file containing class and instance data"
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

    class_data = data.get("classes")
    instance_data = data["instances"]

    # If output JSONL file exists, filter input for those that have not been processed
    out_filename = args.out.rsplit(".", maxsplit=1)[0]
    out_jsonl = out_filename + ".jsonl"
    if os.path.exists(out_jsonl):
        with open(out_jsonl, "r") as f:
            iris = [json.loads(line)["iri"] for line in f.read().splitlines()]
        iris = set(iris)
        instance_data = [datum for datum in instance_data if datum["iri"] not in iris]

    if instance_data:
        iri2label = {
            datum["iri"]: datum["attributes"]["label"] for datum in instance_data
        }
        openai_client = OpenAI(base_url=args.openai_base_url)

        with open(out_jsonl, "a+") as f:
            for i in range(0, len(instance_data), args.chunk_size):
                chunk = instance_data[i : i + args.chunk_size]
                prompt = PROMPT_TEMPLATE.format(
                    class_data=json.dumps(class_data, indent=4),
                    instance_data=json.dumps(chunk, indent=4),
                    k=args.surface_form_num,
                )

                print("Prompt:")
                print(prompt)

                expected_iris = set([row["iri"] for row in chunk])

                try_num = 0
                while try_num < 5:
                    try:
                        print("try_num: " + str(try_num))
                        res = openai_client.chat.completions.create(
                            model=args.model,
                            messages=[{"role": "user", "content": prompt}],
                        )
                        items = json.loads(res.choices[0].message.content)

                        actual_iris = set([item["iri"] for item in items])
                        if actual_iris != expected_iris:
                            raise ValueError(
                                f"Output IRIs do not match input IRIs.\nInput IRIs:\n{expected_iris}\nOutput IRIs:\n{actual_iris}"
                            )

                        break
                    except Exception as e:
                        print(e)
                        try_num += 1

                if try_num >= 5:
                    raise ValueError("Receive no valid JSON response after 5 tries.")

                for item in items:
                    item = {
                        "iri": item["iri"],
                        "clsname": item["clsname"],
                        "label": iri2label[item["iri"]],
                        "surface_forms": item["surface_forms"],
                    }
                    f.write(json.dumps(item))
                    f.write("\n")
                f.flush()

    # Convert JSONL to JSON
    with open(out_jsonl, "r") as f:
        instance_data = [json.loads(line) for line in f.read().splitlines()]
    with open(args.out, "w") as f:
        json.dump(instance_data, f, indent=4)
