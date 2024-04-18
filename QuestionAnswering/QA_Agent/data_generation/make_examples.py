from argparse import ArgumentParser
import json
import random

from openai import OpenAI
from tqdm import tqdm


PROMPT = """Here are some examples of input-output pairs:
{examples}
{schema}

Your task is to generate {num} more input-output pairs. Please formulate your response as a list of objects in the JSON format exactly."""


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filename", help="JSON file containing examples")
    parser.add_argument("--chunk_size", type=int, default=5)
    parser.add_argument(
        "--object_properties",
        help="JSON file containing lists of object properties per entity class",
    )
    parser.add_argument(
        "--categories", help="JSON file containing values per categorical property"
    )
    parser.add_argument("--openai_base_url")
    parser.add_argument("--openai_model")
    args = parser.parse_args()

    with open(args.filename, "r") as f:
        examples = json.load(f)

    random.shuffle(examples)

    schema = dict()
    if args.object_properties:
        with open(args.object_properties, "r") as f:
            schema["object_properties"] = json.load(f)
    if args.categories:
        with open(args.categories, "r") as f:
            schema["categorical_properties"] = json.load(f)

    if schema:
        schema_str = f"\nBelow is additional schema information that you must comply by:\n{json.dumps(schema, indent=4)}"
    else:
        schema_str = ""

    openai_client = OpenAI(base_url=args.openai_base_url)

    new_examples = []
    corrupted_examples = []
    for i in tqdm(range(0, len(examples), args.chunk_size)):
        prompt = PROMPT.format(
            examples=json.dumps(examples[i : i + args.chunk_size], indent=4),
            schema=schema_str,
            num=args.chunk_size * 2,
        )
        print(prompt)
        response = openai_client.chat.completions.create(
            model=args.openai_model, messages=[{"role": "user", "content": prompt}]
        )
        new = response.choices[0].message.content
        try:
            new_examples.extend(json.loads(new))
        except json.decoder.JSONDecodeError:
            corrupted_examples.append(new)

    filename_prefix = args.filename.rsplit(".", maxsplit=1)[0]
    if new_examples:
        with open(filename_prefix + "_synthetic.json", "w") as f:
            json.dump(new_examples, f, indent=4)

    if corrupted_examples:
        with open(filename_prefix + "_corrupted.txt", "w") as f:
            f.write("\n".join(corrupted_examples))
