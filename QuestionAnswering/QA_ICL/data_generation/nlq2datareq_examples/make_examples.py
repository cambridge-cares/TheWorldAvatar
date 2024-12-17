from argparse import ArgumentParser
import json
import math
import os
import random

from openai import OpenAI
from tqdm import tqdm

PROMPT_TEMPLATE = """You are a Blazegraph SPARQL expert creating a data set to train a robust machine learning model .

### Examples:
{examples}
{info}
Your task is to generate {num} more input-output pairs that are based on the given examples and obey the provided schema. Come up with new schema-compliant question types if needed. Please formulate your response as a list of objects in the JSON format exactly."""


def promptify_schema(filename: str):
    """Given path to JSON file of simplified schema, returns a concise textual description of the schema."""
    with open(filename, "r") as f:
        schema = json.load(f)

    return """Node types:
{node_types}

Edge types:
{edge_types}

Relations:
{relations}
""".format(
        node_types=json.dumps(schema["node_types"], indent=4),
        edge_types=json.dumps(schema["edge_types"], indent=4),
        relations="\n".join(
            "({s})-[{p}]->({o})".format(s=row["s"], p=row["p"], o=row["o"])
            for row in schema["relations"]
        ),
    )


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filename", help="JSON file containing examples")
    parser.add_argument("--repeats", type=int, default=1)
    parser.add_argument("--chunk_size", type=int, default=5)
    parser.add_argument("--synthesis_multiplier", type=int, default=2)
    parser.add_argument(
        "--path2schema",
        help="JSON file for schema",
    )
    parser.add_argument(
        "--path2categories", help="JSON file containing values per categorical property"
    )
    parser.add_argument(
        "--path2lexicon", help="JSON file containing labels for schema items"
    )
    parser.add_argument(
        "--output_dir", required=True, help="Directory to save output files"
    )
    parser.add_argument("--openai_base_url")
    parser.add_argument("--openai_model")
    args = parser.parse_args()

    with open(args.filename, "r") as f:
        examples = json.load(f)

    info = ""
    if args.path2schema:
        info += "\n### Schema:\n" + promptify_schema(args.path2schema)
    if args.path2categories:
        with open(args.path2categories, "r") as f:
            categories = f.read()
        info += "\n\n### Categorical properties:\n" + categories
    if args.path2lexicon:
        with open(args.path2lexicion, "r") as f:
            lexicon = f.read()
        info += "\n\n### Lexicon:\n" + lexicon

    openai_client = OpenAI(base_url=args.openai_base_url)

    new_examples = []
    corrupted_examples = []

    batches_per_repeat = math.ceil(len(examples) / args.chunk_size)
    for j in tqdm(range(args.repeats)):
        random.shuffle(examples)
        offset = j * batches_per_repeat
        for i in range(0, len(examples), args.chunk_size):
            prompt = PROMPT_TEMPLATE.format(
                examples=json.dumps(examples[i : i + args.chunk_size], indent=4),
                info=info + "\n",
                num=args.chunk_size * args.synthesis_multiplier,
            )
            batch_num = offset + i
            print(f"Prompt {batch_num}:\n{prompt}")
            response = openai_client.chat.completions.create(
                model=args.openai_model, messages=[{"role": "user", "content": prompt}]
            )
            new = response.choices[0].message.content
            try:
                new_examples.extend(json.loads(new))
            except json.decoder.JSONDecodeError:
                corrupted_examples.append(new)

    os.makedirs(args.output_dir, exist_ok=True)
    if new_examples:
        with open(os.path.join(args.output_dir, "synthetic.json"), "w") as f:
            json.dump(new_examples, f, indent=4)

    if corrupted_examples:
        with open(os.path.join(args.output_dir, "corrupted.txt"), "w") as f:
            f.write("\n".join(corrupted_examples))
