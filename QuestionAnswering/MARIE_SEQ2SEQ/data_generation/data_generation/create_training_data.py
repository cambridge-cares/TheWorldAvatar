from datetime import datetime
import json
import random
import os

import numpy as np
from sklearn.model_selection import train_test_split

from data_generation.example_generator import ExampleGenerator, make_arg_samplers
from data_generation.constants import CHEMICALCLASSES, PKG_ROOT_DIRPATH, PROPERTY_NAMES, IDENTIFIER_NAMES, RESOURCE_DIRPATH, SPECIES, USES


delimiter = ''  # GPT best practice '\n\n###\n\n'
end_delimiter = '' # GPT best practice 'END'

timestamp = datetime.now().strftime("%Y-%m-%d_%H.%M.%S")

random.seed(2023)
np.random.seed(2023)


def generate_dataset(
    directory="data/",
    N_train: int = 30,
    N_dev: int = 3,
    N_test: int = 3,
):
    train_example_generators = []
    test_example_generators = []

    samplers = make_arg_samplers(
        properties=PROPERTY_NAMES,
        identifiers=IDENTIFIER_NAMES,
        species=SPECIES,
        chemicalclasses=CHEMICALCLASSES,
        uses=USES,
    )

    for dirpath, dirnames, filenames in os.walk(os.path.join(PKG_ROOT_DIRPATH, "templates")):
        if dirnames:
            dirnames.sort()
            continue

        template_name = os.path.basename(dirpath)

        with open(os.path.join(dirpath, "query_template.txt"), "r") as f:
            query_template = f.read().strip()

        with open(os.path.join(dirpath, "query_compact_template.txt"), "r") as f:
            query_compact_template = f.read().strip()

        with open(os.path.join(dirpath, "question_templates.txt"), "r") as f:
            question_templates = [x.strip() for x in f.readlines()]

        train_question_templates, test_question_templates = train_test_split(
            question_templates, test_size=0.1
        )

        train_example_generators.append(ExampleGenerator(
            template_name=template_name,
            query_template=query_template,
            query_compact_template=query_compact_template,
            qn_templates=train_question_templates,
            arg_samplers=samplers["train_arg_samplers"],
            val_sampler=samplers["val_sampler"],
            minvalue_maxvalue_sampler=samplers["minvalue_maxvalue_sampler"]
        ))
        test_example_generators.append(ExampleGenerator(
            template_name=template_name,
            query_template=query_template,
            query_compact_template=query_compact_template,
            qn_templates=test_question_templates,
            arg_samplers=samplers["test_arg_samplers"],
            val_sampler=samplers["val_sampler"],
            minvalue_maxvalue_sampler=samplers["minvalue_maxvalue_sampler"]
        ))

    train_examples = [next(example_generator) for _ in range(N_train) for example_generator in train_example_generators]
    dev_examples = [next(example_generator) for _ in range(N_dev) for example_generator in test_example_generators]
    test_examples = [next(example_generator) for _ in range(N_test) for example_generator in test_example_generators]

    json_dump(train_examples, os.path.join(directory, f"train_{timestamp}.json"))
    json_dump(dev_examples, os.path.join(directory, f"dev_{timestamp}.json"))
    json_dump(test_examples, os.path.join(directory, f"test_{timestamp}.json"))
    
    print("all done")


def json_dump(data, path: str):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        json.dump(data, f, indent=4)


def write_json(prompt, completion):
        data = {
            "prompt": prompt + delimiter,
            "completion": ' ' + completion + end_delimiter
        }
        return data


if __name__ == "__main__":
    generate_dataset(
        directory="data/",
        N_train=100,
        N_dev=10,
        N_test=10,
    )
