import argparse
import copy
import json
import random

from data_generation.word_augmenters import (
    random_delete,
    random_insert,
    random_swap,
    synonym_sub,
)


def get_word_count(text: str):
    tokens = text.split()
    return len(tokens)


def augment_data(data: dict, alpha: float, n_aug: int, inplace_prob: float):
    new_examples = []

    for _ in range(n_aug):
        for datum in data:
            augmenter = random.choice(
                [synonym_sub, random_swap, random_insert, random_delete]
            )
            new_example = copy.deepcopy(datum)
            max_ops = int(alpha * get_word_count(datum["question"]))

            new_example["question"] = augmenter(datum["question"], max_ops=max_ops)
            new_examples.append(new_example)

    for datum in data:
        if random.uniform(0, 1) > inplace_prob:
            continue

        augmenter = random.choice(
            [synonym_sub, random_swap, random_insert, random_delete]
        )
        max_ops = int(alpha * get_word_count(datum["question"]))
        datum["question"] = augmenter(datum["question"], max_ops=max_ops)

    return data + new_examples


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_path", type=str, required=True)
    parser.add_argument("--output_path", type=str, required=True)
    parser.add_argument(
        "--alpha",
        type=float,
        default=0.1,
        help="rate of modifications per word",
    )
    parser.add_argument(
        "--n_aug",
        type=int,
        default=4,
        help="number of new examples for each original example",
    )
    parser.add_argument(
        "--inplace_prob",
        type=float,
        default=0,
        help="probability that the modification is made in-place",
    )
    args = parser.parse_args()

    with open(args.input_path, "r") as f:
        data = json.load(f)

    data_augmented = augment_data(data, alpha=args.alpha, n_aug=args.n_aug, inplace_prob=args.inplace_prob)

    with open(args.output_path, "w") as f:
        json.dump(data_augmented, f, indent=4)
