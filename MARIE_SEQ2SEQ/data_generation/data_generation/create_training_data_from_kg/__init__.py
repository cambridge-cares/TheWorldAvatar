from collections import defaultdict

from data_generation.constants import IDENTIFIER_NAMES

from .make_examples_by_query_path import ExampleMakerByQueryPath


class DatasetFromKgMaker:
    def __init__(self):
        self.example_maker = ExampleMakerByQueryPath()

    def make_examples(self, repeats: int = 1):
        self.missing_entries = defaultdict(set)
        examples = []

        for _ in range(repeats):
            print("Generating examples with one head and one tail...")
            examples.extend(self.make_examples_1h_1t())

            print("Generating examples with one head and two tails...")
            examples.extend(self.make_examples_1h_2t())

            print("Generating examples with one head and three tails...")
            examples.extend(self.make_examples_1h_3t())

        if len(self.missing_entries) > 0:
            print("Missing entries: ")
            for k, v in self.missing_entries.items():
                print(k, ": ", v)

        return examples

    def make_examples_1h_1t(self):
        examples = []

        for query_path in ["h2t", "t2h"]:
            examples.extend(
                self.example_maker.make_examples_1h_1t(
                    query_path=query_path,
                    tail_class="property",
                    sampling_size=-1,
                )
            )
            examples.extend(
                self.example_maker.make_examples_1h_1t(
                    query_path=query_path,
                    tail_class="identifier",
                    sampling_size=-1,
                )
            )
            sampling_size = 3
            examples.extend(
                self.example_maker.make_examples_1h_1t(
                    query_path=query_path,
                    tail_class="use",
                    sampling_size=sampling_size,
                )
            )
            examples.extend(
                self.example_maker.make_examples_1h_1t(
                    query_path=query_path,
                    tail_class="chemicalclass",
                    sampling_size=sampling_size,
                )
            )

        return examples

    def make_examples_1h_2t(self):
        examples = []

        for query_path in ["h2t", "t2h"]:
            examples.extend(
                self.example_maker.make_examples_1h_2t(
                    query_path=query_path,
                    tail_class="property",
                    sampling_size=-1,
                )
            )
        examples.extend(
            self.example_maker.make_examples_1h_2t(
                query_path="h2t",
                tail_class="identifier",
                sampling_size=-1,
            )
        )
        for query_path in ["h2t", "t2h", "t2t"]:
            examples.extend(
                self.example_maker.make_examples_1h_2t(
                    query_path=query_path,
                    sampling_size=len(IDENTIFIER_NAMES) if query_path == "t2t" else 5,
                )
            )

        return examples

    def make_examples_1h_3t(self):
        examples = []

        for query_path in ["h2t", "t2h", "t2t"]:
            examples.extend(
                self.example_maker.make_examples_1h_3t(
                    query_path=query_path,
                    sampling_size=len(IDENTIFIER_NAMES) if query_path == "t2t" else 5,
                )
            )

        return examples


if __name__ == "__main__":
    pass
