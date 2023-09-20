from collections import defaultdict

from data_generation.constants import PROPERTY_NAMES
from .make_examples_by_query_path import ExampleMakerByQueryPath


class DatasetFromKgMaker:
    def __init__(self):
        self.example_maker = ExampleMakerByQueryPath()

    def make_examples(self, repeats: int = 1):
        examples = []

        for _ in range(repeats):
            print("Generating examples with one head and one tail...")
            examples.extend(self.make_examples_1h_1t())

            print("Generating examples with one head and two tails...")
            examples.extend(self.make_examples_1h_2t())

            print("Generating examples with one head and three tails...")
            examples.extend(self.make_examples_1h_3t())

            print("Generating examples with two heads and one tail...")
            examples.extend(self.make_examples_2h_1t())

            print("Generating examples with two heads and two tails...")
            examples.extend(self.make_examples_2h_2t())

            print("Generating examples with two heads and three tails...")
            examples.extend(self.make_examples_2h_3t())

        print("Finish generating examples!")

        missing_entries = self.example_maker.get_missing_entries()
        if len(missing_entries) > 0:
            print("Missing entries: ")
            for k, v in missing_entries.items():
                print(k, ": ", v)

        for i, example in enumerate(examples):
            example["id"] = i

        return examples

    def make_examples_1h_1t(self):
        examples = []

        for query_path in ["h2t", "t2h"]:
            examples.extend(
                self.example_maker.make_examples_1t(
                    query_path=query_path,
                    tail_class="property",
                    sampling_size=-1,
                )
            )
            examples.extend(
                self.example_maker.make_examples_1t(
                    query_path=query_path,
                    tail_class="identifier",
                    sampling_size=-1,
                )
            )
            sampling_size = 3
            examples.extend(
                self.example_maker.make_examples_1t(
                    query_path=query_path,
                    tail_class="use",
                    sampling_size=sampling_size,
                )
            )
            examples.extend(
                self.example_maker.make_examples_1t(
                    query_path=query_path,
                    tail_class="chemicalclass",
                    sampling_size=sampling_size,
                )
            )

        for example in examples:
            example["subgraph_type"] = "1h_1t"

        return examples

    def make_examples_1h_2t(self):
        examples = []

        for query_path in ["h2t", "t2h"]:
            examples.extend(
                self.example_maker.make_examples_2t(
                    query_path=query_path,
                    tail_class="property",
                    sampling_size=-1,
                )
            )
        examples.extend(
            self.example_maker.make_examples_2t(
                query_path="h2t",
                tail_class="identifier",
                sampling_size=-1,
            )
        )
        for query_path in ["h2t", "t2h"]:
            examples.extend(
                self.example_maker.make_examples_2t(
                    query_path=query_path,
                    sampling_size=5,
                )
            )
        examples.extend(
            self.example_maker.make_examples_2t(
                query_path="t2t", sampling_size=len(PROPERTY_NAMES)
            )
        )

        for example in examples:
            example["subgraph_type"] = "1h_2t"

        return examples

    def make_examples_1h_3t(self):
        examples = []

        for query_path in ["h2t", "t2h"]:
            examples.extend(
                self.example_maker.make_examples_3t(
                    query_path=query_path,
                    sampling_size=5,
                )
            )
        examples.extend(
            self.example_maker.make_examples_3t(
                query_path="t2t", sampling_size=len(PROPERTY_NAMES)
            )
        )

        for example in examples:
            example["subgraph_type"] = "1h_3t"

        return examples

    def make_examples_2h_1t(self):
        examples = []

        examples.extend(
            self.example_maker.make_examples_1t(
                query_path="h2t",
                tail_class="property",
                sampling_size=3,
                head_num=2,
            )
        )
        examples.extend(
            self.example_maker.make_examples_1t(
                query_path="h2t",
                tail_class="identifier",
                sampling_size=2,
                head_num=2,
            )
        )
        examples.extend(
            self.example_maker.make_examples_1t(
                query_path="h2t",
                tail_class="use",
                sampling_size=1,
                head_num=2,
            )
        )
        examples.extend(
            self.example_maker.make_examples_1t(
                query_path="h2t",
                tail_class="chemicalclass",
                sampling_size=1,
                head_num=2,
            )
        )

        for example in examples:
            example["subgraph_type"] = "2h_1t"

        return examples

    def make_examples_2h_2t(self):
        examples = []

        examples.extend(
            self.example_maker.make_examples_2t(
                query_path="h2t",
                tail_class="property",
                sampling_size=3,
                head_num=2,
            )
        )
        examples.extend(
            self.example_maker.make_examples_2t(
                query_path="h2t",
                tail_class="identifier",
                sampling_size=2,
                head_num=2,
            )
        )
        examples.extend(
            self.example_maker.make_examples_2t(
                query_path="h2t",
                sampling_size=2,
                head_num=2,
            )
        )

        for example in examples:
            example["subgraph_type"] = "2h_2t"

        return examples

    def make_examples_2h_3t(self):
        examples = self.example_maker.make_examples_3t(
            query_path="h2t",
            sampling_size=3,
            head_num=2,
        )

        for example in examples:
            example["subgraph_type"] = "2h_3t"

        return examples
