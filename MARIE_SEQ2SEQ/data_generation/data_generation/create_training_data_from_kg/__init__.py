from collections import defaultdict

from .make_examples_by_query_path import ExampleMakerByQueryPath


class DatasetFromKgMaker:
    def __init__(self):
        self.example_maker = ExampleMakerByQueryPath()

    def make_examples(self, repeats: int = 1):
        self.missing_entries = defaultdict(set)
        examples = []

        for _ in range(repeats):
            print("Generating examples with one head and one tail...")
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

            print("Generating examples with one head and two tails...")
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
                        query_path=query_path, sampling_size=5
                    )
                )

            print("Generating examples with one head and three tails...")
            for query_path in ["h2t", "t2h", "t2t"]:
                examples.extend(
                    self.example_maker.make_examples_1h_3t(
                        query_path=query_path, sampling_size=5
                    )
                )

        if len(self.missing_entries) > 0:
            print("Missing entries: ")
            for k, v in self.missing_entries.items():
                print(k, ": ", v)

        return examples


if __name__ == "__main__":
    pass
