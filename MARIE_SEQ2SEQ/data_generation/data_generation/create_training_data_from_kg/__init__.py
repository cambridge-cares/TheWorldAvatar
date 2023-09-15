from collections import defaultdict
import itertools
import random
from typing import Iterable, List, Optional, Tuple

from data_generation.constants import (
    CHEMICALCLASSES,
    IDENTIFIER_NAMES,
    PROPERTY_NAMES,
    USES,
)
from data_generation.create_training_data_from_kg.retrieve_subgraph import (
    SubgraphRetriever,
)
from data_generation.create_training_data_from_kg.make_example_head2tail import (
    ExampleMakerHead2Tail,
)


class DatasetFromKgMaker:
    def __init__(self):
        self.subgraph_retriever = SubgraphRetriever()
        self.example_maker_head2tail = ExampleMakerHead2Tail()

    def make_examples_factoid_single(self):
        """Covers all properties, identifiers, some uses, and some chemical classes"""
        examples: List[dict] = []
        missing_entries = defaultdict(list)

        for p in PROPERTY_NAMES:
            example = self.make_example_from_properties([p])
            if example is None:
                missing_entries["property"].append(p)
            else:
                examples.append(example)

        for i in IDENTIFIER_NAMES:
            example = self.make_example_from_identifiers([i])
            if example is None:
                missing_entries["identifier"].append(i)
            else:
                examples.append(example)

        sampling_size = (len(PROPERTY_NAMES) + len(IDENTIFIER_NAMES)) // 2
        for u in random.sample(USES, sampling_size):
            example = self.make_example_use_single(u)
            if example is None:
                missing_entries["use"].append(u)
            else:
                examples.append(example)

        for c in random.sample(CHEMICALCLASSES, sampling_size):
            example = self.make_example_chemicalclass_single(c)
            if example is None:
                missing_entries["chemicalclass"].append(c)
            else:
                examples.append(example)

        print("Missing entries: ")
        print(missing_entries)

        return examples

    def make_example_from_properties(self, property_names: Iterable[str]):
        subgraph = self.subgraph_retriever.get_subgraph(
            tail_nums=defaultdict(lambda: 0, property_num=len(property_names)),
            bindings={
                f"hasProperty{i + 1}": "os:has" + property_name
                for i, property_name in enumerate(property_names)
            },
        )
        return self.example_maker_head2tail.make_example(subgraph)

    def make_example_from_identifiers(self, identifier_names: Iterable[str]):
        subgraph = self.subgraph_retriever.get_subgraph(
            tail_nums=defaultdict(lambda: 0, identifier_num=len(identifier_names)),
            bindings={
                f"hasIdentifier{i + 1}": "os:has" + identifier_name
                for i, identifier_name in enumerate(identifier_names)
            },
        )
        return self.example_maker_head2tail.make_example(subgraph)

    def make_example_use_single(self, use: str):
        subgraph = self.subgraph_retriever.get_subgraph(
            tail_nums=defaultdict(lambda: 0, use_num=1),
            bindings=dict(UseValue=use),
        )
        return self.example_maker_head2tail.make_example(subgraph)

    def make_example_chemicalclass_single(self, chemicalclass: str):
        subgraph = self.subgraph_retriever.get_subgraph(
            tail_nums=defaultdict(lambda: 0, chemicalclass_num=1),
            bindings=dict(ChemicalClassValue=chemicalclass),
        )
        return self.example_maker_head2tail.make_example(subgraph)


if __name__ == "__main__":
    pass
