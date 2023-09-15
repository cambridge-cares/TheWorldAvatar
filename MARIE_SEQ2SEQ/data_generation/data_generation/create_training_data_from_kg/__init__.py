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

    def make_examples_factoid_double(self):
        examples: List[dict] = []
        
        for _ in PROPERTY_NAMES:
            p_pair = random.sample(PROPERTY_NAMES, 2)
            example = self.make_example_from_properties(p_pair)
            if example is not None:
                examples.append(example)

        for _ in IDENTIFIER_NAMES:
            i_pair = random.sample(IDENTIFIER_NAMES, 2)
            example = self.make_example_from_identifiers(i_pair)
            if example is not None:
                examples.append(example)

        for _ in range(len(PROPERTY_NAMES) + len(IDENTIFIER_NAMES)):
            try_num = 0
            example = None

            while example is None and try_num < 3:
                example = self.make_example_factoid_double_rand()

            if example is not None:
                examples.append(example)

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

    def make_example_factoid_double_rand(self):
        tail_nums = self.get_tail_nums_total2()
        
        property_names = random.sample(PROPERTY_NAMES,tail_nums["property_num"])
        property_bindings = {
            f"hasProperty{i + 1}": "os:has" + property_name
            for i, property_name in enumerate(property_names)
        }
        identifier_names = random.sample(IDENTIFIER_NAMES, tail_nums["identifier_num"])
        identifier_bindings = {
                f"hasIdentifier{i + 1}": "os:has" + identifier_name
                for i, identifier_name in enumerate(identifier_names)
            }
        
        subgraph = self.subgraph_retriever.get_subgraph(
            tail_nums=tail_nums,
            bindings= {**property_bindings, **identifier_bindings},
        )
        return self.example_maker_head2tail.make_example(subgraph)

    def get_tail_nums_total2(self):
        property_num = random.randint(0, 2)
        identifier_num = random.randint(property_num, 2) - property_num
        p_i_num = property_num + identifier_num
        if p_i_num == 0:
            use_num, chemicalclass_num = 1, 1
        elif p_i_num == 1:
            use_num, chemicalclass_num = random.choice([(0, 1), (1, 0)])
        else:
            use_num, chemicalclass_num = 0, 0
        return dict(
            property_num=property_num,
            identifier_num=identifier_num,
            use_num=use_num,
            chemicalclass_num=chemicalclass_num,
        )


if __name__ == "__main__":
    pass
