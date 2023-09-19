from collections import defaultdict
import random
from typing import Dict, Iterable, List, Optional

from data_generation.constants import (
    CHEMICALCLASSES,
    IDENTIFIER_NAMES,
    PROPERTY_NAMES,
    USES,
)
from .retrieve_subgraph import SubgraphRetriever
from .make_example_head2tail import ExampleMakerHead2Tail


class DatasetFromKgMaker:
    def __init__(self):
        self.subgraph_retriever = SubgraphRetriever()
        self.example_maker_head2tail = ExampleMakerHead2Tail()

    def make_examples_1h_to_1t(self):
        """Covers all properties, identifiers, some uses, and some chemical classes"""
        examples: List[dict] = []
        missing_entries = defaultdict(list)

        for p in PROPERTY_NAMES:
            example = self.make_example(
                property_names=[p], tail_nums=dict(property_num=1)
            )
            if example is None:
                missing_entries["property"].append(p)
            else:
                examples.append(example)

        for i in IDENTIFIER_NAMES:
            example = self.make_example(
                identifier_names=[i], tail_nums=dict(identifier_num=1)
            )
            if example is None:
                missing_entries["identifier"].append(i)
            else:
                examples.append(example)

        sampling_size = (len(PROPERTY_NAMES) + len(IDENTIFIER_NAMES)) // 4
        for u in random.sample(USES, sampling_size):
            example = self.make_example(uses=[u], tail_nums=dict(use_num=1))
            if example is None:
                missing_entries["use"].append(u)
            else:
                examples.append(example)

        for c in random.sample(CHEMICALCLASSES, sampling_size):
            example = self.make_example(
                chemicalclasses=[c], tail_nums=dict(chemicalclass_num=1)
            )
            if example is None:
                missing_entries["chemicalclass"].append(c)
            else:
                examples.append(example)

        if len(missing_entries) > 0:
            print("Missing entries: ")
            for k, v in missing_entries.items():
                print(k, ": ", v)

        return examples

    def make_examples_1h_to_2t(self):
        examples: List[dict] = []

        for _ in PROPERTY_NAMES:
            p_pair = random.sample(PROPERTY_NAMES, 2)
            example = self.make_example(property_names=p_pair, tail_nums=dict(property_num=2))
            if example is not None:
                examples.append(example)

        for _ in IDENTIFIER_NAMES:
            i_pair = random.sample(IDENTIFIER_NAMES, 2)
            example = self.make_example(identifier_names=i_pair, tail_nums=dict(identifier_num=2))
            if example is not None:
                examples.append(example)

        for _ in range((len(PROPERTY_NAMES) + len(IDENTIFIER_NAMES)) // 4):
            try_num = 0
            example = None

            while example is None and try_num < 3:
                tail_nums = self.get_tail_nums_total2()
                example = self.make_example(tail_nums)

            if example is not None:
                examples.append(example)

        return examples

    def make_examples_1h_to_3t(self):
        examples: List[dict] = []

        for _ in range((len(PROPERTY_NAMES) + len(IDENTIFIER_NAMES)) // 6):
            try_num = 0
            example = None

            while example is None and try_num < 3:
                tail_nums = self.get_tail_nums_total3()
                example = self.make_example(tail_nums=tail_nums)
                try_num += 1

            if example is not None:
                examples.append(example)

        return examples

    def make_example(
        self,
        property_names: Optional[Iterable[str]] = None,
        identifier_names: Optional[Iterable[str]] = None,
        uses: Optional[Iterable[str]] = None,
        chemicalclasses: Optional[Iterable[str]] = None,
        tail_nums: Dict[str, int] = dict(),
    ):
        if property_names is None:
            property_names = random.sample(
                PROPERTY_NAMES, tail_nums.get("property_num", 0)
            )
        if identifier_names is None:
            identifier_names = random.sample(
                IDENTIFIER_NAMES, tail_nums.get("identifier_num", 0)
            )
        if uses is None:
            uses = random.sample(USES, tail_nums.get("use_num", 0))
        if chemicalclasses is None:
            chemicalclasses = random.sample(
                CHEMICALCLASSES, tail_nums.get("chemicalclass_num", 0)
            )

        property_bindings = {
            f"hasProperty{i + 1}": "os:has" + property_name
            for i, property_name in enumerate(property_names)
        }
        identifier_bindings = {
            f"hasIdentifier{i + 1}": "os:has" + identifier_name
            for i, identifier_name in enumerate(identifier_names)
        }
        usevalue_bindings = {f"UseValue{i + 1}": use for i, use in enumerate(uses)}
        chemicalclassvalue_bindings = {
            f"ChemicalClassValue{i + 1}": chemicalclass
            for i, chemicalclass in enumerate(chemicalclasses)
        }

        subgraph = self.subgraph_retriever.get_subgraph(
            tail_nums=tail_nums,
            bindings={
                **property_bindings,
                **identifier_bindings,
                **usevalue_bindings,
                **chemicalclassvalue_bindings,
            },
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

    def get_tail_nums_total3(self):
        sampling_space_p_i = [
            (0, 1),
            (0, 2),
            (0, 3),
            (1, 0),
            (1, 1),
            (1, 2),
            (2, 0),
            (2, 1),
            (3, 0),
        ]
        property_num, identifier_num = random.choice(sampling_space_p_i)
        p_i_num = property_num + identifier_num
        if p_i_num == 1:
            use_num, chemicalclass_num = 1, 1
        elif p_i_num == 2:
            use_num, chemicalclass_num = random.choice([(0, 1), (1, 0)])
        elif p_i_num == 3:
            use_num, chemicalclass_num = 0, 0
        else:
            raise Exception(
                f"Unexpected `property_num` value of {property_num} and `identifier_num` value of {identifier_num}."
            )

        return dict(
            property_num=property_num,
            identifier_num=identifier_num,
            use_num=use_num,
            chemicalclass_num=chemicalclass_num,
        )


if __name__ == "__main__":
    pass
