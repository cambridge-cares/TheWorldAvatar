from collections import defaultdict
import itertools
import random
from typing import Dict, Iterable, List, Optional

from data_generation.constants import (
    CHEMICALCLASSES,
    IDENTIFIER_NAMES,
    PROPERTY_NAMES,
    USES,
)
from .make_example_head2tail import ExampleMakerHead2Tail
from .make_example_tail2head import ExampleMakerTail2Head
from .make_example_tail2tail import ExampleMakerTail2Tail
from .retrieve_subgraph import SubgraphRetriever
from .utils.tail_nums import get_tail_nums_total2, get_tail_nums_total3


class ExampleMakerByQueryPath:
    # SubstructureKeysFingerprint is not a numerical value for comparison.
    # We thus exclude it from tail-to-head queries.
    PROPERTY_NAMES_FOR_T2H = [
        x for x in PROPERTY_NAMES if x != "SubStructureKeysFingerprint"
    ]

    def __init__(self, kg_endpoint: str):
        self.subgraph_retriever = SubgraphRetriever(kg_endpoint)
        self.example_maker_head2tail = ExampleMakerHead2Tail()
        self.example_maker_tail2head = ExampleMakerTail2Head()
        self.example_maker_tail2tail = ExampleMakerTail2Tail()
        self.missing_entries = defaultdict(set)

    def get_missing_entries(self):
        return dict(self.missing_entries)

    def make_examples_1t(
        self,
        query_path: str,
        tail_class: str,
        sampling_size: int = 1,
        head_num: int = 1,
    ):
        examples: List[dict] = []

        if tail_class == "property":
            sampling_frame = (
                self.PROPERTY_NAMES_FOR_T2H if query_path == "t2h" else PROPERTY_NAMES
            )
            k = self.resolve_sampling_size(
                sampling_size=sampling_size, sampling_frame=sampling_frame
            )
            for p in random.sample(sampling_frame, k=k):
                example = self.make_example(
                    property_names=[p],
                    tail_nums=dict(property_num=1),
                    query_path=query_path,
                    head_num=head_num,
                )

                if example is None:
                    self.missing_entries["property"].add(p)
                else:
                    examples.append(example)

        elif tail_class == "identifier":
            k = self.resolve_sampling_size(
                sampling_size=sampling_size, sampling_frame=IDENTIFIER_NAMES
            )
            for i in random.sample(IDENTIFIER_NAMES, k=k):
                example = self.make_example(
                    identifier_names=[i],
                    tail_nums=dict(identifier_num=1),
                    query_path="h2t",
                    head_num=head_num,
                )

                if example is None:
                    self.missing_entries["identifier"].add(i)
                else:
                    examples.append(example)

        elif tail_class == "use":
            for u in random.sample(USES, k=sampling_size or 1):
                example = self.make_example(
                    uses=[u],
                    tail_nums=dict(use_num=1),
                    query_path=query_path,
                    head_num=head_num,
                )

                if example is None:
                    self.missing_entries["use"].add(u)
                else:
                    examples.append(example)

        elif tail_class == "chemicalclass":
            for c in random.sample(CHEMICALCLASSES, k=sampling_size):
                example = self.make_example(
                    chemicalclasses=[c],
                    tail_nums=dict(chemicalclass_num=1),
                    query_path=query_path,
                    head_num=head_num,
                )

                if example is None:
                    self.missing_entries["chemicalclass"].add(c)
                else:
                    examples.append(example)

        else:
            raise ValueError(
                f"Unexpected value for argument `tail_class`: {tail_class}."
            )

        return examples

    def make_examples_2t(
        self,
        query_path: str,
        tail_class: Optional[str] = None,
        sampling_size: int = 1,
        head_num: int = 1,
    ):
        examples: List[dict] = []

        if tail_class == "property":
            sampling_frame = (
                self.PROPERTY_NAMES_FOR_T2H if query_path == "t2h" else PROPERTY_NAMES
            )
            k = self.resolve_sampling_size(
                sampling_size=sampling_size, sampling_frame=sampling_frame
            )
            for p_pair in random.sample(
                list(itertools.combinations(sampling_frame, r=2)), k=k
            ):
                example = self.make_example(
                    property_names=p_pair,
                    tail_nums=dict(property_num=2),
                    query_path=query_path,
                    head_num=head_num,
                )
                if example is not None:
                    examples.append(example)

        elif tail_class == "identifier":
            if query_path == "t2h":
                raise ValueError(
                    "Since a species' identifier is used to detect the head entity, tail-to-head query is not applicable when the tail is an identifier."
                )
            k = self.resolve_sampling_size(
                sampling_size=sampling_size, sampling_frame=IDENTIFIER_NAMES
            )
            for i_pair in random.sample(
                list(itertools.combinations(IDENTIFIER_NAMES, r=2)), k=k
            ):
                example = self.make_example(
                    identifier_names=i_pair,
                    tail_nums=dict(identifier_num=2),
                    query_path="h2t",
                    head_num=head_num,
                )
                if example is not None:
                    examples.append(example)

        elif tail_class is None:
            for _ in range(sampling_size):
                try_num = 0
                example = None

                while example is None and try_num < 3:
                    tail_nums = get_tail_nums_total2(
                        identifier_num=0 if query_path == "t2h" else None,
                        chemicalclass_num=1 if query_path == "t2t" else None,
                    )
                    example = self.make_example(
                        tail_nums=tail_nums,
                        query_path=query_path,
                        uses=[] if query_path == "t2t" else None,
                        head_num=head_num,
                    )

                if example is not None:
                    examples.append(example)

        else:
            raise ValueError(
                f"Unexpected value for argument `tail_class`: {tail_class}."
            )

        return examples

    def make_examples_3t(
        self, query_path: str, sampling_size: int = 1, head_num: int = 1
    ):
        examples: List[dict] = []

        for _ in range(sampling_size):
            try_num = 0
            example = None

            while example is None and try_num < 3:
                tail_nums = get_tail_nums_total3(
                    identifier_num=0 if query_path == "t2h" else None,
                    chemicalclass_num=random.randint(1, 2)
                    if query_path == "t2t"
                    else None,
                )
                example = self.make_example(
                    tail_nums=tail_nums,
                    query_path=query_path,
                    uses=[] if query_path == "t2t" else None,
                    head_num=head_num,
                )
                try_num += 1

            if example is not None:
                examples.append(example)

        return examples

    def make_example(
        self,
        query_path: str,
        property_names: Optional[Iterable[str]] = None,
        identifier_names: Optional[Iterable[str]] = None,
        uses: Optional[Iterable[str]] = None,
        chemicalclasses: Optional[Iterable[str]] = None,
        tail_nums: Dict[str, int] = dict(),
        head_num: int = 1,
    ):
        property_sampling_frame = (
            self.PROPERTY_NAMES_FOR_T2H if query_path == "t2h" else PROPERTY_NAMES
        )
        if property_names is None:
            property_names = random.sample(
                property_sampling_frame, tail_nums.get("property_num", 0)
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

        if head_num not in [1, 2]:
            raise ValueError(f"Unexpected value for argument `head_num`: {head_num}.")

        def get_subgraph():
            return self.subgraph_retriever.get_subgraph(
                tail_nums=tail_nums,
                bindings={
                    **property_bindings,
                    **identifier_bindings,
                    **usevalue_bindings,
                    **chemicalclassvalue_bindings,
                },
            )

        if query_path == "h2t":
            if head_num == 1:
                subgraphs = [get_subgraph()]
            else:
                subgraphs = [get_subgraph() for _ in range(head_num)]
                if subgraphs[0] is not None and subgraphs[1] is not None:
                    try_num = 0
                    subgraph = subgraphs[1]

                    while (
                        try_num < 3
                        and subgraph["head"]["SpeciesIRI"]
                        == subgraphs[0]["head"]["SpeciesIRI"]
                    ):
                        subgraph = get_subgraph()
                        if subgraph is None:
                            return None
                        try_num += 1

                    if (
                        subgraph["head"]["SpeciesIRI"]
                        == subgraphs[0]["head"]["SpeciesIRI"]
                    ):
                        return None

                    subgraphs[1] = subgraph
            example = self.example_maker_head2tail.make_example(subgraphs=subgraphs)

        elif query_path == "t2h":
            example = self.example_maker_tail2head.make_example(subgraph=get_subgraph())

        elif query_path == "t2t":
            example = self.example_maker_tail2tail.make_example(subgraph=get_subgraph())

        else:
            raise ValueError(f"Unexpected value for `query_path`: {query_path}.")

        if example is not None:
            example["query_path"] = query_path

        return example

    def resolve_sampling_size(self, sampling_size: int, sampling_frame: list):
        if sampling_size < 0:
            return len(sampling_frame)
        return sampling_size
