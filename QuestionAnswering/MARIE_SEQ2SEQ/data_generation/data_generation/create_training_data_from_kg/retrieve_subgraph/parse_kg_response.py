from typing import Dict, List
from abc import ABC, abstractmethod


class KgResponseParser:
    def __init__(self):
        self.head_parser = HeadParser()
        self.tail_parsers: List[TailParser] = [
            TailParserProperty(),
            TailParserIdentifier(),
            TailParserUse(),
            TailParserChemicalClass(),
        ]

    def to_subgraph(self, valueset: Dict[str, Dict[str, str]]):
        values = {k: v["value"] for k, v in valueset.items()}
        return dict(
            head=self.head_parser.parse(values),
            tails=[
                tail
                for tail_parser in self.tail_parsers
                for tail in tail_parser.parse(values)
            ],
        )


class HeadParser:
    def parse(self, values: Dict[str, str]):
        return dict(
            SpeciesIRI=values["SpeciesIRI"],
            IdentifierIRI=values["IdentifierIRI"],
            IdentifierValue=values["IdentifierValue"],
        )


class TailParser(ABC):
    def __init__(self, variable_prefix: str):
        self.variable_prefix = variable_prefix

    def parse(self, values: Dict[str, str]):
        results = []
        for i in range(1, 4):
            if f"{self.variable_prefix}IRI{i}" not in values:
                break
            results.append(self.parse_per_tail(i, values))
        return results

    @abstractmethod
    def parse_per_tail(self, tail_id: int, values: Dict[str, str]) -> Dict[str, str]:
        pass


class TailParserProperty(TailParser):
    def __init__(self):
        super().__init__("Property")

    def parse_per_tail(self, i: int, values: Dict[str, str]):
        return dict(
            type="property",
            PropertyName=values[f"hasProperty{i}"].split("#", maxsplit=1)[-1][
                len("has") :
            ],
            PropertyIRI=values[f"PropertyIRI{i}"],
            PropertyValue=values[f"PropertyValue{i}"],
        )


class TailParserIdentifier(TailParser):
    def __init__(self):
        super().__init__("Identifier")

    def parse_per_tail(self, i: int, values: Dict[str, str]):
        return dict(
            type="identifier",
            IdentifierName=values[f"hasIdentifier{i}"].split("#", maxsplit=1)[-1][
                len("has") :
            ],
            IdentifierIRI=values[f"IdentifierIRI{i}"],
            IdentifierValue=values[f"IdentifierValue{i}"],
        )


class TailParserUse(TailParser):
    def __init__(self):
        super().__init__("Use")

    def parse_per_tail(self, i: int, values: Dict[str, str]):
        return dict(
            type="use",
            UseIRI=values[f"UseIRI{i}"],
            UseValue=values[f"UseValue{i}"],
        )


class TailParserChemicalClass(TailParser):
    def __init__(self):
        super().__init__("ChemicalClass")

    def parse_per_tail(self, i: int, values: Dict[str, str]):
        return dict(
            type="chemicalclass",
            ChemicalClassIRI=values[f"ChemicalClassIRI{i}"],
            ChemicalClassValue=values[f"ChemicalClassValue{i}"],
        )
