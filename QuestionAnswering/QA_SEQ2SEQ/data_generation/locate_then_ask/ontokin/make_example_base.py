from abc import ABC

from locate_then_ask.data_model import AskDatum


class ExampleMakerBase(ABC):
    @classmethod
    def make_example(self, entity_iri: str) -> AskDatum:
        pass