from abc import ABC


class ExampleMakerBase(ABC):
    @classmethod
    def make_example(self, entity_iri: str):
        pass