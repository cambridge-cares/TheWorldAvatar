from abc import ABC, abstractmethod


class ExampleMakerBase(ABC):
    @abstractmethod
    def make_example(self, entity_iri: str):
        pass