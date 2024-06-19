from abc import ABC, abstractmethod


class IEntityLinker(ABC):
    @abstractmethod
    def link(self, text: str | None, **kwargs) -> list[str]: ...
