from abc import ABC, abstractmethod
from typing import Any, Protocol


class LinkEntity(Protocol):
    def __call__(self, text: str | None, **kwargs) -> list[str]: ...


class LinkerManager(ABC):
    @property
    @abstractmethod
    def cls2linker(self) -> dict[str, LinkEntity]: ...
