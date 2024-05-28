from abc import ABC, abstractmethod
from typing import Dict, List, Optional


class IEntityLinker(ABC):
    @abstractmethod
    def link(self, text: Optional[str], **kwargs) -> List[str]:
        pass
