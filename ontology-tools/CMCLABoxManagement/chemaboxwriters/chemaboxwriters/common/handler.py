from enum import Enum
from abc import ABC, abstractmethod
from typing import List, Tuple
from dataclasses import dataclass, field
import logging

logger = logging.getLogger(__name__)


@dataclass
class IHandler(ABC):
    """
    The Handler interface provides methods to handle file inputs.
    """

    name: str
    in_stages: List[Enum]
    out_stage: Enum
    written_files: List[str] = field(init=False, default_factory=list)

    def notify(
        self,
        inputs: List[str],
        input_type: Enum,
        out_dir: str,
        dry_run: bool,
        **handler_kwargs,
    ) -> Tuple[List[str], Enum]:

        outputs = self._handle_input(
            inputs=inputs,
            out_dir=out_dir,
            input_type=input_type,
            dry_run=dry_run,
            **handler_kwargs,
        )
        self.written_files.extend(outputs)

        return outputs, self.out_stage

    def info(self) -> None:
        logger.info(f"handler: {self.name}")
        logger.info(f"in_stages: {self.in_stages}")
        logger.info(f"out_stage: {self.out_stage}")

    @abstractmethod
    def _handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        dry_run: bool,
        input_type: Enum,
        **handler_kwargs,
    ) -> List[str]:
        pass
