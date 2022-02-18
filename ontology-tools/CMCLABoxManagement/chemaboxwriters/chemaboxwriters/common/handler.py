from chemaboxwriters.app_exceptions.app_exceptions import UnsupportedStage
from enum import Enum
from abc import ABC, abstractmethod
from typing import List, Tuple
from dataclasses import dataclass, field
import logging

logger = logging.getLogger(__name__)


@dataclass
class IHandler(ABC):
    name: str
    in_stages: List[Enum]
    out_stage: Enum
    written_files: List[str] = field(init=False, default_factory=list)

    def execute(
        self,
        inputs: List[str],
        input_type: Enum,
        out_dir: str,
        dry_run: bool,
        **handler_kwargs,
    ) -> Tuple[List[str], Enum]:

        if input_type not in self.in_stages:
            requested_stage = input_type.name.lower()
            raise UnsupportedStage(
                f"Error: Stage: '{requested_stage}' is not supported."
            )

        outputs = self.handle_input(
            inputs=inputs,
            out_dir=out_dir,
            input_type=input_type,
            dry_run=dry_run,
            **handler_kwargs,
        )
        self.written_files.extend(outputs)

        return outputs, self.out_stage

    @abstractmethod
    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        dry_run: bool,
        input_type: Enum,
        **handler_kwargs,
    ) -> List[str]:
        pass
