from chemaboxwriters.common.utilsfunc import getRefName, stage_name_to_enum
from chemaboxwriters.app_exceptions.app_exceptions import (
    UnsupportedStage,
    IncorrectHandlerParameter,
)
from pyuploader import get_uploader
from enum import Enum
from typing import List, Dict, Tuple, Protocol
import logging

logger = logging.getLogger(__name__)


class IHandle_Function(Protocol):
    @staticmethod
    def __call__(inputs: List[str], *args, **kwargs) -> Dict[str, List[str]]:
        ...


class Handler:
    def __init__(
        self,
        name: str,
        handle_function: IHandle_Function,
        in_stages: List[Enum],
        out_stage: Enum,
    ) -> None:

        self.name = name
        self.handle_function = handle_function
        self.in_stages = in_stages
        self.out_stage = out_stage

    def run(
        self,
        inputs: List[str],
        input_type: Enum,
        out_dir: str,
        dry_run: bool,
        **handler_kwargs,
    ) -> Tuple[List[str], Enum]:

        if input_type not in self.in_stages:
            requestedStage = input_type.name.lower()
            raise UnsupportedStage(
                f"Error: Stage: '{requestedStage}' is not supported."
            )

        outputs = self.handle_function(
            inputs=inputs,
            out_file_ext=self.out_stage.name,
            out_dir=out_dir,
            **handler_kwargs,
        )

        self._save_outputs(outputs=outputs, out_dir=out_dir)

        return outputs, self.out_stage

    def _save_outputs(self, outputs: List[str], out_dir: str) -> List[str]:


