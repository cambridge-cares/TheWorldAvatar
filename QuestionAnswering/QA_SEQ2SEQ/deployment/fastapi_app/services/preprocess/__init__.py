from abc import ABC
from dataclasses import dataclass


@dataclass
class PreprocessedText:
    for_user: str
    for_trans: str


class Preprocessor(ABC):
    def preprocess(self, text: str) -> PreprocessedText:
        pass
