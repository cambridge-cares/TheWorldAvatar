from typing import Optional
from pydantic import BaseModel


QTRECOG_EXAMPLES_KEY_PREFIX = "qtRecogExamples:"


class QtAnnotation(BaseModel):
    key: str
    type: str
    value: float
    unit: Optional[str] = None


class QtRecogPred(BaseModel):
    template: str
    quantities: list[QtAnnotation]


class QtRecogExample(BaseModel):
    text: str
    prediction: QtRecogPred
