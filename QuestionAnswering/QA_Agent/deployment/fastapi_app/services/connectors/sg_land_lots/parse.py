from typing import Annotated

from fastapi import Depends

from core.parse import (
    KeyAggregateParser,
    SchemaParser,
    get_schema_parser,
)
from .model import PlotNumAttrKey


def get_plotAttr_aggParser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return KeyAggregateParser(schema_parser=schema_parser, enum_cls=PlotNumAttrKey)
