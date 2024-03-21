from typing import Annotated

from fastapi import Depends

from services.core.parse import KeyAggregateParser, SchemaParser, get_schema_parser
from .model import FactoryAttrKey


def get_factory_attr_agg_parser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return KeyAggregateParser(schema_parser=schema_parser, enum_cls=FactoryAttrKey)
