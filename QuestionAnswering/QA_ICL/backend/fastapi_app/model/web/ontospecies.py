from pydantic import BaseModel

from model.web.comp_op import ComparisonOperator
from model.kg.ontospecies import SpeciesIdentifierKey, SpeciesPropertyKey


class SpeciesReturnFields(BaseModel):
    alt_label: bool
    chemical_class: bool
    use: bool
    identifier: list[SpeciesIdentifierKey]
    property: list[SpeciesPropertyKey]


class SpeciesRequest(BaseModel):
    chemical_class: list[str]
    use: list[str]
    identifier: dict[SpeciesIdentifierKey, str]
    property: dict[SpeciesPropertyKey, list[tuple[ComparisonOperator, float]]]
