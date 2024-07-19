from pydantic import BaseModel

from model.web.comp_op import ComparisonOperator
from model.kg.ontospecies import SpeciesIdentifierKey, SpeciesPropertyKey


class SpeciesReqReturnFields(BaseModel):
    alt_label: bool = False
    chemical_class: bool = False
    use: bool = False
    identifier: list[SpeciesIdentifierKey] = list()
    property: list[SpeciesPropertyKey] = list()


class SpeciesRequest(BaseModel):
    chemical_class: list[str]
    use: list[str]
    identifier: dict[SpeciesIdentifierKey, str]
    property: dict[SpeciesPropertyKey, list[tuple[ComparisonOperator, float]]]
    return_fields: SpeciesReqReturnFields | None
