from pydantic import BaseModel

from model.comp_op import ComparisonOperator
from model.kg.ontospecies import SpeciesPropertyKey


class SpeciesRequest(BaseModel):
    chemical_class: list[str]
    use: list[str]
    property: dict[SpeciesPropertyKey, list[tuple[ComparisonOperator, float]]]
