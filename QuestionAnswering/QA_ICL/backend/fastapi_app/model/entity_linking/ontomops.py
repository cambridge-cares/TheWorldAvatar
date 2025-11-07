from pydantic import BaseModel, model_validator


class MOPLinkingArgs(BaseModel):
    formula: str


class CBULinkingArgs(BaseModel):
    formula: str


class GBUTypeLinkingArgs(BaseModel):
    modularity: int | None = None
    planarity: str | None = None

    @model_validator(mode="after")
    def validate_modularity_or_planarity(self):
        if self.modularity is None and self.planarity is None:
            raise ValueError("At least one of 'modularity' or 'planarity' must be provided.")
        return self

class GBUTypeWithNumLinkingArgs(GBUTypeLinkingArgs):
    num: int


class AMLinkingArgs(BaseModel):
    GBUType: list[GBUTypeWithNumLinkingArgs] = list()
    symmetry_point_group: str | None = None
