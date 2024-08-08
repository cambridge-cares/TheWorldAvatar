from pydantic import BaseModel


class MOPLinkingArgs(BaseModel):
    formula: str


class CBULinkingArgs(BaseModel):
    formula: str


class GBULinkingArgs(BaseModel):
    modularity: int
    planarity: str


class GBUWithNumLinkingArgs(GBULinkingArgs):
    num: int


class AMLinkingArgs(BaseModel):
    GBU: list[GBUWithNumLinkingArgs] = list()
    symmetry_point_group: str | None = None
