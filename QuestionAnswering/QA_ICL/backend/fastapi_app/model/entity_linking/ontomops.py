from pydantic import BaseModel


class MOPLinkingArgs(BaseModel):
    formula: str


class CBULinkingArgs(BaseModel):
    formula: str


class GBUTypeLinkingArgs(BaseModel):
    modularity: int | None = None
    planarity: str

class GBUTypeWithNumLinkingArgs(GBUTypeLinkingArgs):
    num: int


class AMLinkingArgs(BaseModel):
    GBUType: list[GBUTypeWithNumLinkingArgs] = list()
    symmetry_point_group: str | None = None
