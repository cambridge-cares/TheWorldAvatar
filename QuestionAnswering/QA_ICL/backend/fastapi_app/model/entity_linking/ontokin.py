from pydantic import BaseModel


class MechanismLinkingArgs(BaseModel):
    provenance: str


class ReactionLikingArgs(BaseModel):
    equation: str
