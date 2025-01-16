from pydantic import BaseModel


class SpeciesLinkingArgs(BaseModel):
    inchi: str | None = None
    smiles: str | None = None
    iupac_name: str | None = None


class ElementLinkingArgs(BaseModel):
    symbol: str
