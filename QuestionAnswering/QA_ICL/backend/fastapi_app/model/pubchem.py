from pydantic import BaseModel


class PCCompoundTypeId(BaseModel):
    cid: int

class PCCompoundId(BaseModel):
    id: PCCompoundTypeId

class PubChemPUGAtoms(BaseModel):
    aid: list[int]
    element: list[int]

class PubChemPUGConformer(BaseModel):
    x: list[float]
    y: list[float]
    z: list[float]

class PubChemPUGCoord(BaseModel):
    conformers: list[PubChemPUGConformer]

class PCCompound(BaseModel):
    id: PCCompoundId
    atoms: PubChemPUGAtoms
    coords: list[PubChemPUGCoord]

class PubChemPUGResponse(BaseModel):
    PC_Compounds: list[PCCompound]