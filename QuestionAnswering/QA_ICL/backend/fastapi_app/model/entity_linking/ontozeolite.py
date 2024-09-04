from pydantic import BaseModel


class ZeoliteFrameworkLinkingArgs(BaseModel):
    code: str


class ZeoliticMaterialLinkingArgs(BaseModel):
    formula: str
