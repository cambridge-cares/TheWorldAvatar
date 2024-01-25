import dataclasses
from decimal import Decimal
import json

from constants.functions import AggOp, NumOp, StrOp
from constants.ontobuiltenv import OBEAttrKey
from constants.ontospecies import OSIdentifierKey, OSPropertyKey, OSSpeciesAttrKey


PUBLIC_ENUMS = {
    "StrOp": StrOp,
    "NumOp": NumOp,
    "AggOp": AggOp,
    "OSSpeciesAttrKey": OSSpeciesAttrKey,
    "OSPropertyKey": OSPropertyKey,
    "OSIdentifierKey": OSIdentifierKey,
    "OBEAttrKey": OBEAttrKey,
}


class EnumEncoder(json.JSONEncoder):
    def default(self, obj):
        if dataclasses.is_dataclass(obj):
            return dataclasses.asdict(obj)
        if isinstance(obj, Decimal):
            return float(obj)
        if type(obj) in PUBLIC_ENUMS.values():
            return {"__enum__": str(obj)}
        return super().default(self, obj)


def as_enum(d):
    if "__enum__" in d:
        name, member = d["__enum__"].split(".")
        return getattr(PUBLIC_ENUMS[name], member)
    else:
        return d
