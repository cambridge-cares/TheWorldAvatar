from enum import Enum
from typing import List


class CompanyAttrKey(Enum):
    factories: List[str]  # ontocompany:isOwnerOf
    business_activity: str
