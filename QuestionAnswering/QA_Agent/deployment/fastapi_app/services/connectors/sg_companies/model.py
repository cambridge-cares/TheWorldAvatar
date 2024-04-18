from enum import Enum
from typing import List


class CompanyAttrKey(Enum):
    factories: List[str]  # ontocompany:isOwnerOf
    data_centres: List[str]  # ontocompany:hasDataCentre
    business_activity: str
