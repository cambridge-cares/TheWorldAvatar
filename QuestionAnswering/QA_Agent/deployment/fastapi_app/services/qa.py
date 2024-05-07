from typing import Annotated

from fastapi import Depends

from services.nlq2action import get_singapore_nlq2action2data
from services.support_data import DataSupporter


def get_dataSupporter_byDomain(
    singapore_nlq2action: Annotated[
        DataSupporter, Depends(get_singapore_nlq2action2data)
    ],
):
    return {"singapore": singapore_nlq2action}
