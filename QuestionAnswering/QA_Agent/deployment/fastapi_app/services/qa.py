from typing import Annotated

from fastapi import Depends

from services.connectors import get_chemistry_agentMediator, get_singapore_agentMediator
from services.nlq2action import get_singapore_nlq2action2data
from services.support_data import DataSupporter


def get_dataSupporter_byDomain(
    chemistry_agent_mediator: Annotated[
        DataSupporter, Depends(get_chemistry_agentMediator)
    ],
    singapore_agent_mediator: Annotated[
        DataSupporter, Depends(get_singapore_agentMediator)
    ],
    # singapore_nlq2action: Annotated[
    #     DataSupporter, Depends(get_singapore_nlq2action2data)
    # ],
):
    return {
        "chemistry": chemistry_agent_mediator,
        "singapore": singapore_agent_mediator,
        # "singapore": singapore_nlq2action
    }
