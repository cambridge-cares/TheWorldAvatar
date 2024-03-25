from functools import cache, cached_property
import time
from typing import Annotated, List

from fastapi import Depends

from model.qa import QAStep
from services.connectors.retrieval.agent import RetrievalAgent, get_retrieval_agent
from services.connectors.agent_connector import AgentConnectorBase


class RetrievalAgentConnector(AgentConnectorBase):
    def __init__(self, agent: RetrievalAgent):
        self.agent = agent

    @cached_property
    def funcs(self):
        return [
            {
                "name": "semantic_search",
                "description": "Perform semantic search for queries related to concepts, terminologies, enitty type or class relationships",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "query": {
                            "type": "string",
                            "description": "The input questiion",
                        }
                    },
                },
                "required": ["query"],
            }
        ]

    @cached_property
    def name2method(self):
        return {"semantic_search": self.retrieve_docs}

    def retrieve_docs(self, query: str):
        steps: List[QAStep] = []

        timestamp = time.time()
        data = self.agent.retrieve(query)
        latency = time.time() - timestamp
        steps.append(QAStep(action="retrieve_docs", arguments=query, latency=latency))

        return steps, data


@cache
def get_retrieval_agentConnector(
    agent: Annotated[RetrievalAgent, Depends(get_retrieval_agent)]
):
    return RetrievalAgentConnector(agent)
