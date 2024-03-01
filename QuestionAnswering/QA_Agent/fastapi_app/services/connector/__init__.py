import json
import logging

from services.openai_client import openai_client
from services.connector.ontospecies import OntoSpeciesAgent

logger = logging.getLogger(__name__)

class AgentConnector:
    def __init__(self, model: str = "gpt-3.5-turbo-0125"):
        self.model = model
        agents = [OntoSpeciesAgent()]
        self.tools = [x for agent in agents for x in agent.get_tools()]
        self.funcname2agent = {
            method_name: agent
            for agent in agents
            for method_name in agent.get_name2method().keys()
        }

    def query(self, query: str):
        logger.info("Predicting function to call...")
        response = openai_client.chat.completions.create(
            model=self.model,
            messages=[{"role": "user", "content": query}],
            tools=self.tools,
            tool_choice="auto",
        )
        func = response.choices[0].message.tool_calls[0].function
        logger.info("Predicted function: " + str(func))
        return self.funcname2agent[func.name].exec(
            method_name=func.name, args=json.loads(func.arguments)
        )
