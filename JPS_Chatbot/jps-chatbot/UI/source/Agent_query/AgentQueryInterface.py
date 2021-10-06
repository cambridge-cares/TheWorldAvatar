import json
from json import JSONDecodeError
from rasa.nlu.model import Interpreter
from AgentRequestConstructor import AgentRequestConstructor


class AgentQueryInterface:
    def __init__(self, model):
        self.agent_interpreter = Interpreter.load(model)
        self.agent_request_constructor = AgentRequestConstructor()

    def agent_query(self, question):
        rst = self.agent_interpreter.parse(question)
        with open('agent_query_log') as f:
            try:
                agent_query_log = json.loads(f.read())
                f.close()
            except JSONDecodeError:
                agent_query_log = {}

        agent_query_log[question.strip()] = rst
        with open('agent_query_log', 'w') as f:
            f.write(json.dumps(agent_query_log))
            f.close()
        try:
            response = self.agent_request_constructor.call_agent(rst)
            return response
        except:
            return None
