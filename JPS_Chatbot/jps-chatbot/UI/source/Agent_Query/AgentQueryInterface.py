from pprint import pprint
from .AgentRequestConstructor import AgentRequestConstructor
from util.MarieLogger import MarieLog, MarieIOLog, MarieQuestionLog
from util.StopWords import removeStopWords


class AgentQueryInterface:
    def __init__(self, model):
        self.agent_interpreter = model
        self.agent_request_constructor = AgentRequestConstructor()

    @MarieIOLog
    def agent_query(self, question):
        rst = self.parse_question(question)
        try:
            response = self.agent_request_constructor.call_agent(rst)
            return response
        except:
            return None

    def parse_question(self, question):
        question = removeStopWords(question)
        return self.agent_interpreter.parse(question)


if __name__ == '__main__':
    aqi = AgentQueryInterface()
