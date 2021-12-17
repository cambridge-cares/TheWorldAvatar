import json
import random
import time
from pprint import pprint

from util.ModelLoader import AGENT_NLU_MODEL, WIKI_NLU_MODEL, JPS_NLU_MODEL
from util.StopWords import removeStopWords
from Wikidata_Query.WikiQueryInterface import WikiQueryInterface
from JPS_Query.JPSQueryInterface import JPSQueryInterface
from Agent_Query.AgentQueryInterface import AgentQueryInterface
from LDA.LDA_classifier import LDAClassifier
from Agent_Query.AgentUtil.util.MarieLogger import MarieIOLog, MarieMessage, MarieError

class CoordinateAgent:
    def __init__(self):
        # self.agent_nlu_model = AGENT_NLU_MODEL
        # self.wiki_nlu_model = WIKI_NLU_MODEL
        # self.jps_nlu_model = JPS_NLU_MODEL
        self.lda_classifier = LDAClassifier()
        self.jps_interface = JPSQueryInterface(JPS_NLU_MODEL())
        self.wiki_interface = WikiQueryInterface(WIKI_NLU_MODEL())
        self.agent_interface = AgentQueryInterface(AGENT_NLU_MODEL())
        self.topic_function_map = {'wiki': self.wiki_query,
                                   'ontocompchem': self.jps_query,
                                   'ontokin': self.jps_query,
                                   'agent': self.agent_query}

    # @MarieLog
    def run(self, _question):
        if _question.strip() == '':
            # you kidding
            return None
        MarieMessage('\n\n ============================\n '
                     'Processing Question \n {} \n '
                     '=============================== \n'.format(_question))
        # Identify the topics
        topics = self.identify_topic(_question)
        topics = ['agent'] + topics

        for topic in topics:
            rst = self.topic_function_map[topic](_question)
            if rst is None:
                pass
            else:
                try:
                    rst = json.loads(rst)
                except:
                    rst = rst
                MarieMessage('\n ================================================  '
                             'End of question, with answer from {} '
                             '=================================================\n\n'.format(topic))
                return rst

        MarieError('============================  End of question, no answer =============================\n\n {}'
                   .format(_question))
        return None

    @MarieIOLog
    def identify_topic(self, _question):
        return self.lda_classifier.classify(_question)

    @MarieIOLog
    def wiki_query(self, question):
        question = removeStopWords(question)
        return self.wiki_interface.wiki_query(question)

    @MarieIOLog
    def agent_query(self, question):
        return self.agent_interface.agent_query(question)

    @MarieIOLog
    def jps_query(self, question):
        return self.jps_interface.jps_query(question)


if __name__ == '__main__':
    ca = CoordinateAgent()
    ca.run('what is the heat capacity of co2')