import os
from pprint import pprint

from util.ModelLoader import AGENT_NLU_MODEL, WIKI_NLU_MODEL, JPS_NLU_MODEL
from util.MarieLogger import MarieLog, MarieIOLog
from util.StopWords import removeStopWords
from util.ScoreManager import getHighestIntentScore
from Wikidata_Query.WikiQueryInterface import WikiQueryInterface
from JPS_Query.JPSQueryInterface import JPSQueryInterface
from LDA.LDA_classifier import LDAClassifier
from multiprocessing import Process


class CoordinateAgent:
    def __init__(self):
        self.agent_nlu_model = AGENT_NLU_MODEL
        self.wiki_nlu_model = WIKI_NLU_MODEL
        self.jps_nlu_model = JPS_NLU_MODEL
        self.lda_classifier = LDAClassifier()
        self.topic_model_map = {'wiki': self.wiki_nlu_model,
                                'ontocompchem': self.jps_nlu_model,
                                'ontokin': self.jps_nlu_model,
                                'agent': self.agent_nlu_model}

    # @MarieLog
    def run(self, _question):
        # Identify the topics
        topics = self.identify_topic(_question)
        topics.append('agent')
        final_result = None
        for topic in topics:
            # parse_result = self.parse_question(topic, _question)
            if topic == 'wiki':
                _question = removeStopWords(_question)
                rst = self.wiki_query(self.topic_model_map[topic], _question)
            elif topic in ['ontokin', 'ontocompchem']:
                rst = self.jps_query(_question)

            if rst is None:
                pass
            else:
                return rst

    @MarieIOLog
    def identify_topic(self, _question):
        return self.lda_classifier.classify(_question)

    # @MarieIOLog
    # def parse_question(self, topic, _question):
    #     return self.topic_model_map[topic].parse(_question)

    def wiki_query(self, model, question):
        return WikiQueryInterface(model).wiki_query(question)

    @MarieIOLog
    def jps_query(self, question):
        return JPSQueryInterface().jps_query(question)


if __name__ == '__main__':
    ca = CoordinateAgent()
    # TODO: more fine-grained tests
    # TODO: remove v2 from SPARQLs
    thermo_agent_questions = [
        'what is benzene',
        'what is the gibbs energy of H2O2',
        'what is the inner energy of methane at 100 K',
        'plot the heat capacity of CO2',
        'heat capacity of c1cccc1 at 1 atm',
        'heat capacity of c1cccc1 at room temperature',
        'find the entropy of benzene at 100000 Pa',
        'what is the enthalpy of InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3 at 1522.1 Pa and 123245 K']
    # for t_a_q in thermo_agent_questions[:-1]:
    #     ca.run(t_a_q)

    # wiki_questions = ['geometry c=c=c=c', 'heat capacity of methane', 'mass of benzene', 'molecular weight of ch4',
    #                   'chemical structure of c6h6']
    #
    # for wiki_q in wiki_questions:
    #     answer = ca.run(wiki_q)

    jps_questions = ['show me the vibration frequency of H2O2', 'symmetry number of c9h14']
    for jps_q in jps_questions:
        answer = ca.run(jps_q)
