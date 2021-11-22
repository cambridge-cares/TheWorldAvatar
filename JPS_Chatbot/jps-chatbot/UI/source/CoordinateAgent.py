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

        # initialise the interfaces, provide the according models
        # self.topic_model_map = {'wiki': self.wiki_nlu_model,
        #                         'ontocompchem': self.jps_nlu_model,
        #                         'ontokin': self.jps_nlu_model,
        #                         'agent': self.agent_nlu_model}
        #
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
    # for i in range(0,2):
    #     ca.run('aromatic hydrocarbons with mass less than 170')
    # rst = ca.run('what is the pce of CC')
    # print(rst)
    # for i in range(0, 2):
    #     with open('test_questions') as f:
    #         questions = f.readlines()
    #         random.shuffle(questions)
    #         for q in questions:
    #             time.sleep(10)
    #             rst = ca.run(q)

    # TODO: more fine-grained tests
    # TODO: remove v2 from SPARQLs
    thermo_agent_questions = [
        'what is the molecular weight of C=C=C=C',
        'geometry of C=C',
        'what is benzene',
        'what is the gibbs energy of H2O2',
        'what is the internal energy of methane at 100 K',
        'plot the heat capacity of CO2',
        'heat capacity of c1cccc1 at 1 atm',
        'heat capacity of c1cccc1 at room temperature',
        'heat capacity of c1cccc1',
        'heat capacity of co2 at room temperature',
        'heat capacity of carbon dioxide at room temperature',
        'heat capacity at constant pressure of carbon dioxide at room temperature',
        'find the entropy of benzene at 100000 Pa',
        'what is the enthalpy of InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3 at 1522.1 Pa and 123245 K']
    for t_a_q in thermo_agent_questions:
        print('\n===========================================')
        print(t_a_q)
        print('===========================================\n')
        rst = ca.run(t_a_q)
        print('##############################################')
        print(json.dumps(rst, indent=4))
        print('##############################################')


    # ca.run('what is the enthalpy of InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3 at 1522.1 Pa and 123245 K')

    # wiki_questions = ['geometry c=c=c=c', 'heat capacity of methane', 'mass of benzene', 'molecular weight of ch4',
    #                   'chemical structure of c6h6']
    #
    # for wiki_q in wiki_questions:
    #     answer = ca.run(wiki_q)
    # ca.run('geometry c=c=c=c')
    # ca.run('show me the pce of c=c=c=c')
    # ca.run('Chemical structure of aromatic hydrocarbons')
    # failed_questions = ['heat capacity of c1cccc1 at room temperature', ' find the entropy of benzene at 100000 Pa ']
    # for f_q in failed_questions:
    #     ca.run(f_q)
