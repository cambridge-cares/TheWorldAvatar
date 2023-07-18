import json
import random
import time
from pprint import pprint
if __name__ == '__main__':
    from util.ModelLoader import AGENT_NLU_MODEL, WIKI_NLU_MODEL, JPS_NLU_MODEL
    from util.StopWords import removeStopWords
    from Wikidata_Query.WikiQueryInterface import WikiQueryInterface
    from JPS_Query.JPSQueryInterface import JPSQueryInterface
    from Agent_Query.AgentQueryInterface import AgentQueryInterface
    from LDA.LDA_classifier import LDAClassifier
    from Agent_Query.AgentUtil.util.MarieLogger import MarieIOLog, MarieMessage, MarieError
else:
    from .util.ModelLoader import AGENT_NLU_MODEL, WIKI_NLU_MODEL, JPS_NLU_MODEL
    from .util.StopWords import removeStopWords
    from .Wikidata_Query.WikiQueryInterface import WikiQueryInterface
    from .JPS_Query.JPSQueryInterface import JPSQueryInterface
    from .Agent_Query.AgentQueryInterface import AgentQueryInterface
    from .LDA.LDA_classifier import LDAClassifier
    from .Agent_Query.AgentUtil.util.MarieLogger import MarieIOLog, MarieMessage, MarieError


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

    question_list = ['What is enthalpy of C3H5N3O at the temperature of 294.62 degree Celsius?',
                     'What is the power conversion efficiency of OPF with donor of styryltrimethylsilane?',
                     'What is C6H7NSe’s enthalpy at 181.09 Fahrenheit?',
                     'What is C2H3IO’s entropy at 230.84 kelvin and 1.01325 bar?',
                     'What is CH3’s heat capacity at 61.11 degrees in temperature?',
                     'What is heat capacity at constant pressure of C6H11O3 at room temperature?',
                     'What is internal energy of NH4OH at -95 F?',
                     'What is heat capacity of InChI=1/C7H5N/c8-6-7-4-2-1-3-5-7/h1-5H under 30 C?',
                     'What is COC1CC1=C(C)C’s entropy at 162 Fahrenheit?',
                     'What is pce of InChI=1/C7H12/c1-4-5-6-7(2)3/h1?',
                     'What is pce of OPF with donor of C=CC(C)=O?',
                     'What is power conversion efficiency of C2H6B4?',
                     'What is power conversion efficiency of CH3COCHO?',
                     'What is pce of OPF with donor of (CH3)3C-CN?',
                     'What is power conversion efficiency of nicaethan?',
                     'Show me CC1=C(C)CCC1’s heat capacity at constant pressure at 150 kelvin',
                     'What is the heat capacity of CO2'
                     ]

    question_list = ['What is internal energy of NH4OH at -95 F?']

    ca = CoordinateAgent()
    for q in question_list:
        rst = ca.run(q)
        pprint(rst)
        x = input('waiting for your command sir')