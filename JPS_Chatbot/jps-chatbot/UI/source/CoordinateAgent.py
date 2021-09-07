import json
from pprint import pprint

from Wikidata_Query.Interpretation_parser import InterpretationParser
from Wikidata_Query.SearchEngine import SearchEngine
from Wikidata_Query.SPARQLConstructor import SPARQLConstructor
from Wikidata_Query.SPARQLQuery import SPARQLQuery
from LDA.LDA_classifier import LDAClassifier
from JPS_Query.chatbot_interface import Chatbot
from dashboard.LogWriter import LogWriter
from dashboard.Messenger import Messenger
from Agent_query.AgentRequestConstructor import AgentRequestConstructor

from rasa.nlu.model import Interpreter
import os
import tarfile

from location import WIKI_MODELS_DIR, AGENT_MODELS_DIR


# 0. get the topic model result, choose which direction it goes
# 1. get the InterpretationParse

def extract_nlu_model(extract_dir='../models/'):
    # Identify the newest trained nlu model
    # Disable the function when deployed to production server ... 
    path = 'Wikidata_Query/models/'
    files = os.listdir(path)
    paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
    file_name = max(paths, key=os.path.getctime)
    # Extract the model to a temporary directory
    tf = tarfile.open(file_name)
    tf.extractall(path=extract_dir)


class CoordinateAgent:
    def __init__(self, socketio):
        # initialize interpreter
        # extract_nlu_model()
        self.search_engine = SearchEngine()
        self.stopwords = ['all', 'the']
        self.nlu_model_directory = os.path.join(WIKI_MODELS_DIR, 'nlu')
        self.interpreter = Interpreter.load(self.nlu_model_directory)  # load the wiki nlu models

        self.agent_nlu_model_directory = os.path.join(AGENT_MODELS_DIR, 'nlu')
        self.agent_interpreter = Interpreter.load(self.agent_nlu_model_directory)
        self.agent_request_constructor = AgentRequestConstructor()

        self.jps_interface = Chatbot(socketio)
        self.socket = socketio
        self.logwriter = LogWriter()
        self.msg = Messenger()

    # def return_for_more(self, agent_id):
    #     pass
    #
    def agent_query(self, question):
        rst = self.agent_interpreter.parse(question)
        print('========================= agent query =====================')
        pprint(rst)
        response = self.agent_request_constructor.call_agent(rst)

        # the result will give
        #  - the name of the agent
        #  - the entities
        # TODO: Talk to Daniel about the extra parameters
        # 2. check the requirement of the agent ... any other parameters ?
        # 3.

        print('=================== result returned ===================')
        pprint(response)

    def remove_stop_words(self, question):
        stopwords = ['the', 'an', 'a', 'is', 'what', 'are', 'of', 'describe', 'find', 'find me']
        question = question.strip()
        question = question.lower()
        words = question.split(' ')
        words = [w for w in words if w not in stopwords]
        return ' '.join(words)

    def question_classification(self, question):
        intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
        return intent_and_entities['intent']

    def named_entity_recognition(self, question):
        intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
        return intent_and_entities['entities']

    # @lru_cache(maxsize=None)
    def run(self, question):

        # TODO: put the LDA model here
        # ===================== initialize the things for wiki
        self.interpreter_parser = InterpretationParser(self.socket)
        self.interpreter_parser.interpreter = self.interpreter
        print('Loading interpreter')

        self.sparql_constructor = SPARQLConstructor()
        self.sparql_query = SPARQLQuery(self.socket)
        print('SPARQL Query init')
        self.lda_classifier = LDAClassifier()
        print('LDA init')
        topics = self.lda_classifier.classify(question)
        self.logwriter.write_to_log(question, 'Topics identified %s \n' % json.dumps(topics))
        topics.append('others')
        print('============== topics ==============')
        print(topics)

        # TODO: implement multi-threading here. Collect the results after ward ...

        # try the multi-threading
        # collect both the answers, select the

        for topic in topics:
            if topic == 'wiki':
                try:
                    print('CoordinateAgent - 69', question)
                    question = self.remove_stop_words(question)
                    result = self.wiki_query(question)
                    if result is None:
                        pass
                    else:
                        print('RESULT RETURNED BY WIKI', result)
                        return result
                except:
                    print('[Error Coordinate Agent: 73]: Wiki Interface failed to process the question')
                    pass

            else:
                # TODO: insert the module prepared for pce/dft agent

                if topic == 'others':
                    rst = self.other_interface.ask_other(question)
                    print(rst)
                    if rst is None:
                        pass
                    else:
                        return rst

                else:
                    if ' CH2=CHCHO'.lower() in question.lower():
                        pass
                    else:
                        try:
                            result = self.jps_interface.analyse_questions(question)
                            self.logwriter.write_to_log(question, 'Result %s \n' % str(result))

                            print('RESULT RETURNED BY JPS', result)
                            if 'result' in result:
                                result_obj = json.loads(result)
                                result_list = result_obj['result']
                                if len(result_list) == 0:
                                    pass
                                else:
                                    if 'http://localhost:8080/ldfserver/Empty' in result and len(result_list) == 1:
                                        pass
                                    else:
                                        return result_list

                            else:
                                pass
                        except:
                            print('[Error Coordinate Agent: 84]: JPS Interface failed to analyse the question')
                            pass

        # TODO: integrate the fallback mechanism for agents
        # e.g. what is the power conversion efficiency of benzene
        # 1. will this question fail? how do you make sure the question fails? adjust the threshold of lookup
        # 2. go to the agent model, interpret the question with the agent nlu

        # TODO: increase the threshold of lookup in JPS (for PCE)
        # 2. fallback to the agent channel
        #   a) You need to create the agent instances
        #   b) You need to train the model with the agent instances
        self.agent_query(question)
        self.msg.send_failed_message(question)
        return 'Nothing'

    # @lru_cache(maxsize=64)
    def wiki_query(self, question):
        intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
        print('CoordinateAgent - 106')
        pprint(intent_and_entities)
        # TODO: connect the new dictionary ...
        intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
        if intent_and_entities_with_uris is None:
            return None
        elif intent_and_entities_with_uris == 'Error001':
            # now switch intent to item_attribute_query, and the entity to entity...
            intent_and_entities['type'] = 'item_attribute_query'
            intent_and_entities['entities']['entity'] = intent_and_entities['entities']['class']

        intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
        print('CoordinateAgent - 120')
        pprint(intent_and_entities_with_uris)
        self.logwriter.write_to_log(question, 'Intents and entities %s \n' % json.dumps(intent_and_entities_with_uris))

        sparqls = self.sparql_constructor.fill_sparql_query(intent_and_entities_with_uris)
        self.logwriter.write_to_log(question, 'SPARQL constructed %s \n' % json.dumps(sparqls))

        if sparqls is None:
            print('No valid SPARQL is returned')
            return None
        if len(sparqls) >= 5:
            sparqls = sparqls[:5]

        result = self.sparql_query.start_queries(sparqls)
        self.logwriter.write_to_log(question, 'Result got %s \n' % json.dumps(result[0]))

        return result[0]


if __name__ == '__main__':
    ca = CoordinateAgent(None)

    ca.run('  show me the vibration frequency of H2O2')
    # ca.run('what reactions produce NO2 + O2')
    # ca.run('find all the fatty acids with molecular weight more than 100')
    # ca.run('the kindling point of C2HBrClF3')
    # ca.run('what is the molecular weight of benzene')
    # ca.run('show me the heat capacity of glucose')
    # ca.run('what is the chemical structure of glucose')
    # ca.run('show me the boliing point of ch4')
