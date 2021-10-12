import json

from util.ModelLoader import AGENT_NLU_MODEL, WIKI_NLU_MODEL, JPS_NLU_MODEL
from util.MarieLogger import MarieIOLog
from util.StopWords import removeStopWords
from Wikidata_Query.WikiQueryInterface import WikiQueryInterface
from JPS_Query.JPSQueryInterface import JPSQueryInterface
from Agent_Query.AgentQueryInterface import AgentQueryInterface
from LDA.LDA_classifier import LDAClassifier


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

        self.topic_function_map = {'wiki': self.wiki_query,
                                   'ontocompchem': self.jps_query,
                                   'ontokin': self.jps_query,
                                   'agent': self.agent_query}

    # @MarieLog
    def run(self, _question):
        # Identify the topics
        topics = self.identify_topic(_question)
        topics.append('agent')
        rst = None
        topics = ['agent']
        for topic in topics:
            rst = self.topic_function_map[topic](self.topic_model_map[topic], _question)

            # parse_result = self.parse_question(topic, _question)
            # try:
            #     # select the function and model according to the topic
            #     rst = self.topic_function_map[topic](self.topic_model_map[topic], _question)
            # except Exception:
            #     logging.warning('{} failed to provide an answer for {}'.format(topic, _question))
            #     logging.error('{}'.format(traceback.format_exc()))

            if rst is None:
                pass
            else:
                try:
                    rst = json.loads(rst)
                except:
                    rst = rst
                return rst

    @MarieIOLog
    def identify_topic(self, _question):
        return self.lda_classifier.classify(_question)

    @MarieIOLog
    def wiki_query(self, model, question):
        question = removeStopWords(question)
        return WikiQueryInterface(model).wiki_query(question)

    @MarieIOLog
    def agent_query(self, model, question):
        print('Doing Agent query', question)
        return AgentQueryInterface().agent_query(question)


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
    # ca.run('geometry c=c=c=c')
    # ca.run('show me the pce of c=c=c=c')
    ca.run('what is the enthalpy of InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3 at 1522.1 Pa and 123245 K')
    # jps_questions = ['show me the vibration frequency of H2O2', 'symmetry number of c9h14']
    # for jps_q in jps_questions:
    #     answer = ca.run(jps_q)
















# import json
# import sys
# from json import JSONDecodeError
# from pprint import pprint
#
# from Wikidata_Query.Interpretation_parser import InterpretationParser
# from Wikidata_Query.SearchEngine import SearchEngine
# from Wikidata_Query.SPARQLConstructor import SPARQLConstructor
# from Wikidata_Query.SPARQLQuery import SPARQLQuery
# from LDA.LDA_classifier import LDAClassifier
# from JPS_Query.chatbot_interface import Chatbot
# # from dashboard.LogWriter import LogWriter
# from dashboard.Messenger import Messenger
# from Agent_Query.AgentRequestConstructor import AgentRequestConstructor
#
# from rasa.nlu.model import Interpreter
# import os
# import tarfile
#
# from location import WIKI_MODELS_DIR, AGENT_MODELS_DIR
#
#
# def screen_clear():
#     # for mac and linux(here, os.name is 'posix')
#     if os.name == 'posix':
#         _ = os.system('clear')
#     else:
#         # for windows platfrom
#         _ = os.system('cls')
#
#
# def marie_logger(func):
#     import logging
#     logging.basicConfig(stream=sys.stdout, level=logging.INFO)
#
#     def wrapper(*args, **kwargs):
#         logging.info('Running logger')
#         return func(*args, **kwargs)
#     return wrapper
#
#
# # 0. get the topic model result, choose which direction it goes
# # 1. get the InterpretationParse
#
# def extract_nlu_model(extract_dir='../models/'):
#     # Identify the newest trained nlu model
#     # Disable the function when deployed to production server ...
#     path = 'Wikidata_Query/models/'
#     files = os.listdir(path)
#     paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
#     file_name = max(paths, key=os.path.getctime)
#     # Extract the model to a temporary directory
#     tf = tarfile.open(file_name)
#     tf.extractall(path=extract_dir)
#
#
# class CoordinateAgent:
#     def __init__(self, socketio):
#         # initialize interpreter
#         # extract_nlu_model()
#         self.search_engine = SearchEngine()
#         self.stopwords = ['all', 'the']
#         self.nlu_model_directory = os.path.join(WIKI_MODELS_DIR, 'nlu')
#         self.interpreter = Interpreter.load(self.nlu_model_directory)  # load the wiki nlu models
#
#         self.agent_nlu_model_directory = os.path.join(AGENT_MODELS_DIR, 'nlu')
#         self.agent_interpreter = Interpreter.load(self.agent_nlu_model_directory)
#         self.agent_request_constructor = AgentRequestConstructor()
#
#         self.jps_interface = Chatbot(socketio)
#         self.socket = socketio
#         # self.logwriter = LogWriter()
#         self.msg = Messenger()
#         screen_clear()
#
#     # def return_for_more(self, agent_id):
#     #     pass
#     #
#
#     @marie_logger
#     def agent_query(self, question):
#         rst = self.agent_interpreter.parse(question)
#         with open('agent_query_log') as f:
#             try:
#                 agent_query_log = json.loads(f.read())
#                 f.close()
#             except JSONDecodeError:
#                 agent_query_log = {}
#
#         agent_query_log[question.strip()] = rst
#         with open('agent_query_log', 'w') as f:
#             f.write(json.dumps(agent_query_log))
#             f.close()
#         try:
#             response = self.agent_request_constructor.call_agent(rst)
#             return response
#         except:
#             return None
#
#     def remove_stop_words(self, question):
#         stopwords = ['the', 'an', 'a', 'is', 'what', 'are', 'of', 'describe', 'find', 'find me']
#         question = question.strip()
#         question = question.lower()
#         words = question.split(' ')
#         words = [w for w in words if w not in stopwords]
#         return ' '.join(words)
#
#     def question_classification(self, question):
#         intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
#         return intent_and_entities['intent']
#
#     def named_entity_recognition(self, question):
#         intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
#         return intent_and_entities['entities']
#
#     # @lru_cache(maxsize=None)
#     def run(self, question):
#
#         # TODO: use the proper logger to replace current logger, use decorator
#         # TODO: clean the code up a little bit
#
#         # TODO: put the LDA model here
#         # ===================== initialize the things for wiki
#         self.interpreter_parser = InterpretationParser(self.socket)
#         self.interpreter_parser.interpreter = self.interpreter
#         self.sparql_constructor = SPARQLConstructor()
#         self.sparql_query = SPARQLQuery(self.socket)
#         self.lda_classifier = LDAClassifier()
#         topics = self.lda_classifier.classify(question)
#         # self.logwriter.write_to_log(question, 'Topics identified %s \n' % json.dumps(topics))
#
#         # TODO: implement multi-threading here. Collect the results after ward ...
#
#         # try the multi-threading
#         # collect both the answers, select the
#
#         for topic in topics:
#             if topic == 'wiki':
#                 try:
#                     question = self.remove_stop_words(question)
#                     result = self.wiki_query(question)
#                     if result is None:
#                         pass
#                     else:
#                         return result
#                 except:
#                     pass
#
#             else:
#                 # TODO: insert the module prepared for pce/dft agent
#
#                 if topic == 'others':
#                     rst = self.other_interface.ask_other(question)
#                     if rst is None:
#                         pass
#                     else:
#                         return rst
#
#                 else:
#                     if ' CH2=CHCHO'.lower() in question.lower():
#                         pass
#                     else:
#                         try:
#                             result = self.jps_interface.analyse_questions(question)
#                             self.logwriter.write_to_log(question, 'Result %s \n' % str(result))
#                             if 'result' in result:
#                                 result_obj = json.loads(result)
#                                 result_list = result_obj['result']
#                                 if len(result_list) == 0:
#                                     pass
#                                 else:
#                                     if 'http://localhost:8080/ldfserver/Empty' in result and len(result_list) == 1:
#                                         pass
#                                     else:
#                                         return result_list
#
#                             else:
#                                 pass
#                         except:
#                             pass
#
#         # TODO: integrate the fallback mechanism for agents
#         # e.g. what is the power conversion efficiency of benzene
#         # 1. will this question fail? how do you make sure the question fails? adjust the threshold of lookup
#         # 2. go to the agent model, interpret the question with the agent nlu
#
#         # TODO: increase the threshold of lookup in JPS (for PCE)
#         # 2. fallback to the agent channel
#         #   a) You need to create the agent instances
#         #   b) You need to train the model with the agent instances
#         self.agent_query(question)
#         self.msg.send_failed_message(question)
#         return 'Nothing'
#
#     # @lru_cache(maxsize=64)
#     def wiki_query(self, question):
#         # intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
#         # # TODO: connect the new dictionary ...
#         # intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
#         # if intent_and_entities_with_uris is None:
#         #     return None
#         # elif intent_and_entities_with_uris == 'Error001':
#         #     # now switch intent to item_attribute_query, and the entity to entity...
#         #     intent_and_entities['type'] = 'item_attribute_query'
#         #     intent_and_entities['entities']['entity'] = intent_and_entities['entities']['class']
#         #
#         # intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
#         # self.logwriter.write_to_log(question, 'Intents and entities %s \n' % json.dumps(intent_and_entities_with_uris))
#         # sparqls = self.sparql_constructor.fill_sparql_query(intent_and_entities_with_uris)
#         # self.logwriter.write_to_log(question, 'SPARQL constructed %s \n' % json.dumps(sparqls))
#         #
#         # if sparqls is None:
#         #     return None
#         # if len(sparqls) >= 5:
#         #     sparqls = sparqls[:5]
#         #
#         # result = self.sparql_query.start_queries(sparqls)
#         # self.logwriter.write_to_log(question, 'Result got %s \n' % json.dumps(result[0]))
#
#         return result[0]
#
#
# if __name__ == '__main__':
#     ca = CoordinateAgent(None)
#     thermo_agent_questions = ['what is the gibbs energy of H2O2',
#                               'what is the inner energy of methane at 100 K',
#                               'plot the heat capacity of CO2',
#                               'heat capacity of c1cccc1 at 1 atm',
#                               'heat capacity of c1cccc1 at room temperature',
#                               'find the entropy of benzene at 100000 Pa',
#                               'what is the enthalpy of InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3 at 1522.1 Pa and 123245 K']
#     for t_a_q in thermo_agent_questions:
#         ca.run(t_a_q)
#     # ca.run('  show me the vibration frequency of H2O2')
#     # ca.run('what reactions produce NO2 + O2')
#     # ca.run('find all the fatty acids with molecular weight more than 100')
#     # ca.run('the kindling point of C2HBrClF3')
#     # ca.run('what is the molecular weight of benzene')
#     # ca.run('show me the heat capacity of glucose')
#     # ca.run('what is the chemical structure of glucose')
#     # ca.run('show me the boliing point of ch4')
