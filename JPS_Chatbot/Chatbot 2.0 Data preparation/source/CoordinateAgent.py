from Chatbot.Interpretation_parser import InterpretationParser
from Chatbot.SearchEngine import SearchEngine
from Chatbot.SPARQLConstructor import SPARQLConstructor
from Chatbot.SPARQLQuery import SPARQLQuery
from Chatbot.LDA_classifier import LDAClassifier
from pprint import pprint
from rasa.nlu.model import Interpreter
import os
import tarfile
import wolframalpha


# 0. get the topic model result, choose which direction it goes
# 1. get the InterpretationParse

def extract_nlu_model(extract_dir='../models/'):
    # Identify the newest trained nlu model
    path = 'models/'
    files = os.listdir(path)
    paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
    file_name = max(paths, key=os.path.getctime)
    # Extract the model to a temporary directory
    tf = tarfile.open(file_name)
    tf.extractall(path=extract_dir)


class CoordinateAgent():
    def __init__(self):
        # initialize interpreter
        extract_nlu_model()
        self.stopwords = ['all', 'the']
        # self.stopwords.append('all')
        self.interpreter = Interpreter.load('models/nlu')

    def run(self, question):

        # TODO: put the LDA model here
        # ===================== initialize the things for wiki
        self.interpreter_parser = InterpretationParser()
        self.interpreter_parser.interpreter = self.interpreter
        self.search_engine = SearchEngine()
        self.sparql_constructor = SPARQLConstructor()
        self.sparql_query = SPARQLQuery()
        self.lda_classifier = LDAClassifier()




        topics = self.lda_classifier.classify(question)
        print(topics)
        for topic in topics:
            if topic == 'wiki':
                try:
                    result = self.wiki_query(question)
                except:
                    # TODO: add JPS results to it
                    pass

    def wiki_query(self, question):
        intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
        intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
        if intent_and_entities_with_uris is None:
            return None
        elif intent_and_entities_with_uris == 'Error001':
            # now switch intent to item_attribute_query, and the entity to entity...
            print(intent_and_entities)
            print('we have a Error001')
            intent_and_entities['type'] = 'item_attribute_query'
            intent_and_entities['entities']['entity'] = intent_and_entities['entities']['class']

        intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
        print('================= result with uris ================')
        pprint(intent_and_entities_with_uris)
        sparqls = self.sparql_constructor.fill_sparql_query(intent_and_entities_with_uris)
        if sparqls is None:
            print('No valid SPARQL is returned')
            return None
        if len(sparqls) >= 5:
            sparqls = sparqls[:5]

        result = self.sparql_query.start_queries(sparqls)
        print('-------------- we have a result -------------------')
        pprint(result)
        return result[0]


# ca = CoordinateAgent()
# ca.run('melting points of alkaline earth metals')
# ca.run('what reactions produce NO2 + O2')
# ca.run('find all the fatty acids with molecular weight more than 100')
# ca.run('the kindling point of C2HBrClF3')
# # # r = ca.run(question='what is the molecular weight of benzene')
# # ca.run(question='show me the heat capacity of glucose')
# # ca.run(question='what is the chemical structure of glucose')
# ca.run('show me the boliing point of ch4')
