from pprint import pprint

from Chatbot.Interpretation_parser import InterpretationParser
from Chatbot.SearchEngine import SearchEngine
from Chatbot.SPARQLConstructor import SPARQLConstructor
# 0. get the topic model result, choose which direction it goes
# 1. get the InterpretationParse

class CoordinateAgent():
    def __init__(self):
        # initialize interpreter
        self.interpreter = InterpretationParser()
        self.search_engine = SearchEngine()
        self.sparql_constructor = SPARQLConstructor()
    def run(self, question):
        intent_and_entities = self.interpreter.parse_question_interpretation(question)
        intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
        print('================= result with uris ================')
        pprint(intent_and_entities_with_uris)
        sparqls = self.sparql_constructor.fill_sparql_query(intent_and_entities_with_uris)


ca = CoordinateAgent()
# r = ca.run(question='what is the molecular weight of benzene')
r = ca.run(question='what is the pka of all the acids with a molecular weight over 200')

