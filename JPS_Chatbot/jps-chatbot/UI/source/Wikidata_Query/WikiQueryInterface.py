import json
import logging
from pprint import pprint
from .SearchEngine import SearchEngine
from .SPARQLConstructor import SPARQLConstructor
from .SPARQLQuery import SPARQLQuery
from .Interpretation_parser import InterpretationParser


def MarieIOLog(func):
    def wrapper(*args, **kwargs):
        logger = logging.getLogger('Function I/O')
        logger.setLevel(logging.INFO)
        rst = func(*args)
        if rst is None:
            logger.warning('{} is called with input {} but returned None'.format(func.__name__, args[1:]))
        else:
            try:
                rst = json.dumps(rst, indent=4)
            except:
                pass
            logger.info('{} is called with input {} and output {}'.format(func.__name__, args[1:], rst))
        return rst

    return wrapper


class WikiQueryInterface:
    def __init__(self, model):
        self.interpreter_parser = InterpretationParser(None)
        self.search_engine = SearchEngine()
        self.sparql_constructor = SPARQLConstructor()
        self.sparql_query = SPARQLQuery()
        self.interpreter_parser.interpreter = model

    @MarieIOLog
    def wiki_query(self, question):
        intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
        intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
        if intent_and_entities_with_uris is None:
            return None
        elif intent_and_entities_with_uris == 'Error001':
            # now switch intent to item_attribute_query, and the entity to entity...
            intent_and_entities['type'] = 'item_attribute_query'
            intent_and_entities['entities']['entity'] = intent_and_entities['entities']['class']

        intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
        sparqls = self.sparql_constructor.fill_sparql_query(intent_and_entities_with_uris)
        if sparqls is None:
            return None
        result = self.sparql_query.start_queries(sparqls)
        if result is None:
            return None
        else:
            query_result = result[0][0]
            return query_result
