import json
import logging

if __name__ == '__main__':
    from WikiUtil.SearchEngine import SearchEngine
    from WikiUtil.SPARQLConstructor import SPARQLConstructor
    from WikiUtil.SPARQLQuery import SPARQLQuery
    from WikiUtil.InterpretationParser import InterpretationParser

else:
    from .WikiUtil.SearchEngine import SearchEngine
    from .WikiUtil.SPARQLConstructor import SPARQLConstructor
    from .WikiUtil.SPARQLQuery import SPARQLQuery
    from .WikiUtil.InterpretationParser import InterpretationParser


def MarieIOLog(func):
    def wrapper(*args, **kwargs):
        logging.basicConfig(level=logging.DEBUG)
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
    def __init__(self, model_dir):
        self.interpreter_parser = InterpretationParser(model_dir)
        self.search_engine = SearchEngine()
        self.sparql_constructor = SPARQLConstructor()
        self.sparql_query = SPARQLQuery()

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
        if intent_and_entities_with_uris is None:
            return None
        sparqls = self.sparql_constructor.fill_sparql_query(intent_and_entities_with_uris)
        if sparqls is None:
            return None
        result = self.sparql_query.start_queries(sparqls)
        if result is None:
            return None
        else:
            query_result = result[0][0]
            return query_result


if __name__ == '__main__':
    def removeStopWords(_question):
        stopwords = ['the', 'an', 'a', 'is', 'what', 'are', 'of', 'describe', 'find', 'find me']
        _question = _question.strip()
        _question = _question.lower()
        words = _question.split(' ')
        words = [w for w in words if w not in stopwords]
        return ' '.join(words)


    wqi = WikiQueryInterface('models/nlu')
    q = 'aromatic hydrocarbons with mass less than 170'
    q = 'what is methane'
    q = ' mass of aromatic hydrocarbons with mass less than 170'
    q = removeStopWords(q)
    print('processed question', q)
    rst = wqi.wiki_query(q)
    # rst = wqi.wiki_query('h2o2')
    print(rst)
