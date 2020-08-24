import json
from pprint import pprint


class SPARQLConstructor:
    def __init__(self):
        self.templates = {
            'batch_restriction_query_numerical_and_attribute': '''
        SELECT ?oLabel ?v ?unitLabel
        WHERE {?o wdt:%s  wd:%s .   
        ?o wdt:%s ?x . 
        ?o p:%s/psv:%s ?value .
        ?value wikibase:quantityAmount ?v .
        ?value wikibase:quantityUnit ?unit .
        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
        FILTER(?x %s %s ).  }  
        ''',
            'item_attribute_query': '''
        SELECT ?o\nWHERE \n{\n  wd:%s wdt:%s ?o .  \n\n} LIMIT 50
        ''',
            'batch_restriction_query': '''
                    
        SELECT ?oLabel  ?v ?value ?unitLabel\nWHERE\n{\n   ?o wdt:P31  wd:%s .\n   ?o wdt:%s ?v .\n  
        OPTIONAL {\n   ?o  p:%s/psv:%s ?v .\n   ?v     wikibase:quantityAmount     ?value.\n   ?v     
        wikibase:quantityUnit       ?unit.\n}\n   SERVICE wikibase:label { bd:serviceParam 
        wikibase:language \"[AUTO_LANGUAGE],en\". }\n      } LIMIT 50\n ''',
            'batch_restriction_query_numerical':
                '''SELECT ?o ?v\nWHERE\n  {\n    ?o wdt:P31 wd:%s .\n    ?o wdt:%s ?v .\n    FILTER (?v %s %s)\n   SERVICE 
        wikibase:label { bd:serviceParam wikibase:language \"[AUTO_LANGUAGE],en\". }\n } LIMIT 50 
        '''
        }

        pprint(self.templates['item_attribute_query'])

    def fill_sparql_query(self, intent_and_entities_with_uris):
        #     {'entities': [{'attribute': ['http://www.wikidata.org/entity/P8224',
        #                                  'http://www.wikidata.org/entity/P274',
        #                                  'http://www.wikidata.org/entity/P2067']},
        #                   {'entity': ['http://www.wikidata.org/entity/Q210385',
        #                               'http://www.wikidata.org/entity/Q416723',
        #                               'http://www.wikidata.org/entity/Q306051',
        #                               'http://www.wikidata.org/entity/Q2270']}],
        #      'intent': 'item_attribute_query'}
        entities = intent_and_entities_with_uris['entities']
        intent = intent_and_entities_with_uris['intent']


        if intent == 'item_attribute_query':
            order = ['attribute', 'entity']
        elif intent == 'batch_restriction_query_numerical_and_attribute':
            template = self.templates[intent]
            comparison = '>'
            number = 10
            print('======================== ')
            order = ['attribute', 'attribute', 'class']
            for e in entities:
                key = ''
                uris = []
                for k, u in e.items():
                    key = k
                    uris = u
                print('key', key, 'uris', uris)
                print('------------------------------')

sc = SPARQLConstructor()
