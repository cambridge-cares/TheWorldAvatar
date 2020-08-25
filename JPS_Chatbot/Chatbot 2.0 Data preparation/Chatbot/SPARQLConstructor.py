import json
from pprint import pprint
import itertools


def generate_combinations(results):
    # TODO: to generate a list of combinations with a score ...
    temp = []
    for uris in results:
        if type(uris) is str:
            uris = [uris]
            temp.append(uris)
        else:
            temp.append(uris)
    list_of_combination = list(itertools.product(*temp))
    # TODO: calculate a score for the results
    list_of_combination_with_score = []
    for combination in list_of_combination:
        combination_score = 1
        for item in combination:
            if type(item) is tuple:
                uri = item[0]
                score = item[1]
                if score != 0:
                    combination_score = combination_score * score

        list_of_combination_with_score.append({'uris': combination, 'score': combination_score})

    # print('-------------  list_of_combination_with_score --------------')
    # pprint(list_of_combination_with_score)
    sorted_list = [t for t in sorted(list_of_combination_with_score, key=lambda item: item['score'], reverse=True)]
    # pprint(sorted_list[:5])
    return sorted_list


def retrieve_uris_from_entities(entities, order):
    # get the object by label, the remove the object
    print('---------- entities -----------')
    pprint(entities)
    print('\n')
    temp_list = entities
    results = []
    for e, o_label in zip(entities, order):
        key = ''
        uris = []
        for k, u in e.items():
            key = k
            uris = u
        if key == o_label:
            # return the result, remove it from the list
            results.append(uris)
        print('key', key, 'uris', uris)
        print('------------------------------')
    pprint(results)
    return results
    # TODO: put the comparison and the number in, apply filter over them


def fill_sparql_query_for_one_intent(template, order, entities):
    list_of_sparqls = []
    # this function takes the template, entities, the order, returns a list of sparql queries
    r = retrieve_uris_from_entities(entities, order=order)
    combinations = generate_combinations(r)
    for comb in combinations:
        elements = []
        uris = comb['uris']
        for u in uris:
            if type(u) is tuple:
                elements.append(u[0])
            else:
                elements.append(u)
        try:
            sparql_query = template % (elements[2], elements[1], elements[0], elements[0], elements[3], elements[4])
            print('-------------- sparql query --------------')
            print(sparql_query)
            list_of_sparqls.append(sparql_query)
        except:
            return None
    return list_of_sparqls


class SPARQLConstructor:
    def __init__(self):
        self.templates = {
            'batch_restriction_query_numerical_and_attribute': '''
        SELECT ?oLabel ?v ?unitLabel
        WHERE {
        ?o wdt:P31 wd:%s . # class    
        ?o wdt:%s ?x . # attribute_2  
        ?o p:%s/psv:%s ?value . # attribute_1 x 2 
        ?value wikibase:quantityAmount ?v .
        ?value wikibase:quantityUnit ?unit .
        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
        FILTER(?x %s %s ).  } # comparison numerical_value  
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

    # TODO: generalize the fill_sparql_query
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
        template = self.templates[intent]

        if intent == 'item_attribute_query':
            order = ['attribute', 'entity']
        elif intent == 'batch_restriction_query_numerical_and_attribute':
            order = ['attribute', 'attribute', 'class', 'comparison', 'numerical_value']  # first attribute is the
            # attribute_1
        if len(order) == 0:
            return None
        else:
            return fill_sparql_query_for_one_intent(template=template, order=order, entities=entities)

    # this is a tricky move, for type 4, attributes must be put in proper order

# sc = SPARQLConstructor()
# test_object = {'entities': [{'attribute': ['http://www.wikidata.org/entity/P1117',
#                                            'http://www.wikidata.org/entity/P2561',
#                                            'http://www.wikidata.org/entity/P527']},
#                             {'attribute': ['http://www.wikidata.org/entity/P8224',
#                                            'http://www.wikidata.org/entity/P2067']},
#                             {'class': ['http://www.wikidata.org/entity/Q2653638',
#                                        'http://www.wikidata.org/entity/Q21050798',
#                                        'http://www.wikidata.org/entity/Q56397496']}],
#                'intent': 'batch_restriction_query_numerical_and_attribute'}
# sc.fill_sparql_query(test_object)
