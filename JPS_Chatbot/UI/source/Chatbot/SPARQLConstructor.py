import json
from pprint import pprint
import itertools

# try:
#     from __main__ import socketio
#
#     print('Importing socketIO from main in interpretation')
# except ImportError:
#     from run import socketio
#
#     print('Importing socketIO from run_socket in interpretation')

def generate_combinations(results):
    # TODO: to generate a list of combinations with a score ...
    temp = []
    print('generate_combinations results')
    print(results)
    print('-------------------------------')

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
        min_score = 100
        combination_score = 1
        for item in combination:
            if type(item) is tuple:
                uri = item[0]
                score = item[1]
                int_score = int(score)
                # if int(score) < min_score:
                #     min_score = int(score)
                if score != 0:
                    combination_score = combination_score * score
        # if min_score <= 70:
        #     pass
        # else:
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
    for o_label in order:
        for e in entities:
            key = ''
            uris = []
            for k, u in e.items():
                key = k
                uris = u
            if key == o_label:
                print('the key is', key)
                print('the olabel is', o_label)
                # return the result, remove it from the list
                results.append(uris)
            print('key', key, 'uris', uris)
            print('------------------------------')
    pprint(results)
    return results
    # TODO: put the comparison and the number in, apply filter over them
def rename_keys(es):
    counter = 0
    for e in es:
        counter = counter + 1
        k = list(e.keys())[0]
        if k == 'attribute':
            new_k = 'attribute_' + str(counter)
            e[new_k] = e.pop(k)
        print(e)
    return es

def fill_sparql_query_for_one_intent(intent, template, order, entities, index_order):
    list_of_sparqls = []
    # this function takes the template, entities, the order, returns a list of sparql queries
    if intent == 'batch_restriction_query_numerical_and_attribute':
        entities = rename_keys(entities)
    r = retrieve_uris_from_entities(entities, order=order)
    combinations = generate_combinations(r)
    print('=============== check =============')
    print('entities', entities)
    print('order', order)
    print('index order', index_order)
    for index in index_order:
        print(entities[index])
    # x = input()
    # formula of fatty acid with density more than 40
    # aromatic hydrocarbon with molecular weight more than 200
    print('combinantion', combinations)
    for comb in combinations:
        print('each comb')
        print(comb)
        print('---------')
        elements = []
        uris = comb['uris']
        for u in uris:
            print('--- the u is ----')
            print(u)
            if type(u) is tuple:
                elements.append(u[0])
            else:
                elements.append(u)

        try:
            candidates = []
            for i in index_order:
                candidates.append(elements[i])
            print('--------- the candidates are ---------')
            print(candidates)
            elements = tuple(candidates)
            print('elements', elements)
            # sparql_query = template % (elements[2], elements[1], elements[0], elements[0], elements[3], elements[4])
            sparql_query = template % elements
            print('-------------- sparql query --------------')
            print(sparql_query)
            # x = input()
            list_of_sparqls.append(sparql_query)
        except:
            return None
    return list_of_sparqls

# This class constructs SPARQL query for wikidata
class SPARQLConstructor:
    def __init__(self):
        self.templates = {
            'batch_restriction_query': '''
            
        SELECT ?oLabel ?v ?v2 ?unitLabel
        WHERE
         {
          ?o wdt:P31 wd:%s . # class
          ?o wdt:%s ?v . # attribute
          OPTIONAL {
                ?o p:%s/psv:%s ?value . # attribute x 2 
                ?value wikibase:quantityAmount ?v2 .
                ?value wikibase:quantityUnit ?unit .
          }
          SERVICE wikibase:label { bd:serviceParam wikibase:language  "[AUTO_LANGUAGE],en". }
        } LIMIT 50 # class, attribute, attribute, attribute
         ''',

            'batch_restriction_query_numerical': '''
            
        SELECT ?oLabel ?v ?v2 ?unitLabel
        WHERE
         {
          ?o wdt:P31 wd:%s . # class
          ?o wdt:%s ?v . # attribute
          FILTER (?v %s %s) # comparison, numerical_value
          OPTIONAL {
                ?o p:%s/psv:%s ?value . # attribute x 2 
                ?value wikibase:quantityAmount ?v2 .
                ?value wikibase:quantityUnit ?unit .
          }
          SERVICE wikibase:label { bd:serviceParam wikibase:language  "[AUTO_LANGUAGE],en". }
        } LIMIT 50 # class, attribute, comparison, numerical_value, attribute, attribute
        ''',

            'item_attribute_query': '''
            
        SELECT ?name ?v ?v2 ?unitLabel WHERE  {                     
        
        wd:%s wdt:%s ?v2 .
        OPTIONAL {
        wd:%s p:%s/psv:%s ?value . # attribute
        wd:%s rdfs:label ?name filter (lang(?name) = "en").
        ?value wikibase:quantityAmount ?v .
        ?value wikibase:quantityUnit ?unit . 
        }

        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }}  
        ''',

            'batch_restriction_query_numerical_and_attribute': '''
             
        SELECT ?oLabel ?v ?v2 ?unitLabel
        WHERE {
        ?o wdt:P31 wd:%s . # class
        ?o wdt:%s ?v2 . # attribute_2
        ?o wdt:%s ?v .   # attribute_1
        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
        FILTER(?v2 %s %s ).  } # comparison numerical_value
         
        
        '''}

    # TODO: generalize the fill_sparql_query
    def fill_sparql_query(self, intent_and_entities_with_uris):

        # socketio.emit('coordinate_agent', 'Constructing SPARQL query ')

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
            index_order = [1, 0, 1, 0, 0, 1]
        elif intent == 'batch_restriction_query_numerical_and_attribute':
            order = ['attribute_1', 'attribute_2', 'class', 'comparison', 'numerical_value']  # first attribute is the
            index_order = [2, 1, 0, 3, 4] #         ['class', 'a2', 'a1', 'comparison', 'number']

            # attribute_1

        elif intent == 'batch_restriction_query_numerical':
            order = ['class', 'attribute', 'comparison', 'numerical_value']
            index_order = [0, 1, 2, 3, 1, 1]
            # class, attribute, comparison, numerical_value, attribute, attribute
        elif intent == 'batch_restriction_query':
            # class, attribute, attribute, attribute
            order = ['class', 'attribute']
            index_order = [0,1,1,1]
        if len(order) == 0:
            return None
        else:
            return fill_sparql_query_for_one_intent(intent=intent, template=template, order=order, entities=entities,
                                                    index_order=index_order)

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
