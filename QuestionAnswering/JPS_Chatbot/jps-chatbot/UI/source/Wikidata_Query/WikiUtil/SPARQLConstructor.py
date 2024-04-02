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

    sorted_list = [t for t in sorted(list_of_combination_with_score, key=lambda item: item['score'], reverse=True)]
    return sorted_list


def retrieve_uris_from_entities(entities, order):
    # get the object by label, the remove the object
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
                # return the result, remove it from the list
                results.append(uris)

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
    return es


def fill_sparql_query_for_one_intent(intent, template, order, entities, index_order):
    list_of_sparqls = []
    # this function takes the template, entities, the order, returns a list of sparql queries
    if intent == 'attribute_batch_attribute_query_numerical':
        entities = rename_keys(entities)
        print('SPARQLConstructor - 75')
        pprint(entities)
    r = retrieve_uris_from_entities(entities, order=order)
    combinations = generate_combinations(r)
    # x = input()
    # formula of fatty acid with density more than 40
    # aromatic hydrocarbon with molecular weight more than 200
    for comb in combinations:
        elements = []
        uris = comb['uris']
        for u in uris:
            if type(u) is tuple:
                elements.append(u[0])
            else:
                elements.append(u)

        try:
            candidates = []
            for i in index_order:
                candidates.append(elements[i])
            elements = tuple(candidates)
            sparql_query = template % elements
            list_of_sparqls.append(sparql_query)
        except:
            return None

    return list_of_sparqls


# This class constructs SPARQL query for wikidata
class SPARQLConstructor:
    def __init__(self):
        self.templates = {

            'rank_query_list': '''
             
            SELECT  ?oLabel ?v  ?unitLabel
            WHERE 
            {
              ?o wdt:P31 wd:%s . # class
              ?o wdt:%s ?v . # attribute
              
                        OPTIONAL {
                            ?o p:%s/psv:%s ?value . # attribute x 2 
                            ?value wikibase:quantityAmount ?v2 .
                            ?value wikibase:quantityUnit ?unit .
                      }
              
              SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
            
            } 
            ORDER BY DESC(?v)
            
            ''',

            'batch_query': '''
            
             SELECT ?o ?oLabel 
            WHERE 
            {
              ?o wdt:P31 wd:%s.
              SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
            }
                       
            ''',


            'about_query':

                '''
            PREFIX schema: <http://schema.org/>
            SELECT *
            WHERE 
            {
              wd:%s rdfs:label ?v .
              wd:%s schema:description ?v2.
              SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
              FILTER ( lang(?v) = "en" )
              FILTER ( lang(?v2) = "en" )
            }
            
            ''',

            'batch_attribute_query': '''
            
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

            'batch_attribute_query_numerical': '''
            
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

            'attribute_batch_attribute_query_numerical': '''
          SELECT ?oLabel ?v ?v2 ?unitLabel
        WHERE
         {
          ?o wdt:P31 wd:%s . # class
          ?o wdt:%s ?v . # attribute_2
          ?o wdt:%s ?v2 . # attribute_1
          FILTER (?v %s %s) # comparison, number
          SERVICE wikibase:label { bd:serviceParam wikibase:language  "[AUTO_LANGUAGE],en". }
        } LIMIT 50 # class, attribute, comparison, numerical_value, attribute, attribute
        
        ''',

            'item_attribute_query': '''
            
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
        SELECT (GROUP_CONCAT(DISTINCT STR(?name); SEPARATOR=", ") AS ?names) ?v ?unitLabel WHERE  {                     
        wd:%s wdt:%s ?v .
        OPTIONAL {
        wd:%s p:%s/psv:%s ?value . # attribute
        ?value wikibase:quantityAmount ?v .
        ?value wikibase:quantityUnit ?unit . 
        }
        OPTIONAL {
         wd:%s rdfs:label ?name . 
        FILTER (langMatches( lang(?name), "en" ) )
        }

        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
        }  GROUP BY ?names ?v ?unitLabel
 
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
            order = ['attribute', 'species']
            index_order = [1, 0, 1, 0, 0, 1]

        elif intent == 'rank_query_list':
            order = ['class', 'attribute']
            index_order = [0,1,1,1]

        elif intent == 'batch_query':
            order = ['class']
            index_order = [0]

        elif intent == 'about_query':
            order = ['species']
            index_order = [0,0]

        elif intent == 'attribute_batch_attribute_query_numerical':
            order = ['attribute_1', 'attribute_2', 'class', 'comparison', 'number']  # first attribute is the
            index_order = [2, 1, 0, 3, 4]  # ['class', 'a2', 'a1', 'comparison', 'number']

        elif intent == 'batch_attribute_query_numerical':
            order = ['class', 'attribute', 'comparison', 'number']
            index_order = [0, 1, 2, 3, 1, 1]
            # class, attribute, comparison, numerical_value, attribute, attribute
        elif intent == 'batch_attribute_query':
            # class, attribute, attribute, attribute
            order = ['class', 'attribute']
            index_order = [0, 1, 1, 1]
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
