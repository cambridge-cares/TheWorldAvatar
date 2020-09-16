# Our wolfram alpha user id is G5WRTA-882W6ATLGU
import json
from pprint import pprint

app_id = 'G5WRTA-882W6ATLGU'
import wolframalpha

client = wolframalpha.Client(app_id)

# res = client.query('stability of FMOC amide + zinc')


object_template =  {"head": {"vars": ["v"]}, "results": {"bindings": []}}

#[{"v": {"value": "147"}}]
res = client.query('What is the atomic radius of S?')
bindings = []
counter = 0
for pod in res.pods:

    counter = counter + 1
    for sub in pod.subpods:
        text = sub.plaintext
        if counter <= 2:
            bindings.append({'v': {'value': text}})

object_template['results']['bindings'] = bindings

pprint(object_template)


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
        index_order = [1, 0, 1, 0, 0, 1]
    elif intent == 'batch_restriction_query_numerical_and_attribute':
        order = ['attribute', 'attribute', 'class', 'comparison', 'numerical_value']  # first attribute is the
        index_order = [2, 1, 0, 0, 3, 4]
        # attribute_1

    elif intent == 'batch_restriction_query_numerical':
        order = ['class', 'attribute', 'comparison', 'numerical_value']
        index_order = [0, 1, 2, 3, 1, 1]
        # class, attribute, comparison, numerical_value, attribute, attribute
    elif intent == 'batch_restriction_query':
        # class, attribute, attribute, attribute
        order = ['class', 'attribute']
        index_order = [0, 1, 1, 1]
    if len(order) == 0:
        return None
    else:
        return None

