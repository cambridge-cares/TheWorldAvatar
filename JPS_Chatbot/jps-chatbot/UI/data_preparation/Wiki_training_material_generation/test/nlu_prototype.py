# get the question
# interpret it
# ols it
# make the SPARQL query
# lets look at the query 1/0
# lets fire it to the
# load the test questions
from pprint import pprint
import fuzzysearch_wiki
from rasa.nlu.model import Interpreter
import json


def convert_comparison(comparison):
    smaller_than = ['smaller than', 'less', 'less than', 'under', 'smaller', 'beneath', 'lower', 'lower than', 'fewer',
                    'beneath']
    larger_than = ['bigger', 'bigger than', 'larger than', 'larger', 'over', 'above', 'beyond', 'broader', 'broader ',
                   'than', 'more than', 'more']

    if comparison in smaller_than:
        return '<'
    else:
        return '>'


def process_questions(q):
    labels = ['species', 'class', 'attribute', 'reaction use ahead',
              'separator', 'reaction produce ahead', 'reaction produce', 'reaction use',
              'comparison', 'number']
    for label in labels:
        label = '(%s)' % label.strip()
        q = q.replace(label, '')
    q = q.replace('[', '').replace(']', '').strip()
    q2 = q

    q = q.lower()
    q = q.replace(' does ', ' ')
    q = q.replace(' for ', ' of ').replace(' all ', ' ')
    q = q.replace(' in ', ' of ')
    q = q.replace(' is ', ' ')
    q = q.replace(' the ', ' ')
    q = q.replace(' are ', ' ').strip()
    stop_wh = ['how much ', 'what ', 'give me', 'list', 'list me'
                                     'give ', 'show me', 'display', 'give',
               'where', 'get ', 'show', 'draw', 'select', 'determine', 'find', 'describe ', 'which', ' me ']

    for wh in stop_wh:
        q = q.replace(wh, '')

    return q, q2


def fill_sparql_template(converted_entities, intent):
    if intent in WIKI_TEMPLATES:
        template = WIKI_TEMPLATES[intent]
        query_list = []
        species_uris = []
        attribute_uris = []
        class_uris = []
        if "species" in converted_entities:
            species_uris = converted_entities['species']
        if "attribute" in converted_entities:
            attribute_uris = converted_entities['attribute']
        if "class" in converted_entities:
            class_uris = converted_entities['class']

        if intent == 'rank_query_list':
            for c_uri in class_uris:
                for a_uri in attribute_uris:
                    query = template % (c_uri, a_uri)
                    query_list.append(query)
        elif intent == 'item_attribute_query':
            # join the attributes
            if len(species_uris) == 0 and len(class_uris) > 0: # common mismatch between species and class
                pass
                # for a_uri in attribute_uris:
                    # for c_uri in class_uris:
                    #     query = template % (c_uri, a_uri)
                    #     query_list.append(query)

            else:
                for a_uri in attribute_uris:
                    for s_uri in species_uris:
                        query = template % (s_uri, a_uri)
                        query_list.append(query)

        elif intent == 'about_query':
            for s_uri in species_uris:
                query = template % s_uri
                query_list.append(query)

        elif intent == 'batch_query':
            for c_uri in class_uris:
                query = template % c_uri
                query_list.append(query)

        elif intent == 'batch_attribute_query':

            if len(class_uris) == 0 and len(species_uris) > 0: # common mismatch between species and class
                pass
                # for s_uri in species_uris:
                #     for a_uri in attribute_uris:
                #         query = template % (s_uri, a_uri, a_uri, a_uri)
                #         query_list.append(query)
            else:
                for c_uri in class_uris:
                    for a_uri in attribute_uris:
                        query = template % (c_uri, a_uri, a_uri, a_uri)
                        query_list.append(query)




        elif intent == 'batch_attribute_query_numerical':
            # class, attribute, comparison , number
            if 'comparison' in converted_entities and 'number' in converted_entities:
                comparison = converted_entities['comparison']
                comparison = convert_comparison(comparison)
                number = converted_entities['number']
                for a_uri in attribute_uris:
                    for c_uri in class_uris:
                        query = template % (c_uri, a_uri, comparison, number)
                        query_list.append(query)

    else:
        query_list = []
    query_list = [
        q.replace('http://www.wikidata.org/entity/', '').replace('wdt:rdfs:label', 'rdfs:label').replace('\n', ' ') for
        q in query_list]

    return query_list


def mapping_to_templates(converted_entities, intent):
    type_string = ' '.join(sorted(converted_entities.keys())).strip()
    fallback_dict = {
        'item_attribute_query': 'attribute species',
        'batch_attribute_query': 'attribute class',
        'about_query': 'species',
        'rank_query': 'attribute class',
        'batch_query': 'class',
        'batch_attribute_query_numerical': 'attribute class comparison number'
    }
    query_list = fill_sparql_template(converted_entities, intent)
    if len(query_list) > 0:
        pass
    else:
        # use the fallback based on the ner results
        for fallback_intent in fallback_dict:
            fallback_entities = fallback_dict[fallback_intent]
            if type_string == fallback_entities:
                # this thing then match a different template
                query_list = fill_sparql_template(converted_entities, fallback_intent)
                intent = fallback_intent
                print('# Fallback Intent: ', intent)
                for q in query_list:
                    print(q)
    print('# Intent:', intent)
    if len(query_list) > 0:
        successful.append(1)
        for q in query_list:
            print(q)
    # rank_query_list, class, attribute


def interpret_question(q):
    result = interpreter.parse(q)
    intent = result['intent']['name']
    # second_intent = result
    intent_confidence = result['intent']['confidence']
    entities = [(e['entity'], e['value']) for e in result['entities']]
    converted_entities = {}
    print('-------- entities --------')
    print(entities)
    attributes = []
    for entity in entities:
        entity_value = entity[1]
        entity_type = entity[0]
        if entity_type == 'attribute':
            attributes.append(entity_value)
        if entity_type in ['species', 'class']:
            URI = search_for_uri_wiki(entity_value, entity_type)
            converted_entities[entity_type] = URI
        elif entity_type in ['comparison', 'number']:
            converted_entities[entity_type] = entity_value.strip()
    attribute_joined = ' '.join(attributes)
    URI = search_for_uri_wiki(attribute_joined, 'attribute')
    converted_entities['attribute'] = URI

    mapping_to_templates(converted_entities, intent)
    print(converted_entities)
    print('-----------------------')


def search_for_uri_wiki(term, term_type):
    term = term.strip()
    rst = fuzzysearch_wiki.find_nearest_match(term, term_type)
    URI = rst[0]
    candidate = rst[1]
    # print(URI)
    print('\t CANDIDATE:', candidate)
    return URI


# def resolve_mismatches(q):
#     item_attribute_query = ['attribute', 'species']
#     batch_attribute_query = ['attribute', 'class']


with open('Wiki_SPARQL_Template', encoding='utf-8') as f:
    WIKI_TEMPLATES = json.loads(f.read())

with open('test_questions_classified.json') as f:
    questions_dict = json.loads(f.read())

wiki_intents = ['item_attribute_query', 'batch_attribute_query',
                'batch_attribute_query_numerical', 'about_query', 'batch_query', 'rank_query_list']

successful = []
total = []
current_intents = ['about_query']
current_intents = wiki_intents
# initiate the nlu models
interpreter = Interpreter.load('./models/nlu')  # load the wiki nlu models

original_question_dict = {}

for intent in questions_dict:
    # if intent in wiki_intents:
    if intent in current_intents:
        questions = questions_dict[intent]
        questions = sorted(questions)
        for q in questions:
            total.append(1)
            rst = process_questions(q)
            q = rst[0]
            original_q = rst[1]
            if intent in original_question_dict:  # create the dictionary for the questions,
                # without the labels but in their original form
                original_question_dict[intent].append(original_q)
            else:
                original_question_dict[intent] = [original_q]
            print(q)
            interpret_question(q)

print('TOTAL NUMBER', sum(total))
print('SUCCESSFUL', sum(successful))

with open('original_questions', 'w', encoding='utf-8') as f:
    for intent in original_question_dict:
        f.write('========================\n')
        f.write(intent + '\n')
        for q in original_question_dict[intent]:
            f.write(q + '\n')

    f.close()
