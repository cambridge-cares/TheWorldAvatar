from pprint import pprint
from rasa.nlu.model import Interpreter


def make_confidence_map(result):
    intent_ranking = result['intent_ranking']
    rst = {}
    for intent_candidate in intent_ranking:
        intent = intent_candidate['name']
        confidence = intent_candidate['confidence']
        rst[intent] = confidence
    return rst


class InterpretationParser:

    def __init__(self, model_dir):
        self.interpreter = Interpreter.load(model_dir)
        self.entity_intent_map = {'attribute_batch_attribute_query_numerical': {'attribute': [],
                                                                                'class': None, 'comparison': None,
                                                                                'number': None},
                                  'batch_attribute_query_numerical': {'class': None, 'attribute': [],
                                                                      'comparison': None, 'number': None},
                                  'item_attribute_query': {'attribute': None, 'species': None},
                                  'batch_attribute_query': {'attribute': None, 'class': None},

                                  'rank_query_list': {'class': None, 'attribute': None},
                                  'batch_query': {'class': None},
                                  'about_query': {'species': None}
                                  }

    def parse_question_interpretation(self, question):
        result = self.interpreter.parse(question)
        # get the key components and their types out
        # get the intent of the question
        intent = result['intent']['name']
        entities = result['entities']
        confidence_map = make_confidence_map(result)
        result = self.fill_in_components(intent, confidence_map, entities)
        return result

    def fill_in_components(self, intent, confidence_map, entities):
        intent_list = list(self.entity_intent_map.keys())
        intent_list.remove(intent)
        intent_list.insert(0, intent)
        # if the intent confidence score is super high, cancel the fallback mechanism, make it fail

        for candidate_intent in intent_list:
            if confidence_map[candidate_intent] < 0.01:
                pass
            else:
                obj = self.entity_intent_map[candidate_intent]
                perfect_match = True
                for entity in entities:
                    entity_type = entity['entity']
                    term = entity['value'].lower()
                    if entity_type in obj:
                        slot = obj[entity_type]
                        if type(slot) == type([]):
                            obj[entity_type].append(term)
                            # more than one term should present ...
                        else:
                            obj[entity_type] = term
                    else:
                        # TODO: MISMATCH
                        # there is a mismatch, proceed to the next intent
                        perfect_match = False

                # batch_attribute_query_numerical must contain only one attribute
                # attribute_batch_attribute_query_numerical must contain two attributes
                if candidate_intent == 'batch_attribute_query_numerical':
                    if len(obj['attribute']) != 1:
                        perfect_match = False
                if candidate_intent == 'attribute_batch_attribute_query_numerical':
                    if len(obj['attribute']) != 2:
                        perfect_match = False

                for entity_type in obj:
                    if obj[entity_type] is None:
                        perfect_match = False

                if perfect_match:
                    return {'type': candidate_intent, 'entities': obj}
                else:
                    pass
