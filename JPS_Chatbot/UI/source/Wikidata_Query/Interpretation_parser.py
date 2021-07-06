from pprint import pprint


class InterpretationParser:

    def __init__(self, socketio):
        self.interpreter = None
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
        self.socketio = socketio

    def parse_question_interpretation(self, question):
        print('question', question)
        result = self.interpreter.parse(question)
        print('============================================')
        print('Interpretation parser - 23')
        pprint(result)
        print('============================================')
        # get the key components and their types out
        # get the intent of the question
        intent = result['intent']['name']
        entities = result['entities']
        result = self.fill_in_components(intent, entities)
        return result



    def fill_in_components(self, intent, entities):
        pprint(entities)
        print('intent', intent)
        intent_list = list(self.entity_intent_map.keys())
        intent_list.remove(intent)
        intent_list.insert(0, intent)
        for candidate_intent in intent_list:
            obj = self.entity_intent_map[candidate_intent]
            perfect_match = True
            for entity in entities:
                entity_type = entity['entity']
                term = entity['value'].lower()
                if entity_type in obj:
                    slot = obj[entity_type]
                    if type(slot) == type([]):
                        print('the slot is a list ')
                        obj[entity_type].append(term)
                        # more than one term should present ...
                    else:
                        obj[entity_type] = term
                else:
                    print('There is a mismatch', candidate_intent)
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
