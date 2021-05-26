class InterpretationParser:

    def __init__(self, socketio):
        self.interpreter = None
        self.entity_intent_map = {'item_attribute_query': {'attribute': None, 'entity': None},
                                  'batch_restriction_query': {'attribute': None, 'class': None},
                                  'batch_restriction_query_numerical': {'class': None, 'attribute': None,
                                                                        'comparison': None, 'numerical_value': None},
                                  'batch_restriction_query_numerical_and_attribute': {'attribute': [],
                                                                                      'class': None, 'comparison': None,
                                                                                      'numerical_value': None}
                                  }
        self.socketio = socketio

    def parse_question_interpretation(self, question):
        result = self.interpreter.parse(question)
        # get the key components and their types out
        # get the intent of the question
        intent = result['intent']['name']
        entities = result['entities']
        result = self.fill_in_components(intent, entities)
        return result

    def fill_in_components(self, intent, entities):
        obj = self.entity_intent_map[intent]
        for entity in entities:
            entity_type = entity['entity']
            term = entity['value'].lower()

            slot = obj[entity_type]
            if type(slot) is list:
                obj[entity_type].append(term)
                # more than one term should present ...
            else:
                obj[entity_type] = term
        return {'type': intent, 'entities': obj}
