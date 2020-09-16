# This files contains your custom actions which can be used to run
# custom Python code.
#
# See this guide on how to implement these action:
# https://rasa.com/docs/rasa/core/actions/#custom-actions/


# This is a simple example for a custom action which utters "Hello World!"

from typing import Any, Text, Dict, List

from rasa_sdk import Action, Tracker
from rasa_sdk.executor import CollectingDispatcher


class ActionHelloWorld(Action):

    def __init__(self):
        self.the_class = ''
        self.the_entity = ''
        self.the_attribute = ''

    def name(self) -> Text:
        return "batch_restriction_query"

    def run(self, dispatcher: CollectingDispatcher,
            tracker: Tracker,
            domain: Dict[Text, Any]) -> List[Dict[Text, Any]]:
        entities = (tracker.latest_message["entities"])
        print('entities', entities)

        for entity in entities:
            if entity['entity'] == 'class':
                self.the_class = entity['value']
            elif entity['entity'] == 'entity':
                self.the_entity = entity['value']
            elif entity['entity'] == 'attribute':
                self.the_attribute = entity['value']
            else:
                pass

        dispatcher.utter_message(self.construct_query())

        return []

    def construct_query(self):
        return """
        SELECT ?entities {
            ?entities a <%s> .
            ?entities <%s> <%s> .
            
        }
        """ % (self.the_class, self.the_attribute, self.the_entity)
