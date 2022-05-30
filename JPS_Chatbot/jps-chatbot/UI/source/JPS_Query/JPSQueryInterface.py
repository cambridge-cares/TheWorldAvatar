import json
from .chatbot_interface import Chatbot


class JPSQueryInterface:

    def __init__(self, model_dir):
        self.jps_interface = Chatbot(None)

    def jps_query(self, question):

        result = self.jps_interface.analyse_questions(question)
        if result is None:
            return None
        if 'result' in result:
            result_obj = json.loads(result)
            result_list = result_obj['result']
            if len(result_list) == 0:
                pass
            else:
                if 'http://localhost:8080/ldfserver/Empty' in result and len(result_list) == 1:
                    pass
                else:
                    return result_list
 