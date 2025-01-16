import datetime
import math
import os
import tarfile
from rasa_nlu.model import Interpreter
import json

import warnings


class TestIntent:

    def __init__(self):
        warnings.filterwarnings("ignore")
        path = '../rasa_default/models'
        files = os.listdir(path)
        paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
        file_name = max(paths, key=os.path.getctime)
        print(file_name)
        tf = tarfile.open(file_name)
        tf.extractall()

        self.interpreter = Interpreter.load('./nlu')
        with open('../questions.json') as f:
            self.questions = json.loads(f.read())['set']
        with open('query_result.json', 'w') as f:
            f.write('@' + str(datetime.datetime.now()) + '\n')
        with open('entity_recognition.json', 'w') as f:
            f.write('')

        self.entities_results = []

        self.direct_hit_rate = 0
        self.indirect_hit_rate = 0
        self.question_number = 0

    def test_question(self, question, label):
        self.question_number = self.question_number + 1
        # print(question)
        result = self.interpreter.parse(question)
        intent_ranking_first_3 = result['intent_ranking'][:3]

        if intent_ranking_first_3[0]['name'] == label:
            self.direct_hit_rate = self.direct_hit_rate + 1
        else:
            print('------ a wrong question -------')
            print(question)

        for intent in intent_ranking_first_3[-1:]:
            if label == intent['name']:
                self.indirect_hit_rate = self.indirect_hit_rate + 1

        with open('query_result.json', 'a') as f:
            f.write('--------------------------\n')
            f.write(question + '  !  ' + label + '\n')
            f.write(json.dumps(result, indent=4) + '\n')
        # TODO: make a file recording all the entity recognition.

        with open('entity_recognition.json', 'a') as f1:
            f1.write('-------------\n')
            f1.write(question + '\n')
            f1.write(json.dumps(result['entities'], indent=4))

        new_entities_result = \
            {'question': question, 'result': [{'value': x['value'], 'entity': x['entity']} for x in result['entities']]}
        self.entities_results.append(new_entities_result)

    def iterate_questions(self):
        for category in self.questions:
            label = category['label']
            for question in category['questions']:
                self.test_question(question, label)

    def run(self):
        self.iterate_questions()
        print('direct hit', self.direct_hit_rate, 'out of ', self.question_number)
        print('indirect hit', self.indirect_hit_rate, 'out of ', self.question_number)
        print('success rate', round(self.direct_hit_rate / self.question_number * 100, 2), '%')

a_test = TestIntent()
a_test.run()
with open('entity_recognition.json', 'w') as f:
    f.write(json.dumps(a_test.entities_results))
