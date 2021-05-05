import json

from rasa.nlu.model import Interpreter

from LDA.LDA_classifier import LDAClassifier

with open('test_questions_classified.json') as f:
    questions_object = json.loads(f.read())


interpreter = Interpreter.load('./models/nlu')  # load the wiki nlu models

lda_classifier = LDAClassifier()
total_counter  = 0
bad_counter = 0
for question_type in questions_object:
    questions = questions_object[question_type]
    for question in questions:
        total_counter = total_counter + 1
        question = question.lower()
        result = interpreter.parse(question)
        intent = result['intent']['name']
        entities = [(e['entity'], e['value']) for e in result['entities']]
        topics = lda_classifier.classify(question)
        if intent in question_type:
            bad_counter = bad_counter + 1
            print('==========================================')
            print('# ', question.replace('\n',''))
            print('# ', topics)
            print('# ')
            print('# ----------------------')
            print('# ', str(result['intent_ranking']))
            print('# ')
            print('# ', str(entities))
            print('# ==========================================\n')

print('total questions', total_counter)
print('bad questions', bad_counter)
print(round((1 - bad_counter/total_counter) * 100, 2))