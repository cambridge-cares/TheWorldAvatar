from rasa.nlu.model import Interpreter

from LDA.LDA_classifier import LDAClassifier

with open('../../../../chatbot-log.txt') as f:
    questions = f.readlines()


interpreter = Interpreter.load('./models/nlu')  # load the wiki nlu models

lda_classifier = LDAClassifier()

for question in questions:
    result = interpreter.parse(question)
    intent = result['intent']
    entities = [(e['entity'], e['value']) for e in result['entities']]
    topics = lda_classifier.classify(question)

    print('==========================================')
    print('# ', question.replace('\n',''))
    print('# ', topics)
    print('# ')
    print('# ----------------------')
    print('# ', str(intent))
    print('# ')
    print('# ', str(entities))
    print('# ==========================================\n')