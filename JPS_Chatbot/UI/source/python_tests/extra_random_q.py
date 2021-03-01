import json

with open('random_questions') as f:
    questions = [x.replace('\n', '') for x in f.readlines()]
    print(questions)
