import json

from CoordinateAgent import CoordinateAgent
from contextlib import redirect_stdout

ca = CoordinateAgent(None)
q_a_pairs = {}

with open('test_questions') as f:
    questions = f.readlines()

for q in questions:
    rst = ca.run(q)
    q_a_pairs[q] = rst


with open('test_result', 'w') as f:
    f.write(json.dumps(q_a_pairs))



