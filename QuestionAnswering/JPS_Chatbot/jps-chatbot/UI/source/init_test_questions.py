import json
import time
from pprint import pprint

from CoordinateAgent import CoordinateAgent

ca = CoordinateAgent()
qa_pairs = {}
with open('test_questions') as f:
    questions = f.readlines()

# q = 'chemical structure of benzene'
#
# rst = ca.run(q)
# print(rst)

for q in questions:
    time.sleep(10)
    rst = ca.run(q)
    while rst == 'Nothing':
        rst = ca.run(q)
    if type(rst) == type([]):
        rst = rst[0]
    qa_pairs[q] = rst
    time.sleep(10)

with open('test_result', 'w') as f:
    f.write(json.dumps(qa_pairs))
    f.close()

print('============================================')
pprint(qa_pairs)
