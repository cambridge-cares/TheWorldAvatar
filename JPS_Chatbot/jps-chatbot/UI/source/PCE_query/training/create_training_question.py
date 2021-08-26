import json
import random

with open('pce_smiles') as f:
    pce_smiles = json.loads(f.read())
    f.close()

question_block = '## intent: pce_agent \n'

attributes = ['pce', 'power conversion efficiency']
template = ' - [%s](attribute) of [%s](species) \n'

for s in pce_smiles:
    a = random.choice(attributes)
    q = template % (a, s)
    question_block = question_block + q

with open('data/nlu.md', 'w') as f:
    f.write(question_block)
    f.close()











# with open('FORMULA_URI_DICT') as f:
#     FORMULA_URI_DICT = json.loads(f.read())
#
#
# with open('SMILES_URI_DICT') as f:
#     SMILES_URI_DICT = json.loads(f.read())
#
#
# with open('FORMULA_URI_DICT') as f:
#     FORMULA_URI_DICT = json.loads(f.read())
