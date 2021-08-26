import json
import re

def remove_stuff(entity):
    stuff = ['[', ']', '(species)']
    for s in stuff:
        entity = entity.replace(s, '')

with open('test_questions_classified.json') as f:
    TEST_QUESTIOSN = json.loads(f.read())



re_get_species = r'\[[ = A-Z a-z 0-9 \- ]+\]\(species\)'
rst = re.findall(re_get_species, TEST_QUESTIOSN)
rst  = [remove_stuff(s) for s in rst]

print(rst)

with open('species_for_test_wiki', 'w') as f:
    f.write(json.dumps(rst))