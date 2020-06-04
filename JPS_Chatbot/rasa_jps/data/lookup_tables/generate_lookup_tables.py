import json
import os.path

from .locations import RASA_JPS_DATA_DIR

with open(os.path.join(RASA_JPS_DATA_DIR,'species.json'), encoding="utf8") as f:
    species = f.read()

SUB = str.maketrans("₀₁₂₃₄₅₆₇₈₉", "0123456789")
species = json.loads(species.translate(SUB))
list_of_names = []

for result in species:
    label = result['propertyLabel']
    list_of_names.append(label.strip())
    alt_label = list(set(result['altLabel_list'].split('$ ')))
    # list_of_names = list_of_names + alt_label
    formula = result['formula'].translate(SUB)
    list_of_names.append(formula.strip())

list_of_names = list(set(list_of_names))
temp = [x.lower() for x in list_of_names if x != '']
list_of_names = list_of_names + temp

with open(os.path.join(RASA_JPS_DATA_DIR,'species.txt'), 'w' ,encoding="utf8") as f:
    for name in list_of_names[1:]:
        if name != '':
            f.write(name + '\n')
    f.close()



# TODO: get label, alter label, formula
# TODO: make lower case version as well ...