import json
import random

# pce_csv_lines = open('pce.csv').readlines()[1:]
#
# selected = open('selected').readlines()
#
# pce_questions = []
# for pce_csv in pce_csv_lines:
#     line = pce_csv.strip().replace('"','').split(',')
#     attribute, species, question = line
#     new_s = random.choice(selected).strip()
#     new_question = question.replace(species, new_s).strip()
#     new_obj = {'question': new_question.strip(), 'attribute': attribute.strip(), 'species': new_s.strip()}
#     pce_questions.append(new_obj)
#
# with open('pce_dictionary', 'w') as f:
#     f.write(json.dumps(pce_questions, indent=4))
#     f.close()

stdc_csv_lines = open('stdc.csv').readlines()[1:]
selected = open('selected').readlines()

stdc_questions = []
for stdc_csv in stdc_csv_lines:
    line = stdc_csv.strip().replace('"','').split(',')
    print(len(line))
    attribute, species, temperature, pressure, question = line
    new_s = random.choice(selected).strip()
    new_question = question.replace(species, new_s).strip()
    new_obj = {'question': new_question.strip(), 'attribute': attribute.strip(), 'species': new_s.strip(),
               'temperature': temperature.strip(), 'pressure': pressure.strip()}
    stdc_questions.append(new_obj)

with open('stdc_dictionary', 'w') as f:
    f.write(json.dumps(stdc_questions, indent=4))
    f.close()

