# import pce and stdc evaluation set
# 1. Evaluate accuracy of agent classification and ner, with mode "NLP_DEBUG"
import json
import os
from pprint import pprint
from CoordinateAgent import CoordinateAgent
from Evaluation_data.location import AGENT_EVALUATION_DIR

ca = CoordinateAgent()

# load dictionaries containing both the question and the correct results (NER)
# ====================================================================================
stdc_dict = json.loads(open(os.path.join(AGENT_EVALUATION_DIR, r'stdc_dictionary')).read())
thermo_agent_questions = [o['question'] for o in stdc_dict]
pce_dict = json.loads(open(os.path.join(AGENT_EVALUATION_DIR, r'pce_dictionary')).read())
pce_agent_questions = [o['question'] for o in pce_dict]
# ====================================================================================

# =====================================================================
# Evaluate the intent classification
def ner():
    for t_a_q in thermo_agent_questions:
        rst = ca.run(t_a_q)
        intent = rst['intent']['name']
        if intent != 'Thermo_Agent':
            print('============ WE HAVE A PROBLEM ==============')
            print(t_a_q)

    def make_ner_dict(entities):
        _dict = {}
        for e in entities:
            key = e['entity']
            value = e['value'].lower().strip()
            if key in _dict:
                _dict[key].append(value)
            else:
                _dict[key] = [value]
        return _dict

    def analyze_ner_pce(rst, real_result):
        # TP:
        TP = 0
        FP = 0
        FN = 0
        real_attribute = real_result['attribute'].lower().strip()
        real_species = real_result['species'].lower().strip()
        if real_species.endswith('-'):
            real_species = real_species[:-1]
        if real_species.endswith('+'):
            real_species = real_species[:-1]
        ner_dict = make_ner_dict(rst)
        if real_attribute not in ner_dict['attribute']:
            FN = FN + 1
            print('=================== FALSE NEGATIVE ===================')
            print('attribute', ner_dict['attribute'])
            print('real_attribute', real_attribute)

        if real_species not in ner_dict['species']:
            FN = FN + 1
            print('=================== FALSE NEGATIVE ===================')
            print('species', ner_dict['species'])
            print('real_species', real_species)

        for predicted_attribute in ner_dict['attribute']:
            if predicted_attribute == real_attribute:
                TP = TP + 1  # true positive
            else:
                FP = FP + 1  # false positive
                print('=================== FALSE POSITIVE ===================')
                print('attribute', predicted_attribute)
                print('real_attribute', real_attribute)

        for predicted_species in ner_dict['species']:
            if predicted_species == real_species:
                TP = TP + 1  # true positive
            else:
                FP = FP + 1  # false positive
                print('=================== FALSE POSITIVE ===================')
                print('species', predicted_species)
                print('real_species', real_species)

        return TP, FP, FN

    def analyze_ner_stdc(rst, real_result):
        # TP:
        TP = 0
        FP = 0
        FN = 0
        real_attribute = real_result['attribute'].lower().strip()
        real_species = real_result['species'].lower().strip()
        if real_species.endswith('-'):
            real_species = real_species[:-1]
        if real_species.endswith('+'):
            real_species = real_species[:-1]
        real_temperature = real_result['temperature'].lower().strip()
        real_pressure = real_result['pressure'].lower().strip()

        ner_dict = make_ner_dict(rst)
        if 'pressure' not in ner_dict:
            ner_dict['pressure'] = ['']
        if 'temperature' not in ner_dict:
            ner_dict['temperature'] = ['']

        if real_attribute not in ner_dict['attribute']:
            FN = FN + 1

        if real_species not in ner_dict['species']:
            FN = FN + 1

        if real_temperature not in ner_dict['temperature']:
            FN = FN + 1

        if real_pressure not in ner_dict['pressure']:
            FN = FN + 1

        for predicted_attribute in ner_dict['attribute']:
            if predicted_attribute == real_attribute:
                TP = TP + 1  # true positive
            else:
                FP = FP + 1  # false positive
                print('=================== FALSE POSITIVE ===================')
                print('attribute', predicted_attribute)
                print('real_attribute', real_attribute)

        for predicted_species in ner_dict['species']:
            if predicted_species == real_species:
                TP = TP + 1  # true positive
            else:
                FP = FP + 1  # false positive
                print('=================== FALSE POSITIVE ===================')
                print('species', predicted_species)
                print('real_species', real_species)

        for predicted_pressure in ner_dict['pressure']:
            if predicted_pressure == real_pressure:
                TP = TP + 1  # true positive
            else:
                if predicted_pressure != '':
                    FP = FP + 1  # false positive
                    print('=================== FALSE POSITIVE ===================')
                    print('predicted_pressure', predicted_pressure)
                    print('real_pressure', real_pressure)

        for predicted_temperature in ner_dict['temperature']:
            if predicted_temperature == real_temperature:
                TP = TP + 1  # true positive
            else:
                if real_temperature != '':
                    FP = FP + 1  # false positive
                    print('=================== FALSE POSITIVE ===================')
                    print('predicted_temperature', predicted_temperature)
                    print('real_temperature', real_temperature)

        return TP, FP, FN

    TOTAL_TP = 0
    TOTAL_FP = 0
    TOTAL_FN = 0

    for pce_obj in pce_dict:
        pce_question = pce_obj['question']
        rst = ca.run(pce_question)
        TP, FP, FN = analyze_ner_pce(rst['entities'], pce_obj)
        TOTAL_TP = TOTAL_TP + TP
        TOTAL_FP = TOTAL_FP + FP
        TOTAL_FN = TOTAL_FN + FN
        intent = rst['intent']['name']

    for stdc_obj in stdc_dict:
        stdc_question = stdc_obj['question']
        rst = ca.run(stdc_question)
        TP, FP, FN = analyze_ner_stdc(rst['entities'], stdc_obj)
        TOTAL_TP = TOTAL_TP + TP
        TOTAL_FP = TOTAL_FP + FP
        TOTAL_FN = TOTAL_FN + FN

    print('TOTAL_TP', TOTAL_TP)
    print('TOTAL_FP', TOTAL_FP)
    print('TOTAL_FN', TOTAL_FN)

    precision = TOTAL_TP / (TOTAL_TP + TOTAL_FP)
    recall = TOTAL_TP / (TOTAL_TP + TOTAL_FN)
    f1 = 2 * precision * recall / (precision + recall)

    print('precision', precision)
    print('recall', recall)
    print('f1', f1)


# ============================================================================

import json
from pprint import pprint
from CoordinateAgent import CoordinateAgent

with open('NLP results', 'w') as f:
    f.write('=======================')


thermo_result_dictionary = []

for taq in thermo_agent_questions:
    rst = ca.run(taq)
    tmp = {'question': taq, 'result': rst}
    thermo_result_dictionary.append(tmp)

# with open('thermo_result', 'w') as f:
#     f.write(json.dumps(thermo_result_dictionary, indent=4))
#     f.close()

pce_result_dictionary = []
for paq in pce_agent_questions:
    rst = ca.run(paq)
    tmp = {'question': paq, 'result': rst}
    pce_result_dictionary.append(tmp)
#
# with open('pce_result', 'w') as f:
#     f.write(json.dumps(pce_result_dictionary, indent=4))
#     f.close()