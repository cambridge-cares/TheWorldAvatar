import json, re, time, random

selected_instances = {}

with open('URI_SMILES_DICT') as f:
    URI_SMILES_DICT = json.loads(f.read())
    f.close()

with open('WIKI_URI_LIST') as f:  # this LIST contains 120,000+ instances
    WIKI_URI_LIST = json.loads(f.read())
    f.close()

no_label_instances = []
broken_instances = []
print(len(URI_SMILES_DICT))
print(len(WIKI_URI_LIST))

NAME_URI_DICT = {}
FORMULA_URI_DICT = {}
FORMULA_NAME_DICT = {}

def process_key(key):
    key = key.strip().upper()
    subscript = str.maketrans("₀₁₂₃₄₅₆₇₈₉", "0123456789")
    key = key.translate(subscript)
    return key


def get_label_and_alt_label_and_formula(_instance):
    _id = _instance.replace('http://www.wikidata.org/entity/', '')
    with open('./instance_info/%s' % _id) as f:
        raw_content = json.loads(f.read())['results']['bindings']

        if len(raw_content) <= 0:
            pass
        else:
            content = raw_content[0]
            if 'label' in content:
                label = content['label']['value']
                label = process_key(label)
                if label in NAME_URI_DICT:
                    if _instance not in NAME_URI_DICT[label]:
                        NAME_URI_DICT[label].append(_instance)
                else:
                    NAME_URI_DICT[label] = [_instance]

            if 'altLabel_list' in content:
                alt_label_string = content['altLabel_list']['value']
                if alt_label_string == '':
                    alt_labels = []
                else:
                    alt_labels = alt_label_string.split('$')
                    for l in alt_labels:
                        l = process_key(l)
                        if l in NAME_URI_DICT:
                            if _instance not in NAME_URI_DICT[l]:
                                NAME_URI_DICT[l].append(_instance)
                        else:
                            NAME_URI_DICT[l] = [_instance]

            if 'formula' in content:
                formula = content['formula']['value']
                formula = process_key(formula)

                if 'label' in content:
                    if formula in FORMULA_NAME_DICT:
                        FORMULA_NAME_DICT[formula].append(process_key(label))
                    else:
                        FORMULA_NAME_DICT[formula] = [process_key(label)]

                if formula in FORMULA_URI_DICT:
                    FORMULA_URI_DICT[formula].append(_instance)
                else:
                    FORMULA_URI_DICT[formula] = [_instance]

        # formula to URIs
        # SMILES to URIs
        # name to URIs

        f.close()


counter = 0
# URI_SMILES_DICT should be used as the root list for
for instance in WIKI_URI_LIST:
    # print(instance)
    # get the following properties of the instance
    # 1. SMILES, which comes with the URI_LIST
    # 2. Formula, which comes with the file in instance_info
    # 3. Labels and Alt_names, which comes with the file in instance_info
    SMILES = URI_SMILES_DICT[instance]  # get the SMILES of the
    get_label_and_alt_label_and_formula(instance)
    counter = counter + 1
    print(counter, 'out of', len(WIKI_URI_LIST))

print('broken_instances', len(broken_instances))
print('no_label_instances', len(no_label_instances))

with open('NAME_URI_DICT', 'w') as f:
    f.write(json.dumps(NAME_URI_DICT))
    f.close()

with open('FORMULA_URI_DICT', 'w') as f:
    f.write(json.dumps(FORMULA_URI_DICT))
    f.close()

with open('FORMULA_NAME_DICT', 'w') as f:
    f.write(json.dumps(FORMULA_NAME_DICT))
    f.close()