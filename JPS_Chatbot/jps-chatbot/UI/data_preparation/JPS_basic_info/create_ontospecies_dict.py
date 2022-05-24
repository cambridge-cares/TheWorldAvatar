import json
import os

from location import TRAINING_FILES_DIR

filepath = os.path.join(TRAINING_FILES_DIR, 'ONTOSPECIES_IDENDITIFERS_QUERY_RESULT')

every_term = []
ontospecies_dict = {}


def split_terms(variable_name, _binding):
    terms = _binding[variable_name]['value']
    if '; ' in terms:
        terms = terms.split('; ')
    else:
        terms = [terms]
    return terms

counter = 0
with open(filepath) as f:
    OntoSpecies_Identifiers = json.loads(f.read())['results']['bindings']
    for binding in OntoSpecies_Identifiers:
        counter += 1
        print(counter, 'out of', len(OntoSpecies_Identifiers))
        # print(binding)
        IRI = binding['species']['value']
        labels = split_terms('labels', binding)
        smiles = split_terms('smiles', binding)
        inChIs = split_terms('inChIs', binding)
        alt_labels = split_terms('alt_labels', binding)
        everything_in_this_binding = list(set(labels + smiles + inChIs + alt_labels))
        for _term in everything_in_this_binding:
            if _term not in ontospecies_dict:
                ontospecies_dict[_term] = [IRI]
            else:
                ontospecies_dict[_term].append(IRI)

        every_term = every_term + everything_in_this_binding

every_term = list(set(every_term))

# write the dictionary
with open(os.path.join(TRAINING_FILES_DIR, 'ONTOSPECIES_URI_DICT'), 'w') as f:
    f.write(json.dumps(ontospecies_dict))
    f.close()

with open(os.path.join(TRAINING_FILES_DIR, 'ONTOSPECIES_KEYS'), 'w') as f:
    f.write(json.dumps(every_term))
    f.close()








