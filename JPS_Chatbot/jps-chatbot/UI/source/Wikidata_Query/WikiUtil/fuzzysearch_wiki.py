from rapidfuzz import process, fuzz

if __name__ == '__main__':
    from load_dicts import FORMULA_URI_DICT, SMILES_URI_DICT, NAME_URI_DICT, \
        FORMULA_KEYS, NAME_KEYS, SMILES_KEYS, ATTRIBUTE_URI_DICT, \
        ATTRIBUTE_KEYS, CLASS_URI_DICT, CLASS_KEYS, process_species, process_species_reversed

else:
    from .load_dicts import FORMULA_URI_DICT, SMILES_URI_DICT, NAME_URI_DICT, \
        FORMULA_KEYS, NAME_KEYS, SMILES_KEYS, ATTRIBUTE_URI_DICT, \
        ATTRIBUTE_KEYS, CLASS_URI_DICT, CLASS_KEYS, process_species, process_species_reversed


def find_nearest_match(entity_value, entity_type):
    # rst = URI, score, candidate
    if entity_type == 'attribute':
        rst = find_nearest_match_in_attributes(entity_value)
    elif entity_type == 'class':
        rst = find_nearest_match_classes(entity_value)
    elif entity_type == 'species':
        rst = find_nearest_match_species(entity_value)
    URI = [u.replace('http://www.wikidata.org/entity/', '') for u in rst[0]]
    score = rst[1]
    # TODO: increase the threshold
    if score < 75:
        return None
    candidate = rst[2]
    return URI, candidate


def find_nearest_match_classes(_class):
    _class = _class.upper()
    KEYS = CLASS_KEYS
    DICT = CLASS_URI_DICT
    if _class not in DICT:
        rst = process.extractOne(_class, KEYS, scorer=fuzz.ratio)
        candidate = rst[0]
        score = rst[1]
        URI = DICT[candidate]
    else:
        score = 100
        candidate = _class
        URI = DICT[candidate]
    return URI, score, candidate

def find_nearest_match_species(species):
    species = process_species(species)
    KEYS_LIST = [FORMULA_KEYS, SMILES_KEYS, NAME_KEYS]
    DICT_LIST = [FORMULA_URI_DICT, SMILES_URI_DICT, NAME_URI_DICT]
    LABELS = ['FORMULA', 'SMILE', 'NAME']
    highest_score = 0
    best_uri = []
    best_label = ''
    best_candidate = ''
    for KEYS, DICTS, LABEL in zip(KEYS_LIST, DICT_LIST, LABELS):
        rst = find_nearest_match_in_one_species(species, KEYS, DICTS)
        URIS = rst[0]
        score = rst[1]
        candidate = rst[2]
        if score > highest_score:
            best_uri = URIS
            best_label = LABEL
            highest_score = score
            best_candidate = candidate
    return best_uri, highest_score, process_species_reversed(best_candidate), best_label


def find_nearest_match_in_one_species(species, KEYS, DICT):
    if species not in DICT:
        rst = process.extractOne(species, KEYS, scorer=fuzz.ratio)
        candidate = process_species_reversed(rst[0])
        score = rst[1]
        URI = DICT[candidate]
    else:
        score = 100
        candidate = process_species_reversed(species)
        URI = DICT[species]
    return URI, score, candidate


# it is exactly like the one for species
def find_nearest_match_in_attributes(attribute):
    attribute = attribute.upper()
    KEYS = ATTRIBUTE_KEYS
    DICT = ATTRIBUTE_URI_DICT
    if attribute not in DICT:
        rst = process.extractOne(attribute, KEYS, scorer=fuzz.ratio)
        candidate = rst[0]
        score = rst[1]
        URI = DICT[candidate]
    else:
        score = 100
        candidate = attribute
        URI = DICT[candidate]
    return URI, score, candidate
