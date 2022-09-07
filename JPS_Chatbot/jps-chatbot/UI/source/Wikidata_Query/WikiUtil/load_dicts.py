import json
if __name__ == '__main__':
    from location import WIKI_DICT_DIR
else:
    from .location import WIKI_DICT_DIR

FOLDER = './WIKI_DICTS'


def process_species(species):
    return species.replace('#', 'HASH').replace(".", "DOT").replace('=', 'EQUAL')


def process_species_reversed(species):
    return species.replace('HASH', '#').replace("DOT", ".").replace('EQUAL', '=')


with open(WIKI_DICT_DIR + '/SMILES_URI_DICT') as f:
    SMILES_URI_DICT = json.loads(f.read())
    f.close()

with open(WIKI_DICT_DIR + '/NAME_URI_DICT') as f:
    NAME_URI_DICT = json.loads(f.read())
    f.close()

with open(WIKI_DICT_DIR + '/FORMULA_URI_DICT') as f:
    FORMULA_URI_DICT = json.loads(f.read())
    f.close()

with open(WIKI_DICT_DIR + '/ATTRIBUTE_URI_DICT') as f:
    ATTRIBUTE_URI_DICT = json.loads(f.read())
    f.close()

with open(WIKI_DICT_DIR + '/CLASS_URI_DICT') as f:
    CLASS_URI_DICT = json.loads(f.read())
    f.close()

NAME_KEYS = [process_species(n) for n in NAME_URI_DICT.keys()]
SMILES_KEYS = [process_species(n) for n in SMILES_URI_DICT.keys()]
FORMULA_KEYS = [process_species(n) for n in FORMULA_URI_DICT.keys()]
ATTRIBUTE_KEYS = ATTRIBUTE_URI_DICT.keys()
CLASS_KEYS = CLASS_URI_DICT.keys()
