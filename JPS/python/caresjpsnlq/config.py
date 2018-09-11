import json
TAGGING_RECTIFIER_DICTIONARY = './Dictionaries/tagging_recitifier_dictionary.json'
DBPEDIA_PROPERTY_RECTIFIER_DICTIONARY = './Dictionaries/dbpeida_property_rectifier_dictionary.json'
KEY_WORD_RECTIFIER_DICTIONARY = './Dictionaries/key_word_rectifier_dictionary.json'

def read_file(file_path):
    with open(file_path) as file:
        return json.loads(file.read())