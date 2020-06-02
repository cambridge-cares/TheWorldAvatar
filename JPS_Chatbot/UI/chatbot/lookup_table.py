# this class processes the query results obtained from wikidata
import json
from pprint import pprint


class LookUpTableGenerator:

    def __init__(self):
        self.directory = './corpus'
        self.global_list = []
        self.lookup_table = {}
        self.dictionary = []

    def generate_property_lookup_table(self, topic):

        with open(self.directory + '/%s_altLabel.json' % topic, errors='ignore') as f:
            properties_json = json.loads(f.read()) #.encode('utf-8', 'ignore').decode('utf-8', 'ignore')
            f.close()

        for _property in properties_json:
            label = _property['label']
            alt_label_list = [l.strip() for l in _property['altLabel_list'].split(', ') if l is not '']
            uri = _property['uri']

            self.append_new_uri(label, uri)
            for altL in alt_label_list:
                self.append_new_uri(altL, uri)

            # for each word, we create a list of URIs, therefore, it is possible that a word accords to multiple URIs.

    def append_new_uri(self, word, uri):
        self.dictionary.append(word)
        if word not in self.lookup_table:
            # create a new list to store URIs
            self.lookup_table[word] = [uri]
        else:
            self.lookup_table[word].append(uri)


topics = ['physical_properties', 'chemistry_properties', 'process_properties', 'chemical_compound_entity','chemical_substance_class']
for topic in topics:
    generator = LookUpTableGenerator()
    generator.generate_property_lookup_table(topic)
    with open('./lookup_tables/%s.json' % topic, 'w') as f:
        f.write(json.dumps(generator.lookup_table))
    with open('./lookup_tables/%s_dictionary.json' % topic, 'w') as f1:
        f1.write(json.dumps(generator.dictionary))
