# read dictionaries from OntoMoPs, Wikidata, and ontospecies_new
# reverse the key-values
# and merge the dictionaries to create a global IRI to label dictionary label_dict.json
import json
import os

from Marie.Util.location import DICTIONARY_DIR, ROOT_DIR


class LabelDictCreator:

    def __init__(self):
        pass

    def get_wikidata(self):
        wikidata_dict_path = os.path.join(DICTIONARY_DIR, 'wikidata_numerical', 'name_dict.json')
        wikidata_dict = {v[0]: k for k, v in json.loads(open(wikidata_dict_path).read()).items()}
        return wikidata_dict

    def get_ontomops(self):
        ontomops_dict_path = os.path.join(DICTIONARY_DIR, 'OntoMoPs', 'name_dict.json')
        ontomops_dict = json.loads(open(ontomops_dict_path).read())
        full_ontomops_dict = {}
        for main_key in ['gbus', 'cbus', 'ams']:
            temp_dict = {}
            for key, name_list in ontomops_dict[main_key].items():
                temp_dict.update({n: key for n in name_list})
            full_ontomops_dict.update(temp_dict)
        return full_ontomops_dict

    def get_ontospecies_new(self):
        ontospecies_dict_path = os.path.join(DICTIONARY_DIR, 'ontospecies_new', 'name_dict.json')
        ontospecies_dict = {v[0]: k for k, v in json.loads(open(ontospecies_dict_path).read())["species"].items()}
        return ontospecies_dict

    def run(self):
        label_dict = {}
        label_dict.update(self.get_wikidata())
        label_dict.update(self.get_ontomops())
        label_dict.update(self.get_ontospecies_new())
        with open(os.path.join(ROOT_DIR, 'static', 'js', 'label_dict.js'), 'w') as f:
            f.write(f"let global_label_dict = {json.dumps(label_dict)}")
            f.close()


if __name__ == "__main__":
    my_creator = LabelDictCreator()
    my_creator.run()
