import json
import os

from Marie.Util.location import DICTIONARY_DIR

ontology = "wikidata_numerical"
new_ontology = "wikidata_numerical_updated"

old_name_dict = json.loads(open(os.path.join(DICTIONARY_DIR, ontology, "name_dict.json")).read())
old_name_list = json.loads(open(os.path.join(DICTIONARY_DIR, ontology, "name_list.json")).read())



label_iri_dict = {"class": {"SPECIES": "species"} , "species": old_name_dict}
all_labels = old_name_list + ["species"]
type_dict = {"species": old_name_list, "class": ["species"]}



with open(os.path.join(DICTIONARY_DIR, new_ontology, "name_dict.json"), "w") as f:
    f.write(json.dumps(label_iri_dict))
    f.close()

with open(os.path.join(DICTIONARY_DIR, new_ontology, "name_list.json"), "w") as f:
    f.write(json.dumps(all_labels))
    f.close()

with open(os.path.join(DICTIONARY_DIR, new_ontology, "class_label_list.json"), "w") as f:
    f.write(json.dumps(["SPECIES"]))
    f.close()

with open(os.path.join(DICTIONARY_DIR, new_ontology, "type_dict.json"), "w") as f:
    f.write(json.dumps(type_dict))
    f.close()