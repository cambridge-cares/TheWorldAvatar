# make a dictionary on stuff we have
import itertools
import json
import os

from KGToolbox.Tools.IntegratedTrainingFileCreator import IntegratedTrainingFileCreator
from KGToolbox.OntoMoPs.OntoMoPsSPARQLWarehouse import GBU_LABEL_QUERY, CBU_LABEL_QUERY, AMS_SHAPE_LABEL_QUERY
from KGToolbox.Tools.GeneralTools import split_iri, update_dict_with_list
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR

ontology = "OntoMoPs"
sub_ontology = "numerical_with_implicit"
dataset_dir = os.path.join(DATA_DIR, "CrossGraph", ontology)
sub_ontology_dataset_dir = os.path.join(dataset_dir, "numerical_with_implicit")

# raw_data_path = os.path.join(dataset_dir, "classes_species_use_mops.jsonl")
dictionary_dir = os.path.join(DATA_DIR, "Dictionaries", ontology)
file_loader = FileLoader(full_dataset_dir=sub_ontology_dataset_dir, dataset_name=sub_ontology)
entity2idx, idx2entity, rel2idx, idx2rel = file_loader.load_index_files()
triples = file_loader.load_all_triples()
integrated_file_creator = IntegratedTrainingFileCreator(sparql_namespace="ontomops",
                                                        endpoint_url="http://kg.cmclinnovations.com:81/blazegraph_geo",
                                                        ontology=ontology, sub_ontology=sub_ontology)

label_am_dict = json.loads(open(os.path.join(dataset_dir, "label_am_dict.json")).read())
label_am_dict = {k.upper(): v for k, v in label_am_dict.items()}

QUERY_DICT = {"cbus": CBU_LABEL_QUERY, "gbus": GBU_LABEL_QUERY, "ams": AMS_SHAPE_LABEL_QUERY}

def create_label_dict(keyword):
    base_url = "http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#"
    label_list = []
    label_dict = {}
    QUERY = QUERY_DICT[keyword]
    bindings = integrated_file_creator.query_blazegraph(QUERY)["results"]["bindings"]
    for binding in bindings:
        label = binding["label"]["value"].replace(base_url, "").upper()
        iri = split_iri(binding["target"]["value"])
        label_list.append(label)
        label_dict = update_dict_with_list(key=label, value=iri, dictionary=label_dict)
    label_list = list(set(label_list))

    return label_list, label_dict


def create_class_label_set():
    # ams, cbus, mops
    class_label_dict = {"mops": ["MOPS", "MOP"], "ams": ["ASSEMBLY MODEL", "AMS", "AM"],
                        "cbus": ["CHEMICAL BUILDING UNITS", "CBU", "CBUS"],
                        "gbus": ["GBU", "GENERIC BUILDING UNIT", "GBUS"]}
    class_label_list = list(set(itertools.chain.from_iterable([v for k, v in class_label_dict.items()])))
    label_class_dict = {}
    for class_key, label_list in class_label_dict.items():
        for label in label_list:
            label_class_dict[label] = class_key

    with open(os.path.join(dictionary_dir, "label_class_dict.json"), "w") as f:
        f.write(json.dumps(label_class_dict))
        f.close()

    with open(os.path.join(dictionary_dir, "class_label_list.json"), "w") as f:
        f.write(json.dumps(class_label_list))
        f.close()

    return class_label_list, label_class_dict


keywords = ["cbus", "gbus", "ams"]
class_label_list, label_class_dict = create_class_label_set()
gbu_label_list, label_gbu_dict = create_label_dict("gbus")
cbu_label_list, label_cbu_dict = create_label_dict("cbus")
ams_shape_label_list, label_ams_shape_dict = create_label_dict("ams")
am_label_list, label_am_dict = [k.upper() for k in list(set(label_am_dict.keys()))], label_am_dict
label_am_dict.update(label_ams_shape_dict)
am_label_list += ams_shape_label_list

global_name_list = list(set(class_label_list + gbu_label_list + cbu_label_list + am_label_list))
global_type_dict = {"class": class_label_list, "gbus": gbu_label_list, "cbus": cbu_label_list, "ams": am_label_list}
global_name_dict = {"class": label_class_dict, "gbus": label_gbu_dict, "cbus": label_cbu_dict, "ams": label_am_dict}

with open(os.path.join(dictionary_dir, "name_list.json"), "w") as f:
    f.write(json.dumps(global_name_list))
    f.close()

with open(os.path.join(dictionary_dir, "name_dict.json"), "w") as f:
    f.write(json.dumps(global_name_dict))
    f.close()

with open(os.path.join(dictionary_dir, "type_dict.json"), "w") as f:
    f.write(json.dumps(global_type_dict))
    f.close()
