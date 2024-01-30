import os, sys

sys.path.append("..")
from Marie.QAEngineNumerical import QAEngineNumerical
from Marie.Util.location import DATA_DIR


class OntoSpeciesNew(QAEngineNumerical):

    def __init__(self, dataset_dir='CrossGraph/ontospecies_new/base_full_no_pref_selected_role_limited_100',
                 dataset_name='base_full_no_pref_selected_role_limited_100',
                 sub_ontology='base_full_no_pref_selected_role_limited_100', ontology="ontospecies_new",
                 operator_dict={0: "smaller", 1: "larger", 2: "none"}, nel=None):
        self.dataset_dir = dataset_dir
        self.ontology = ontology
        self.sub_ontology = sub_ontology
        super().__init__(dataset_dir, dataset_name, dim=100, test=False, value_dict_name="node_value_dict.json",
                         seperated_model=True, operator_dict=operator_dict,
                         enable_class_ner=True, ontology=ontology,
                         numerical_scale_factor=1000, bert_tokenizer_name="bert-base-uncased", mode="transr",
                         nel=nel)


if __name__ == "__main__":
    ontology = "ontospecies_new"
    sub_ontology = "base_full_no_pref_selected_role_limited_100"
    dataset_dir = os.path.join(DATA_DIR, f"CrossGraph/{ontology}/{sub_ontology}")
    my_engine = OntoSpeciesNew(dataset_dir=dataset_dir, dataset_name=sub_ontology, sub_ontology=sub_ontology,
                               ontology=ontology)

    # question_list = ["what is the melting point of CH4",
    #                  "find species with molecular weight less than 100",
    #                  "find species with molecular weight over 100",
    #                  "what is the boiling point of Metanol"]

    question_list = ["find species with molecular weight over 100", "Find all species with boiling point above 0 celsius", "What are the chemical species having molecular weight around 150 g/mol"]

    for question in question_list:
        print(my_engine.run(question))
