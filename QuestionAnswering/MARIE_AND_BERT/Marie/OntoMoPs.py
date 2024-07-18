import os, sys
from pprint import pprint

sys.path.append("..")
from Marie.QAEngineNumerical import QAEngineNumerical
from Marie.Util.location import DATA_DIR


class OntoMoPsQAEngine(QAEngineNumerical):

    def __init__(self, dataset_dir="CrossGraph/OntoMoPs/numerical_with_implicit",
                 dataset_name='numerical_with_implicit',
                 sub_ontology='numerical_with_implicit',
                 ontology='OntoMoPs',
                 nel=None):
        self.dataset_dir = dataset_dir
        self.ontology = ontology
        self.sub_ontology = sub_ontology

        super().__init__(dataset_dir, dataset_name, dim=50, test=False, value_dict_name="node_value_dict.json",
                         seperated_model=True, operator_dict={0: "smaller", 1: "larger", 2: "none"},
                         enable_class_ner=True, ontology=ontology,
                         numerical_scale_factor=1000, mode="transr", nel=nel)


if __name__ == "__main__":
    from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
    cn = ChemicalNEL()
    ontology = "OntoMoPs"
    sub_ontology = "numerical_with_implicit"
    dataset_dir = os.path.join(DATA_DIR, f"CrossGraph/{ontology}/{sub_ontology}")
    my_engine = OntoMoPsQAEngine(dataset_dir=dataset_dir, dataset_name=sub_ontology, sub_ontology=sub_ontology,
                                 ontology=ontology, nel=cn)

    text = ""
    while text != "quit":
        text = input("QUESTION: ")
        rst = my_engine.run(text)
        print("============================= RESULT ==============================")
        pprint(rst)
        print("===================================================================")
    # text = "List the Chemical Building Units with 2-linear as the Generic Building Unit"
    # rst = my_engine.run(text)
    # print(rst)

    # text = "List the MOPs with (3-pyramidal)8(2-bent)12(Cs) as the assembly model"
    # rst = my_engine.run(text)
    # print(rst)
    #
    # text = "MoPs with molecular weight more than 10"
    # rst = my_engine.run(text)
    # print(rst)
    #
    # text = "MoPs with molecular weight less than 100"
    # rst = my_engine.run(text)
    # print(rst)
