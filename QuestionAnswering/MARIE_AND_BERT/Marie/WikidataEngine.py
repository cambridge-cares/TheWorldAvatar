from Marie.QAEngineNumerical import QAEngineNumerical


class WikidataEngine(QAEngineNumerical):
    """
        my_engine = QAEngineNumerical(dataset_dir="CrossGraph/wikidata_numerical", dataset_name="wikidata_numerical",
                                  embedding="transe",
                                  dict_type="json", dim=40, test=False)
    """

    def __init__(self, dataset_dir="CrossGraph/wikidata_numerical", dataset_name="wikidata_numerical", nel=None):
        super().__init__(dataset_dir, dataset_name, dim=40, test=False,
                         operator_dict={0: "smaller", 1: "larger", 2: "about", 3: "none"}, enable_class_ner=True,
                         ontology=dataset_name, nel=nel)

    def test(self, true_mention, true_rel, true_tails):
        """
        :param true_mention:
        :param true_rel:
        :param true_tails:
        :return:
        """
        pass


if __name__ == "__main__":
    from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
    cn = ChemicalNEL()
    my_engine = WikidataEngine(dataset_dir="CrossGraph/wikidata_numerical", dataset_name="wikidata_numerical", nel=cn)
    text = ""
    while text != "quit":
        text = input("Question:")
        rst = my_engine.run(text)
        print(rst)
    # rst = my_engine.run("what is the smiles string of CH4O4S", mention="C7H5NO")
    # print(rst)
    # rst = my_engine.run("what is the boiling point of C7H5NO", mention="C7H5NO")
    # print(rst)
    # rst = my_engine.run("find species with boiling point larger than 10 degrees")
    # print(rst)
    # rst = my_engine.run("find species with boiling point smaller than 10 degrees")
    # print(rst)
    # rst = my_engine.run("find species with boiling point around 100 degrees")
    # print(rst)
