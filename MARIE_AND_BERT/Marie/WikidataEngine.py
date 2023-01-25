from Marie.QAEngineNumerical import QAEngineNumerical


class WikidataEngine(QAEngineNumerical):
    """
        my_engine = QAEngineNumerical(dataset_dir="CrossGraph/wikidata_numerical", dataset_name="wikidata_numerical",
                                  embedding="transe",
                                  dict_type="json", dim=40, test=False)
    """
    def __init__(self, dataset_dir="CrossGraph/wikidata_numerical", dataset_name="wikidata_numerical"):
        super().__init__(dataset_dir, dataset_name, dim=40, test=False)


if __name__ == "__main__":
    my_engine = WikidataEngine(dataset_dir="CrossGraph/wikidata_numerical", dataset_name="wikidata_numerical")
    # rst = my_engine.run("what is the smiles string of CH4O4S")
    # print(rst)
    rst = my_engine.run("what is the boiling point of C7H5NO", mention="C7H5NO")
    print(rst)
    # rst = my_engine.run("find species with boiling point below 10 degrees")
    # print(rst)
