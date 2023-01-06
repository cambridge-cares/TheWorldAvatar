from Marie.QAEngine import QAEngine


class WikidataEngine(QAEngine):

    def __init__(self, dataset_dir="CrossGraph/wikidata_single", dataset_name="wikidata_single"):
        super().__init__(dataset_dir, dataset_name, dim=40)


if __name__ == "__main__":
    my_engine = WikidataEngine(dataset_dir="CrossGraph/wikidata_single", dataset_name="wikidata_single")
    rst = my_engine.run("what is the smiles string of CH4")
    print(rst)
