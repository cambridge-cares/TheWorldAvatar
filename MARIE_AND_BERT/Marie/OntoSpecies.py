from Marie.QAEngine import QAEngine


class OntoSpeciesQAEngine(QAEngine):

    def __init__(self, dataset_dir="CrossGraph/ontospecies", dataset_name="ontospecies", nel = None):
        super().__init__(dataset_dir, dataset_name, dim=80, nel=nel)


if __name__ == "__main__":
    my_engine = OntoSpeciesQAEngine(dataset_dir="CrossGraph/ontospecies", dataset_name="ontospecies")
    rst = my_engine.run("what is the smiles string of CO2")
    print(rst)
