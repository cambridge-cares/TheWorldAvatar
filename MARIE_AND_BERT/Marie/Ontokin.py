from Marie.QAEngine import QAEngine


class OntoKinQAEngine(QAEngine):

    def __init__(self, dataset_dir="CrossGraph/ontokin", dataset_name="ontokin"):
        super().__init__(dataset_dir, dataset_name)


if __name__ == "__main__":
    my_engine = OntoKinQAEngine(dataset_dir="CrossGraph/ontokin", dataset_name="ontokin")
    rst = my_engine.run("what is the chemical formula of CO2")
    print(rst)
