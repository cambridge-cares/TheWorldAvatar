from KGToolbox.NHopExtractor import HopExtractor
from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
from Marie.Util.Models.TransEScoreModel import TransEScoreModel


class OntoKinQAEngine:

    def __init__(self):
        self.model_name = "bert_ontokin"
        self.dataset_dir = "CrossGraph/ontokin"
        self.score_model = TransEScoreModel(device="cpu",
                                            model_name=self.model_name,
                                            dataset_dir=self.dataset_dir,
                                            dim=80)

        self.subgraph_extractor = HopExtractor(dataset_dir=self.dataset_dir, dataset_name='ontokin')
        self.chemical_nel = ChemicalNEL(dataset_dir="ontokin")


if __name__ == "__main__":
    my_engine = OntoKinQAEngine()
