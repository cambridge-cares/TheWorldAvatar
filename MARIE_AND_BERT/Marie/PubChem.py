from pprint import pprint

from Marie.SubgraphExtraction.SubgraphExtractor import SubgraphExtractor
from Marie.EntityLinking.Inference import NELInfer


class PubChemEngine:

    def __init__(self):
        self.subgraph_extractor = SubgraphExtractor()
        print('============ Initializing entity linking ============')
        self.entity_linker = NELInfer('conf/base500.yaml')
        print('============ Done initializing entity linking ==============')

    def extract_head_ent(self, question):
        return self.entity_linker.infer([{"text": question}])


if __name__ == '__main__':
    my_pubchem_engine = PubChemEngine()
    my_pubchem_engine.extract_head_ent('what is the molar mass of benzene')
