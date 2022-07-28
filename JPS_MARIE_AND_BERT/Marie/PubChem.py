from pprint import pprint

from Marie.SubgraphExtraction.SubgraphExtractor import SubgraphExtractor
from Marie.CandidateSelection.EntityInferencer import EntityInferencer
from Marie.CandidateSelection.Entity2Index import relation_list, relation2idx_map, entity2idx_map


class PubChemEngine:

    def __init__(self):
        self.subgraph_extractor = SubgraphExtractor()
        self.entity_inferencer = EntityInferencer()

    def find_best_match(self, head_entity, relation):
        candidate_list = self.subgraph_extractor.retrieve_subgraph(_head_entity=head_entity)
        head_entity_idx = entity2idx_map[head_entity]
        relation_idx = relation2idx_map[relation]
        predicted_answers = self.entity_inferencer.infer_entity(
            head_entity_idx,
            relation_idx
        )
        for idx, predicted_entity in predicted_answers.items():
            if predicted_entity in candidate_list:
                return idx, predicted_entity

        return None


if __name__ == '__main__':
    my_pubchem_engine = PubChemEngine()
    heads = ['CID1', 'CID2', 'CID3']
    relations = relation_list
    for h in heads:
        for r in relations:
            print('========================')
            print('head:', h)
            print('rel: ', r)
            print('----------------')
            print(my_pubchem_engine.find_best_match(h, r))
