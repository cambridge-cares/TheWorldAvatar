import json
import os
import random

from Marie.Util.location import DATA_DIR


class DataProcessorOntoKinReactions:

    def __init__(self):
        self.dataset_dir = "CrossGraph/ontokin_reactions"
        self.three_hop_dict_index_path = os.path.join(DATA_DIR, self.dataset_dir, 'three_hop_dict_index')
        self.three_hop_dict_index = json.loads(open(self.three_hop_dict_index_path).read())
        self.idx_triples_path = os.path.join(DATA_DIR, self.dataset_dir, 'idx_triples.json')
        self.idx_triples = json.loads(open(self.idx_triples_path).read())
        self.train_path = os.path.join(DATA_DIR, self.dataset_dir, 'triples-train.json')
        self.test_path = os.path.join(DATA_DIR, self.dataset_dir, 'triples-test.json')
        self.train = json.loads(open(self.train_path).read())
        self.test = json.loads(open(self.test_path).read())
        # TODO: sample a bigger evaluation set

    def replace_fake_triples(self):
        for triple in self.train:
            score = triple[3]
            if score == 0:
                head = triple[0]
                # find the neighours of head
                neighbours = self.three_hop_dict_index[str(head)]
                print(len(neighbours))
                # this is a "fake triple"

    def resample_test_set(self):
        frac = int(round(len(self.train) * 0.0002, 0))
        test = random.sample(self.train, frac)
        print(len(test))
        with open(self.test_path, 'w') as f:
            f.write(json.dumps(test))
            f.close()

    def check_existence(self, triple):
        return triple in self.idx_triples


if __name__ == "__main__":
    my_processor = DataProcessorOntoKinReactions()
    my_processor.resample_test_set()
