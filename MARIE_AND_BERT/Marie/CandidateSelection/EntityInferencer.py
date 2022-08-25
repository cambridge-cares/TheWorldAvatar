import sys

from pykg2vec.common import Importer, KGEArgParser
from pykg2vec.utils.trainer import Trainer

from Marie.CandidateSelection.location import DATASET_DIR, MODEL_DIR


class EntityInferencer:

    def __init__(self):
        args = KGEArgParser().get_args(None)
        # args.__setattr__('load_from_data', DATASET_DIR)
        args.__setattr__('load_from_data', MODEL_DIR)
        args.__setattr__('dataset_path', DATASET_DIR)
        args.__setattr__('model_name', 'TransR')
        args.__setattr__('dataset_name', 'pubchemsupermini')
        args.__setattr__('device', 'cuda')
        print(args)
        config_def, model_def = Importer().import_model_config(args.model_name.lower())
        config = config_def(args)
        model = model_def(**config.__dict__)
        self.trainer = Trainer(model, config)
        self.trainer.build_model()

    def infer_entity(self, head_idx, relation_idx):
        # Create the model and load the trained weights.
        rst = self.trainer.infer_tails(head_idx, relation_idx, topk=20)
        return rst


if __name__ == '__main__':
    seed = sys.argv[1:]
    my_entity_inferencer = EntityInferencer()
    my_entity_inferencer.infer_entity(0, 1)
