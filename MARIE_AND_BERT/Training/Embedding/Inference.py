import sys
from pprint import pprint

from pykg2vec.common import Importer, KGEArgParser
from pykg2vec.utils.trainer import Trainer
import argparse


def main():
    # getting the customized configurations from the command-line arguments.

    args = KGEArgParser().get_args(sys.argv[1:])
    args.__setattr__('load_from_data', '../../Dataset/Supermini/intermediate/transr')
    args.__setattr__('model_name', 'TransR')
    # Extracting the corresponding model config and definition from Importer().
    config_def, model_def = Importer().import_model_config(args.model_name.lower())
    print(args)
    config = config_def(args)
    model = model_def(**config.__dict__)

    # Create the model and load the trained weights.
    trainer = Trainer(model, config)
    trainer.build_model()

    if config.load_from_data is None:
        trainer.train_model()
    for i in range(0, 10):
        rst = trainer.infer_tails(0, i, topk=1)
        pprint(rst)


if __name__ == "__main__":
    main()
