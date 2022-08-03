# Author: Sujit Rokka Chhetri and Shiy Yuan Yu
# License: MIT

import sys

from pykg2vec.common import Importer, KGEArgParser
from pykg2vec.utils.trainer import Trainer


def main():
    # getting the customized configurations from the command-line arguments.
    args = KGEArgParser().get_args(sys.argv[1:])

    # Extracting the corresponding model config and definition from Importer().
    config_def, model_def = Importer().import_model_config(args.model_name.lower())
    config = config_def(args)
    model = model_def(**config.__dict__)

    # Create the model and load the trained weights.
    trainer = Trainer(model, config)
    trainer.build_model()

    if config.load_from_data is None:
        trainer.train_model()

    trainer.infer_tails(1, 10, topk=5)
    trainer.infer_heads(10, 20, topk=5)
    trainer.infer_rels(1, 20, topk=5)


if __name__ == "__main__":
    main()