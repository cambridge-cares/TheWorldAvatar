# Author: Sujit Rokka Chhetri
# License: MIT

import sys
from pprint import pprint

from pykg2vec.data.kgcontroller import KnowledgeGraph
from pykg2vec.common import Importer, KGEArgParser
from pykg2vec.utils.trainer import Trainer


def main():
    # getting the customized configurations from the command-line arguments.
    args = KGEArgParser().get_args(sys.argv[1:])

    # Preparing data and cache the data for later usage
    #knowledge_graph = KnowledgeGraph(dataset=args.dataset_name, custom_dataset_path=args.dataset_path)
    knowledge_graph = KnowledgeGraph()
    knowledge_graph.prepare_data()

    # Extracting the corresponding model config and definition from Importer().
    config_def, model_def = Importer().import_model_config('transr')
    config = config_def(args)

    model = model_def(**config.__dict__)

    # Create, Compile and Train the model. While training, several evaluation will be performed.
    trainer = Trainer(model, config)
    trainer.build_model()
    trainer.save_model()


if __name__ == "__main__":
    print('Hello we are here!')
    main()


    # To start the training, use the following command.
    #  pykg2vec-train -exp True -mn TransE -ds pubchem -dsp '../../../Dataset' -hpf config.yaml