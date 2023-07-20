import os

import torch

from Marie.Util.location import DATA_DIR

model_path = os.path.join(DATA_DIR, "CrossGraph/pubchem/bert_embedding_10000")
state_dict = torch.load(model_path)

for layer in state_dict:
    if "bert" not in layer:
        print("------------")
        print(layer)
        print(state_dict[layer].shape)
