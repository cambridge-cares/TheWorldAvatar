import os

import pandas as pd

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR

dataset_name = "ontokin_reactions"
full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph/ontokin_reactions")
file_loader = FileLoader(full_dataset_dir=full_dataset_dir, dataset_name=dataset_name)

triples = file_loader.load_all_triples()

all_triples = []
for triple in triples:
    s, p, o = [e.strip() for e in triple.split("\t")]
    old_triple = (s, p, o)
    if p == "isReactant":
        new_triple = (o, "hasReactant", s)
    else:
        new_triple = (o, "hasProduct", s)

    all_triples.append(old_triple)
    all_triples.append(new_triple)

df_train = pd.DataFrame(all_triples)
df_train.to_csv(f"{full_dataset_dir}/{dataset_name}-train.txt", sep="\t", header=False, index=False)