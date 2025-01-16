import os

import pandas as pd

from Marie.Util.location import DATA_DIR

headers = ["question", "head", "tail", "rel", "numerical_operator"]

ontology = "OntoMoPs"
sub_ontology = "numerical_with_implicit"
full_dataset_dir = os.path.join(DATA_DIR, "../CrossGraph", ontology, sub_ontology)


df = pd.read_csv(os.path.join(full_dataset_dir, "../score_model_training.tsv"), sep="\t", header=None, index_col=None)
df.columns = headers

df.to_csv(os.path.join(full_dataset_dir, "../score_model_training.tsv"), sep="\t")