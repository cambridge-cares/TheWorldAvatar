## Overview

Multiple models are trained in this system: 
1. Knowledge Graph embedding
2. Relation prediction
3. Entity Linking 
4. Score alignment


###  Knowledge Graph Embedding

To train the embedding of an ontology, a list of files are required. 

- Triples: `[name]-train.txt`, `[name]-test.txt`, `[name]-valid.txt`. The files are headless tsv files with three columns, subject-predicate-object, separated by `tab`. 

- Indexing: `entity2idx.pkl`, `idx2entity.pkl`, `rel2idx.pkl`, `idx2rel.pkl`, provide index-to-label and label-to-index
