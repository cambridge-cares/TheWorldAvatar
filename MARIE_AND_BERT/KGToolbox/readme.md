This document provides guides about how the training dataset for different ontologies are created. 

The user will need to create a folder `MARIE_AND_BERT/DATA/CrossGraph` and a series of sub folders for each ontology.
```
├── MARIE_AND_BERT
├───├── DATA
│   │   ├── CrossGraph
│   │   │   ├── agents
│   │   │   ├── ontocompchem
│   │   │   ├── ontokin
│   │   │   ├── OntoMoPs
│   │   │   ├── ontospecies_new
│   │   │   ├── wikidata_numerical
```
# Dataset
The dataset is created using a set of question templates. The script fills the templates to create draft questions. The grammar of the draft questions must be corrected manually using the the GUI. The dataset comprises a set of about 1000 questions with labelled head and answer entities. The dataset is divided into a ``training set``, ``validation set`` and ``test set`` in the ratio `7:2:1`.
The ``training set`` and `validation set` are used for training the models. The ``test set``
is used for the evaluation of individual models. More importantly, the ``test set`` is also used to evaluate
the [overall performance](#overall-performance testing)
of the QA system.

The `KGToolbox` folder contains all the scripts for generating training and evaluation datasets.

## Cross Graph 

`KGToolbox/CrossGraph/CrossGraph.py` is for the generation of the training questions for training 
the cross-graph score alignment model, which generates a tsv file named `cross_graph_alignment_training_updated.tsv`
To run this script, it requires a file named `cross_graph_alignment_training_labelled.tsv`, which is manually created. 

`cross_graph_alignment_training_labelled.tsv` has two columns. The question column should provide the main 
keywords within the question, with stop words for example "what", "find", "the" and the entities 
should be removed from the question. The "true_domain" column contains a list of strings, where the 
domains that could answer the question should be stated. The available options as of 2023-06-21 are 
"ontokin", "ontocompchem", "ontospecies", "ontoagent", "wikidata_numerical", "pubchem", "ontokin_reaction",
"ontospecies_new". In the near future, the "ontospecies_new" label should be removed if the "old ontospecies"
ontology is removed.  Below is an example of the question. 

```
question,true_domain
lennard jones diameter,['ontokin']
lennard jones well depth,['ontokin']
polarizability,['ontokin']
```
 
## OntoAgent 
To create OntoAgent training set, first the `info_dict.json` files should be created for each agent.

First, it requires the user to set up a local blazegraph and load the target agent owl files on 
`http://127.0.0.1:9999`. By running `KGToolbox/OntoAgent/OntoAgentReader.py`, the script will create both 
the triple files for embedding and the `info_dict.json` files for each agent. 

The OntoAgent training set provides the training material for training the BERT model to transform OntoAgent 
related questions to the OntoAgent relation embedding. 

With the `info_dict.json` files created, create the 
training set with `KGToolbox/OntoAgent/createOntoAgentBERTTrainingSet.py`, which creates a file named 
`score_model_training.tsv` in the `DATA/CrossGraph/agents` folder. 

## OntoCompChem 

OntoCompChem requires implicit relation derivation. To create the triples, 
1. Run`KGToolbox/OntoCompChem/OntoCompChemReader.py` to create the triples and supporting files. 
2. Run `CreateOntoCompChemTrainingSet.py` to create the BERT training set, 
which creates a file named `score_model_traininng.tsv` in the `DATA/CrossGraph/ontocompchem` folder. 

## OntoKin

OntoKin reactions and other parts of the OntoKin ontology are dealt with separately, the other parts of 
the OntoKin are regarded as `OntoKin` while the reactions are regarded to as `OntoKinReaction`. `OntoKinReaction`
is answered via Semantic Parsing and requires no training. `OntoKin` is handled in the same way 
as the other ontologies. 

To create the training dataset, the user needs to 

1. Run `KGToolbox/OntoKin/OntoKinReader.py`, all the necessary
file including the triples and the `score_model_training.tsv` will be created and put into `DATA/CrossGraph/ontokin`.

## OntoMoPs

The OntoMoPs training dataset is created manually. The training dataset is archived in 
`http://www.theworldavaatar.com/MARIE_DATA/OntoMoPs/score_model_training.tsv`. 
The file follows the format blow. 
```
question,head,tail,rel,numerical_operator
List the Chemical Building Units with as the Generic Building Unit ,0,0,isFunctioningAsReversed,none
List the CBUs with as the Generic Building Unit ,0,0,isFunctioningAsReversed,none
Give MOPs with molecular weight above ,0,0,hasMolecularWeight,larger
Give MOPs with molecular weight beneath ,0,0,hasMolecularWeight,smaller
```

## OntoSpecies 

In this document, `OntoSpecies` refers to the latest version of OntoSpecies created in 2023,
while the previous versions of OntoSpecies are referred to as `OntoSpecies_Old`. 

To create the training dataset, the user needs to

1. Run`KGToolbox/OntoSpeciesNew/OntoSpeciesReader.py`to create the triples and the supporting files. 
2. Run `KGToolbox/OntoSpeciesNew/CreateOntoSPeciesNewScoreTrainingSet.py`. The script will
create `score_model_training.tsv` under `DATA/CrossGraph/ontospecies_new`
 
## Wikidata 

To create the Wikidata training dataset,a set of Wikidata dump files are required. The files 
can be downloaded from `http://www.theworldavatar.com/MARIE_DATA/instance_info.zip`. The zip
file needs to be extracted into `DATA/instance_info` folder. To create the training dataset

1. Run `KGToolbox/Wikidata/WikiDataReader.py` to create triples and supporting files
2. Run `KGToolbox/Wikidata/WikidataCreateTrainingData.py`, which will create `score_model_training.tsv` 
in `DATA/CrossGraph/wikidata_numerical`. 

##Entity Linking

To create the Entity Linking dataset, follow the steps outlined in [Entity Linking Dataset Creation readme.md](./EntityLinking/readme.md)


## Future work
The hard-coded URLs in the code need to be removed and more maintainable solutions need to be used. 
