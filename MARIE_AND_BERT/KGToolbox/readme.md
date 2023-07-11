This document provides guides about how the training dataset for different ontologies are created. 

The user will need to create a folder `MARIE_AND_BERT/DATA/CrossGraph` and a series of sub folders for each ontology.
```
├── MARIE_AND_BERT
├───├── DATA
│   │   ├── CrossGraph
│   │   │   ├── agents
│   │   │   │   ├── ontopceagent
│   │   │   │   ├── ontothermoagent
│   │   │   ├── ontocompchem
│   │   │   ├── ontokin
│   │   │   ├── OntoMoPs
│   │   │   │   ├── numerical_with_implicit
│   │   │   ├── ontospecies_new
│   │   │   │   ├── base_full_no_pref_selected_role_limited_100
│   │   │   ├── wikidata_numerical
│   │   │   ├── pubchem
│   │   ├── Dictionaries
│   │   │   ├── ontoagent
│   │   │   │   ├── pceagent
│   │   │   │   ├── thermalagent
│   │   │   ├── OntoMoPs
│   │   │   │   ├── numerical_with_implicit
│   │   │   ├── ontospecies_new
│   │   │   ├── pubchem
│   │   │   ├── wikidata_numerical
│   │   │   ├── ontocompchem
│   │   │   ├── ontokin
```

## CrossGraph 
1. create the `DATA` folder and create the subfolders as the folder structure above. 

The following subsections give steps for creating the training set for both embedding training and relation prediction
model training for each ontology. The [Score Alignment](#score-alignment-model) subsection gives steps for 
creating the training set for training the score alignment model (`CrossGraph/cross_graph_model_with_all_9_updated`). 

###  OntoAgent (CrossGraph/agents folder)

To create OntoAgent training set:
1. Set up a local blazegraph and have it running on 
`http://127.0.0.1:9999`. For details, please follow [Blazegraph](https://blazegraph.com/)
2. Download the ontology files `Agents.zip` containing two `.nt` files for the agents from the [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0) 
3. Create namespaces `ontopceagent` and `ontothermalagent` and load the two `.nt` files to the according namespace. 
4. Run `KGToolbox/OntoAgent/OntoAgentReader.py`, the script will create both 
the triple files for embedding training and the `info_dict.json` files for each agent. 
5. Run `KGToolbox/OntoAgent/createOntoAgentBERTTrainingSet.py`, which creates a file named 
`score_model_training.tsv` in the `DATA/CrossGraph/agents` folder. 
 
###  OntoCompChem (CrossGraph/ontocompchem folder)
To create OntoCompChem training set:
1. Run`KGToolbox/OntoCompChem/OntoCompChemReader.py` to create the triples and supporting files. 
2. Run `CreateOntoCompChemTrainingSet.py` to create the BERT training set, 
which creates a file named `score_model_traininng.tsv` in the `DATA/CrossGraph/ontocompchem` folder. 
3. Run `python NHopExtractor.py -onto ontocompchem -dir CrossGraph/ontocompchem` under`MARIE_AND_BERT\Marie\Util`to create neighbour dictionary


### OntoKin (CrossGraph/ontokin folder)
To create OntoKin training set:

1. Run `KGToolbox/OntoKin/OntoKinReader.py`, all the necessary
file including the triples and the `score_model_training.tsv` will be created and put into `DATA/CrossGraph/ontokin`.
2. Run `python NHopExtractor.py -onto ontokin -dir CrossGraph/ontokin` under`MARIE_AND_BERT\Marie\Util`

### OntoMoPs (CrossGraph/OntoMoPs folder)

To create the OntoMoPs training set:

1. Run `KGToolbox/OntoMoPs/OntoMoPsReader.py` to create the supporting files for 
2. The OntoMoPs relation prediction model training dataset is created manually. The training dataset is archived in 
`http://159.223.42.53:8080/OntoMoPs/score_model_training.tsv`.

### OntoSpecies (CrossGraph/ontospecies_new folder)

To create the OntoSpecies training dataset:

1. Download `http://159.223.42.53:8080/OntoSpeciesNewData.zip` and unzip the files under `CrossGraph/ontospecies_new`. The zip files contains dictionaries for 
manually identified duplicated uses and classes.  
2. Run`KGToolbox/OntoSpeciesNew/OntoSpeciesReader.py`to create the triples and the supporting files for embedding training. 
3. Run `KGToolbox/OntoSpeciesNew/CreateOntoSpeciesNewScoreTrainingSet.py`. The script will
create `score_model_training.tsv` under `DATA/CrossGraph/ontospecies_new/base_full_no_pref_selected_role_limited_100` for relation prediction training. 
 
### Wikidata (CrossGraph/wikidata_numerical folder)
1. Download Wikidata dump files from `http://159.223.42.53:8080/instance_info.zip`.
2. Unzip the file into `DATA/CrossGraph/wikidata/instance_info` folder. 
3. Run `KGToolbox/Wikidata/WikiDataReader.py` to create triples and supporting files
4. Run `KGToolbox/Wikidata/WikidataCreateTrainingData.py`, which will create `score_model_training.tsv` 
in `DATA/CrossGraph/wikidata_numerical`. 
5. Run `python NHopExtractor.py -onto wikidata_numerical -dir CrossGraph/wikidata_numerical` under`MARIE_AND_BERT\Marie\Util`


### Pubchem (CrossGraph/pubchem folder)
1. Download `pubchem.csv` from the [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0) to `CrossGraph/pubchem`
2. Run `KGToolbox/PubChem/PubchemReader.py`
3. Run `python NHopExtractor.py -onto pubchem -dir CrossGraph/pubchem` under `MARIE_AND_BERT\Marie\Util`

### Score alignment model

To create the training set for training the score alignment model `CrossGraph/cross_graph_model_with_all_9_updated`:

1. Download archived manually created training set `cross_graph_alignment_training_labelled.tsv` from `http://159.223.42.53:8080/cross_graph_alignment_training_labelled.tsv`
2. Manually create or modify `cross_graph_alignment_training_labelled.tsv` under folder `DATA/CrossGraph` based on the 
archived `cross_graph_alignment_training_labelled.tsv` file. 
3. Run `KGToolbox/CrossGraph/CrossGraph.py`, which will create `cross_graph_alignment_training_labelled.tsv` under `DATA/CrossGraph`


## Dictionaries (DATA/Dictionaries folder)

### Agents (Dictionaries/ontoagent)

1. Run `KGToolbox/OntoAgent/createOntoAgentPCEAgentDictionary.py`
2. Run `KGToolbox/OntoAgent/createOntoAgentThermalAgentDictionary.py`

### OntoMoPs (Dictionaries/OntoMoPs)

1. Download `label_am_dict.json` from `http://159.223.42.53:8080/label_am_dict.json` to `CrossGraph/OntoMoPs`
2. Run `KGToolbox/OntoMoPs/createOntoMoPsDictionary.py`

### PubChem (Dictionaries/pubchem)
1. Download `missing_cid_list.json` from `http://159.223.42.53:8080/missing_cid_list.json` to `CrossGraph/pubchem`.
2. Run `KGToolbox/PubChem/PubchemDictCreator.py`

### OntoSpecies (Dictionaries/ontospecies_new)

1. Run `KGToolbox/OntoSpeciesNew/CreateOntoSPeciesNewDictionary.py`

### Wikidata (Dictionaries/wikidata_numerical)

The Dictionary files for Wikidata will be created by running `KGToolbox/Wikidata/WikiDataReader.py`. 
No extra steps are required. 


### OntoCompChem (Dictionaries/ontocompchem)

1. Run `KGToolbox/OntoCompChem/create_dict_for_ontocompchem.py`

### OntoKin (Dictionaries/ontokin)

1. Make sure `KGToolbox/OntoKin/OntoKinReader.py` has been run already and created `CrossGraph/ontokin/all_species.tsv` is created
2. Run `KGToolbox/OntoKin/create_dict_for_ontokin.py`


## Head tensor files 
Head tensor files are dictionaries storing the embeddings of instances under a certain class to shorten the system's responses,
wikidata_numerical, OntoMoPs, and ontospecies_new folders contain them, to create the files:

### Wikidata (CrossGraph/wikidata_numerical)

1. Run `KGToolbox/Wikidata/WikidataCreateCachedHeads.py`, creates `all_heads.pkl` under `CrossGraph/wikidata_numerical`

### OntoMoPs (CrossGraph/OntoMoPs)

1. Run `KGToolbox/OntoMoPs/CreateHeadTensorForOntoMoPs.py`, creates `all_heads.pkl` under `CrossGraph/OntoMoPs`

### OntoSpecies (CrossGraph/ontospecies_new)
1. Run `KGToolbox/OntoMoPs/CreateHeadTensorForOntoSpeciesNew.py`, creates `all_heads.pkl` under `CrossGraph/ontospecies_new`


Once the above-mentioned steps are done, please see [Training readme.md](../Training/readme.md) to train the knowledge 
graph embeddings and the natural language models. 


##Entity Linking

To create the Entity Linking dataset, follow the steps outlined in [Entity Linking Dataset Creation readme.md](./EntityLinking/readme.md)
## Future work
The hard-coded URLs in the code need to be removed and more maintainable solutions need to be used. 
