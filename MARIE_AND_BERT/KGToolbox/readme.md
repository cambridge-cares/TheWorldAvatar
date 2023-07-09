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
```

## CrossGraph 
1. create the `DATA` folder and create the subfolders as the folder structure above. 

The following subsections give steps for creating the training set for both embedding training and relation prediction
model training for each ontology. The [Score Alignment](##Score alignment model) subsection gives steps for 
creating the training set for training the score alignment model (`CrossGraph/cross_graph_model_with_all_9_updated`). 

###  OntoAgent (CrossGraph/agents folder)

To create OntoAgent training set:
1. Set up a local blazegraph and have it running on 
`http://127.0.0.1:9999`. For details, please follow [Blazegraph](https://blazegraph.com/)
2. Download the ontology files for the agents from [Agents.zip](http://159.223.42.53:8080/Agents.zip) 
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

### OntoKin (CrossGraph/ontokin folder)
To create OntoKin training set:

1. Run `KGToolbox/OntoKin/OntoKinReader.py`, all the necessary
file including the triples and the `score_model_training.tsv` will be created and put into `DATA/CrossGraph/ontokin`.


### OntoMoPs (CrossGraph/OntoMoPs folder)

To create the OntoMoPs training set:

1. Run `KGToolbox/OntoMoPs/OntoMoPsReader.py` to create the supporting files for 
2. The OntoMoPs relation prediction model training dataset is created manually. The training dataset is archived in 
`http://159.223.42.53:8080/OntoMoPs/score_model_training.tsv`.

### OntoSpecies (CrossGraph/ontospecies_new folder)

To create the OntoSpecies training dataset:

1. Download 
2. Run`KGToolbox/OntoSpeciesNew/OntoSpeciesReader.py`to create the triples and the supporting files for embedding training. 
3. Run `KGToolbox/OntoSpeciesNew/CreateOntoSPeciesNewScoreTrainingSet.py`. The script will
create `score_model_training.tsv` under `DATA/CrossGraph/ontospecies_new/base_full_no_pref_selected_role_limited_100` for relation prediction training. 
 
### Wikidata (CrossGraph/wikidata_numerical folder)
1. Download Wikidata dump files from `http://159.223.42.53:8080/instance_info.zip`.
2. Unzip the file into `DATA/CrossGraph/wikidata/instance_info` folder. 
3. Run `KGToolbox/Wikidata/WikiDataReader.py` to create triples and supporting files
4. Run `KGToolbox/Wikidata/WikidataCreateTrainingData.py`, which will create `score_model_training.tsv` 
in `DATA/CrossGraph/wikidata_numerical`. 

### Pubchem (CrossGraph/pubchem folder)
1. Download `pubchem.csv` from `http://159.223.42.53:8080/pubchem.csv`.
2. Run `KGToolbox/PubChem/PubchemReader.py`


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


##Entity Linking

To create the Entity Linking dataset, follow the steps outlined in [Entity Linking Dataset Creation readme.md](./EntityLinking/readme.md)


## Future work
The hard-coded URLs in the code need to be removed and more maintainable solutions need to be used. 
