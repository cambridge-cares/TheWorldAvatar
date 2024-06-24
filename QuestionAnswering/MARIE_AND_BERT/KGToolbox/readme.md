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
model training for each ontology. The `Score alignment model` subsection gives steps for 
creating the training set for training the score alignment model (`CrossGraph/cross_graph_model_with_all_9_updated`). 

The user is expected to create 

1. Triple files, for example `[ontology_name-train.txt]` and `[ontology_name-test.txt]`. 
2. Indexing files, including `entity2idx.pkl`, `idx2entity.pkl`, `relation2idx.pkl`, `idx2relation.pkl`. 
3. Neighbour dictionary files: `three_hop_dict_index`, `three_hop_dict_label`. 

Other optional embedding training files will be specified in the following sections.  


###  OntoAgent (CrossGraph/agents folder)

Files expected to be created under `CrossGraph/agents/[agent_name]`: 
1. `[agent_name-test.txt]`, `[agent_name-train.txt]`, `[agent_name-train-2.txt]`,
2. The indexing files 
3. The Neighbour dictionary files
4. `info_dict.json`
5. `neg_sample_dict.json`

To create OntoAgent training set:
1. Set up a local blazegraph and have it running on 
`http://127.0.0.1:9999`. For details, please follow [Blazegraph](https://blazegraph.com/)
2. Download the ontology files `Agents.zip` containing two `.nt` files for the agents from the [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0) 
3. Create namespaces `ontopceagent` and `ontothermalagent` and load the two `.nt` files to the according namespace. 
4. Run `KGToolbox/OntoAgent/OntoAgentReader.py`, the script will create both 
the triple files `[agent_name]-train.txt`, `[agent_name]-test.txt`, `[agent_name]-train-2.txt` for embedding training and the `info_dict.json` files for each agent. 
5. Run `KGToolbox/OntoAgent/createOntoAgentBERTTrainingSet.py`, which creates a file named 
`score_model_training.tsv` in the `DATA/CrossGraph/agents` folder. 
 
###  OntoCompChem (CrossGraph/ontocompchem folder)

Files expected to be created under `CrossGraph/ontocompchem`: 
1. `[ontocompchem-test.txt]`, `[ontocompchem-train.txt]`, `[ontocompchem-valid.txt]`,
2. The indexing files 
3. The Neighbour dictionary files
4. `ontocompchem_value_dict.json` 


To create OntoCompChem training set:
1. Run`KGToolbox/OntoCompChem/OntoCompChemReader.py` to create the triples and supporting files. 
2. Run `CreateOntoCompChemTrainingSet.py` to create the BERT training set, 
which creates a file named `score_model_traininng.tsv` in the `DATA/CrossGraph/ontocompchem` folder. 
3. Run `python NHopExtractor.py -onto ontocompchem -dir CrossGraph/ontocompchem` under`MARIE_AND_BERT\Marie\Util`to create neighbour dictionary


### OntoKin (CrossGraph/ontokin folder)

Files expected to be created under `CrossGraph/ontokin`: 
1. `[ontokin-test.txt]`, `[ontokin-train.txt]`
2. The indexing files 
3. The Neighbour dictionary files
4. `ontokin_value_dict.json` 


To create OntoKin training set:

1. Run `KGToolbox/OntoKin/OntoKinReader.py`, all the necessary
file including the triples and the `score_model_training.tsv` will be created and put into `DATA/CrossGraph/ontokin`.
2. Run `python NHopExtractor.py -onto ontokin -dir CrossGraph/ontokin` under`MARIE_AND_BERT\Marie\Util`

### OntoMoPs (CrossGraph/OntoMoPs folder)

Files expected to be created under `CrossGraph/OntoMoPs/numerical_with_implicit`: 
1. `[numerical_with_implicit-test.txt]`, `[numerical_with_implicit-train.txt]`,`[numerical_with_implicit-train-2.txt]`,`[numerical_with_implicit-singular-train.txt]`
2. The indexing files 
3. The Neighbour dictionary files and `three_hop_dict_index_numercial` and `three_hop_dict_label_numercial`
4. `node_value_dict.json`
5. `neg_sample_dict.json`

To create the OntoMoPs training set:

1. Run `KGToolbox/OntoMoPs/OntoMoPsReader.py` to create the supporting files for 
2. The OntoMoPs relation prediction model training dataset is created manually. The training dataset is archived in folder
`OntoMoPs_training_data` from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0)

### OntoSpecies (CrossGraph/ontospecies_new folder)

Files expected to be created under `CrossGraph/ontospecies_new/base_full_no_pref_selected_role_limited_100`: 
1. `[base_full_no_pref_selected_role_limited_100-test.txt]`, `[base_full_no_pref_selected_role_limited_100-train.txt]`,
`[base_full_no_pref_selected_role_limited_100-train-2.txt]`
2. The indexing files 
3. The Neighbour dictionary files and `three_hop_dict_index_numercial` and `three_hop_dict_label_numercial`
4. `node_value_dict.json`
5. `numerical_eval.tsv`
6. `candidate_dict.json`
7. `neg_sample_dict.json`

To create the OntoSpecies training dataset:

1. Download `OntoSpeciesNewData.zip` from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0) and unzip the files under `CrossGraph/ontospecies_new`. The zip files contains dictionaries for 
manually identified duplicated uses and classes.  
2. Run`KGToolbox/OntoSpeciesNew/OntoSpeciesReader.py`to create the triples and the supporting files for embedding training. 
3. Run `KGToolbox/OntoSpeciesNew/CreateOntoSpeciesNewScoreTrainingSet.py`. The script will
create `score_model_training.tsv` under `DATA/CrossGraph/ontospecies_new/base_full_no_pref_selected_role_limited_100` for relation prediction training. 
 
### Wikidata (CrossGraph/wikidata_numerical folder)

Files expected to be created under `CrossGraph/wikidata_numerical`: 
1. `[wikidata_numerical-test.txt]`, `[wikidata_numerical-train.txt]`,
`[wikidata_numerical-valid.txt]`
2. The indexing files 
3. The Neighbour dictionary files and `three_hop_dict_index_numercial` and `three_hop_dict_label_numercial`
4. `wikidata_numerical_value_dict.json`, `wikidata_numerical_value_new.json`, `wikidata_numerical_value_dict_no_unit.json`

To create the Wikidata training dataset:

1. Download Wikidata dump files `instance_info.zip` from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0).
2. Unzip the file into `DATA/CrossGraph/wikidata/instance_info` folder. 
3. Run `KGToolbox/Wikidata/WikiDataReader.py` to create triples and supporting files
4. Download `p_dict.txt` and `p_labels.txt` to `CrossGraph/wikidata_numerical`, the files are under folder `wikidata_numerical_label_files` in  [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0).
5. Run `KGToolbox/Wikidata/WikidataCreateTrainingData.py`, which will create `score_model_training.tsv` 
in `DATA/CrossGraph/wikidata_numerical`. 
6. Run `python NHopExtractor.py -onto wikidata_numerical -dir CrossGraph/wikidata_numerical` under`MARIE_AND_BERT\Marie\Util`


### Pubchem (CrossGraph/pubchem folder)

Files expected to be created under `CrossGraph/pubchem`: 
1. `[pubchem-train.txt]`
2. The indexing files 
3. The Neighbour dictionary files
4. `pubchem_value_dict.pkl`

To create the Pubchem training dataset:

1. Download `pubchem.csv` from the [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0) to `CrossGraph/pubchem`
2. Run `KGToolbox/PubChem/PubchemReader.py`
3. Run `python NHopExtractor.py -onto pubchem -dir CrossGraph/pubchem` under `MARIE_AND_BERT\Marie\Util`

### Score alignment model

To create the training set for training the score alignment model `CrossGraph/cross_graph_model_with_all_9_updated`:

1. Download archived manually created training set `cross_graph_alignment_training_labelled.tsv` from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0)
2. Manually create or modify `cross_graph_alignment_training_labelled.tsv` under folder `DATA/CrossGraph` based on the 
archived `cross_graph_alignment_training_labelled.tsv` file 
3. Run `KGToolbox/CrossGraph/CrossGraph.py`, which will create `cross_graph_alignment_training_labelled.tsv` under `DATA/CrossGraph`


## Dictionaries (DATA/Dictionaries folder)

### Agents (Dictionaries/ontoagent)
The expected files created are under `Dictionaries/ontoagent/[agent_name]`

1. `name_dict.json` 
2. `name_list.json`

To create the files: 

1. Run `KGToolbox/OntoAgent/createOntoAgentPCEAgentDictionary.py`
2. Run `KGToolbox/OntoAgent/createOntoAgentThermalAgentDictionary.py`

### OntoMoPs (Dictionaries/OntoMoPs)

The expected files created are under `Dictionaries/OntoMoPs/numerical_with_implicit`

1. `name_dict.json` 
2. `name_list.json`
3. `class_label_list.json`
4. `label_class_dict.json`
5. `type_dict.json`

To create the files: 

1. Download `label_am_dict.json` from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0) to `CrossGraph/OntoMoPs`
2. Run `KGToolbox/OntoMoPs/createOntoMoPsDictionary.py`

### PubChem (Dictionaries/pubchem)

The expected files created are under `Dictionaries/pubchem`

1. `name_dict.json` 
2. `name_list.json`

To create the files:

1. Download `missing_cid_list.json` from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0) to `CrossGraph/pubchem`.
2. Run `KGToolbox/PubChem/PubchemDictCreator.py`

### OntoSpecies (Dictionaries/ontospecies_new)

The expected files created are under `Dictionaries/ontospecies_new`

1. `name_dict.json` 
2. `name_list.json`
3. `class_label_list.json`
4. `type_dict.json`

To create the files:

1. Run `KGToolbox/OntoSpeciesNew/CreateOntoSPeciesNewDictionary.py`

### Wikidata (Dictionaries/wikidata_numerical)

The expected files created are under `Dictionaries/wikidata_numerical`

1. `name_dict.json` 
2. `name_list.json`
3. `class_label_list.json`
4. `type_dict.json`


The Dictionary files for Wikidata will be created by running `KGToolbox/Wikidata/WikiDataReader.py`. 
No extra steps are required. 


### OntoCompChem (Dictionaries/ontocompchem)

The expected files created are under `Dictionaries/ontocompchem`

1. `name_dict.json` 
2. `name_list.json`

To create the files:

1. Run `KGToolbox/OntoCompChem/create_dict_for_ontocompchem.py`

### OntoKin (Dictionaries/ontokin)

The expected files created are under `Dictionaries/ontokin`

1. `name_dict.json` 
2. `name_list.json`

To create the files: 

1. Make sure `KGToolbox/OntoKin/OntoKinReader.py` has been run already and created `CrossGraph/ontokin/all_species.tsv` is created
2. Run `KGToolbox/OntoKin/create_dict_for_ontokin.py`

### Global dictionary (static/js/label_dict.js)
The expected files `label_dict.js` will be created under `static/js/`

To create the file: 

After dictionaries for Wikidata_numerical, OntoMoPs, and OntoSpecies are created

1. Run `KGToolbox/OntoKin/CreateLabelDict.py` to create `label_dict.js`, the file will be placed under `static/js/label_dict.js`

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
