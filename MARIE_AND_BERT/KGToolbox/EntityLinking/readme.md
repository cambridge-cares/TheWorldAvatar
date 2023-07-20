# Entity Linking 
This document serves as a guide for creating datasets used for Entity Linking.

## Overview
The dataset used for Entity Linking consists of a dictionary of entities in the `.json` format and `train.jsonl`, `val.jsonl`, and 
`test.jsonl` files containing questions for training, validation and testing respectively. The questions are generated randomly by combining domain entity names and manually written question templates. Please refer to the 
[Entity Linking Training Readme](../../Training/EntityLinking/readme.md) for more details on Training.

## Setup
Install `Python 3.8`

### Required Files
1. Follow the steps outlined in the [Required Files section of the main README](../../readme.md#required-files) by creating a `DATA` folder under `MARIE_AND_BERT` and unzip `Dictionaries.zip` and `EntityLinking.zip` into the `DATA` folder.
2. Copy `pubchemwithSMILE.jsonl` from `DATA/EntityLinking/training_files_generation` into `KGToolbox/EntityLinking`.
3. Copy all the files from `DATA/EntityLinking/training_files_generation/templates` into `DATA/KGToolbox/EntityLinking/templates`.

`DATA/EntityLinking/training_files_generation/examples` contains example files that can be referred to for checking the expected 
format of output files.

## Running
To create a dataset for the Entity Linking model, follow the steps outlined below:

### Create Entity Dictionaries

Generate a dictionary of entities for each ontology using:
```
python generateEntityDictJPS.py --infile ../../DATA/Dictionaries/ontokin/name_dict.json  --outfile ./ontokin.json
```
* `--infile`  original name_dict `.json` file located in `DATA/Dictionaries` within the respective ontology subfolder.
* `--outfile` path where the output entity dictionary file is saved

See `DATA/EntityLinking/training_files_generation/examples/ontokin.json` for an example output format of entity dictionary for ontokin.

### Create Question Files
#### 1. For SMILES-NER:

Generate the `train/val/test.jsonl` SMILES question files by running the following:
```
python generateFromTemplate.py \
 --outfile smiles_train_raw.jsonl \               #path where the output jsonl file is saved
 --mode train \                        #possible values: 'test', 'val', 'train'
 --question_type smiles \              #possible values: 'general', 'smiles'
 --question_num 1000 \                 #number of questions to generate
 --seed 0                              #random seed
```
* `--outfile` Path where the output jsonl file is saved.
* `--mode` 'test' or 'val' or 'train'. Test and Train/Val use separate templates for evaluation purposes.
* `--question_type` 'general' or 'smiles'. Question with SMILES strings or general entities.
* `--question_num` Number of questions to be generated. 
* `--seed` Random seed. It is recommended to use different seeds for different modes.

Run `covert_ner_train_files.py`.
```
python covert_ner_train_files.py --infile ./smiles_train_raw.jsonl  --outfile ./smiles_train.jsonl
```
* `--infile` Path to the input raw question file generated in the step [above](#1-for-smiles-ner).
* `--outfile` Path to save the formatted output question file.

#### 2. For General Entity Extraction: 

Run the following command to generate the <b>raw</b> `train/val/test.jsonl` question files for each ontology:

Example:
```
python generateFromTemplate.py \
--infile ontokin.json \               #path to the domain Entity Dictionary
 --outfile test_raw.jsonl \            #path where the output jsonl file is saved
 --mode train \                        #possible values: 'test', 'val', 'train'
 --question_type general \             #possible values: 'general', 'smiles'
 --question_num 10000 \                #number of questions to generate
 --seed 0                              #random seed
```
* `--infile` path of the respective domain entity dictionary generated in the [Entiity Dictionaries](#entity-dictionary) step.
* The other parameters are the same as described in the [SMILES-NER](#1.-for-smiles-ner:) section.

##### 2.1 Generate the `train/val/test.jsonl` question files required for the first step of Entity Extraction Training:

Run `convertQuestionBlinkFormat.py` to convert the raw outfile generated [above](#2-for-general-entity-extraction) (e.g., `test_raw.jsonl`) to the accepted format for the first step. 

Set the `convert_type` parameter to `blink`.

Example:
```
python convertQuestionBlinkFormat.py \
--infile test_raw.jsonl \             # path to the raw question file
--outfile test.jsonl \                # path where the output jsonl file is saved
--convert_type blink                  # possible values: 'blink', 'elq'
```
* `--infile` Path to the input raw question file generated in the step [above](#2-for-general-entity-extraction).
* `--outfile` Path to save the formatted output question file.
* `--convert_type` 'blink' or 'elq'. Value is chosen depending on whether the user is converting to the `blink` (1st step) format or `elq` (2nd step) format. 

See `DATA/EntityLinking/training_files_generation/examples/ontokin_blink_format_example.jsonl` for an example of an expected output file.


##### 2.2. Generate the `train/test/valid.jsonl` question files required for the second step of Entity Extraction Training:   

Run `convertQuestionBlinkFormat.py` to convert the raw outfile generated [above](#2-for-general-entity-extraction) (e.g., `test_raw.jsonl`) to the accepted format for the second step.

Set the `convert_type` parameter to `elq`.

Example:
```
python convertQuestionBlinkFormat.py \
--infile test_raw.jsonl \             # path to the raw question file
--outfile test.jsonl \                # path where the output jsonl file is saved
--convert_type elq                    # possible values: 'blink', 'elq'
```
* Parameters are described in [2.1](#2.1.Generate-the-train/val/test.jsonl-question-files-required-for-the-first-step-of-Entity-Extraction-Training).

See `DATA/EntityLinking/training_files_generation/examples/ontokin_elq_format_example.jsonl` for an example of an expected output file.