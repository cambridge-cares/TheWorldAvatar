# Entity Linking 
This document serves as a guide for creating datasets used for Entity Linking.

##Overview
The dataset used for Entity Linking consists of a dictionary of entities in the `.json` format and `train.jsonl`, `val.jsonl`, and 
`test.jsonl` files containing questions for training, validation and testing respectively. Please refer to the 
[Entity Linking Training Readme](../../Training/EntityLinking/readme.md) for more details on Training.

##Setup
Install `Python 3.8`

###Required Files
1. Follow the steps outlined in the [Required Files section of the main README](../../readme.md#required-files) by creating a `DATA` folder under `MARIE_AND_BERT` and unzip `Dictionaries.zip` and `EntityLinking.zip` into the `DATA` folder.
2. Copy `pubchemwithSMILE.jsonl` from `DATA/EntityLinking/training_files_generation` into `DATA/KGToolbox/EntityLinking`.
3. Copy all the files from `DATA/EntityLinking/training_files_generation/templates` into `DATA/KGToolbox/EntityLinking/templates`.

`DATA/EntityLinking/training_files_generation/examples` contains example files that can be referred to for checking the expected 
format of output files.

##Running
To create a dataset for the Entity Linking model, follow the steps outlined below:

###Create Entity Dictionaries

Generate a dictionary of entities for each ontology using:
```
python generateEntityDictJPS.py --infile   --outfile
```
* `--infile` 
* `--outfile`

See `DATA/EntityLinking/training_files_generation/examples/ontokin.json` for an example output format of entity dictionary for ontokin.

### Create Question Files
#### 1. For SMILES-NER:   

Generate the `train/val/test.jsonl` SMILES question files by running the following:
```
python generateFromTemplate.py --outfile train.jsonl --question_type smiles --seed 0
```
* `--outfile` 
* `--question_type`
* `--seed` It is recommended to change random seed when generate train/test/valid files.

#### 2. For General Entity Extraction: 

Run the following command to generate the <b>raw</b> `train/val/test.jsonl` question files for each ontology:

Example:
```
python generateFromTemplate.py --infile ontokin.jsonl --outfile test_raw.jsonl --question_type general --seed 0
```
* `--infile` path of the respective domain entity dictionary generated in the [Entiity Dictionaries](#entity-dictionary) step.
* `--outfile`
* `--question_type`
* `--seed`

####2.1 Generate the `train/val/test.jsonl` question files required for the first step of Entity Extraction Training:

Run `convertQuestionBlinkFormat.py` to convert the raw outfile generated [above](#2-for-general-entity-extraction) (e.g., `test_raw.jsonl`) to the accepted format. 

Set the `convert_type` parameter to `blink`.

Example:
```
python convertQuestionBlinkFormat.py --infile test_raw.jsonl --outfile test.jsonl --convert_type blink --seed 0
```
* `--infile` .
* `--outfile`
* `--convert_type`
* `--seed`

See `DATA/EntityLinking/training_files_generation/examples/ontokin_blink_format_example.jsonl` for an example of an expected output file.


#####2.2. Generate the `train/test/valid.jsonl` question files required for the second step of Entity Extraction Training:   

Run `convertQuestionBlinkFormat.py` to convert the raw outfile generated [above](#2-for-general-entity-extraction) (e.g., `test_raw.jsonl`) to the accepted format.

Set the `convert_type` parameter to `elq`.

Example:
```
python convertQuestionBlinkFormat.py --infile test_raw.jsonl --outfile test.jsonl --convert_type elq --seed 0
```
* `--infile` .
* `--outfile`
* `--convert_type`
* `--seed`

See `DATA/EntityLinking/training_files_generation/examples/ontokin_elq_format_example.jsonl` for an example of an expected output file.