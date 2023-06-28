## Overview
Two models are trained for Entity Linking:
1. A SMILES-NER model for SMILES string recognition.
2. The Entity Extraction module which performs joint Entity Recognition and Entity Linking. We use the method proposed in the BLINK project: https://github.com/facebookresearch/BLINK.

## Setup
1. Copy all the files in `Training/EntityLinking` to `Marie/EntityLinking`.
2. Install Python 3.8 and create the virtual environment as described in the [MARIE AND BERT README](../../readme.md#running) 

## Data Preparation
Please refer to [Dataset creation for Entity Linking](../../KGToolbox/EntityLinking/readme.md) to create the dataset required for training the Entity Linking models.


Create a `data` folder with three sub-folders as shown below in `Marie/EntityLinking` and copy the three groups of files created in the [Data Preparation Step](../../KGToolbox/EntityLinking/readme.md) into them. 

For instance, create `data/step1`, `data/step2`,`data/ner`.

Shown below is an example of the expected folder structure after set-up:
<pre>
├── data
    ├── step1
        ├── dict.jsonl#
        ├── train.jsonl#
        ├── valid.jsonl#
        └── test.jsonl#
    ├── step2
        ├── dict.jsonl#
        ├── train.jsonl#
        ├── valid.jsonl#
        └── test.jsonl#
    └── ner
        ├── train.jsonl#
        ├── val.jsonl#
        └── test.jsonl#
</pre>

## Train SMILES NER
Run the following command to train the SMILES NER model:
```
bash train_ner.sh [valid_file_name] [train_file_name]  [output_path]
```
* `--valid_file_name` Path to the validation `.jsonl` file.
* `--train_file_name` Path to the training `.jsonl` file.
* `--output_path` Path to the output SMILES-NER model.



## Train Entity Extraction
Entity Extraction training needs two steps.

First step:
```
bash train_entity_encoder.sh [data_path] [output_path]
```
* `--data_path` Path to data files required for the first step of training. Please refer to [Entity Linking Dataset Readme Section 2.1](../../KGToolbox/EntityLinking/readme.md#21-generate-the-trainvaltestjsonl-question-files-required-for-the-first-step-of-entity-extraction-training) for creating these data files. Suggested path is `data/step1`.
* `--output_path` Path to the output folder for the first step of training. Suggested path is `models/step1`.


Second step:
```
bash train_question_encoder.sh [path_to_first_step_out] [data_path] [output_path]
```
* `--path_to_first_step_out` Path to the output folder of the first step of training. Suggested path following from above is `models/step1`.
* `--data_path` Path to data files required for the second step of training. Please refer to [Entity Linking Dataset Readme Section 2.2](../../KGToolbox/EntityLinking/readme.md#22-generate-the-traintestvalidjsonl-question-files-required-for-the-second-step-of-entity-extraction-training) for creating these data files. Suggested path is `data/step2`.
* `--output_path` Path to the output folder for the second step of training. Suggested path is `models/step2`.




Files marked with `*` are the ones generated in the training process and are the final binary files required for running the Marie system. These binary files should be moved to `DATA/EntityLinking` folder in the main project. Please refer to the [Main README](../../readme.md) to download `EntityLinking.zip` containing an example set of `EntityLinking` binary files. 

Shown below is an example of the expected file structure after training:
<pre>
├── project-files...
├── data
└── models
    ├── step1
         ├── ...
	 ├── id2text.json*
	 ├── id2title.json*
	 ├── id2wikidata.json*
    	 └── embeddingbase.pt*
    ├── step2
         ├── ...
	 ├── pytorch_model.bin*
    	 └── training_params.txt*
    └── ner
         └── SMILES_NER.bin*
</pre>

The resulting folder structure after moving the binary files to `DATA/EntityLinking`:
<pre>
├── EntityLinking
    ├── ...
    ├── models
         ├── id2text.json
         ├── id2title.json
         └── id2wikidata.json
    ├── pytorch_model.bin
    ├── embeddingbase.pt
    ├── training_params.txt
    └── SMILES_NER.bin*
</pre>