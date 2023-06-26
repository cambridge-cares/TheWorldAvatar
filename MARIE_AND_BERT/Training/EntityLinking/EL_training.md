## Overview
Two models are trained for EL:
1. A SMILES-NER model for smiles string recognition and translation module
2. The joint ER/EL for head entity extraction. We use the method of BLINK project: https://github.com/facebookresearch/BLINK.

## Setup
1. Import the BLINK project from https://github.com/facebookresearch/BLINK. The customized files should be in the top level directory of the original project.
2. Install Python 3.8 and Run pip install requirements.txt. It is recommended to use conda for creating a virtual environment. Note that this can share the same environment with the whole Marie project and does not necessarily need separate environment.


## Data Preparation
1. SMILES-NER needs a trio of train/valid/test|jsonl files.
2. Joint ER/EL(BLINK) uses a two-step process. First step needs an entity dictionary |jsonl and a trio of train/valid/test|jsonl. Second steps needs another trio if train/valid/test |jsonl which is of slightly different format.
Regarding the creation of these files, please see MARIE_AND_BERT/KGToolbox/EntityLinking/readme.md.

##Folder Structure
<pre>
├── blink-project-files...
├── trainbi.py    #Customized entry point to run BLINK training
├── evalbi.py     #Customized entry point to run BLINK evaluation
└── translator    # NER training code
    ├── util.py
    ├── train.py  #Entry point for NER train
    └── test.py   #Entry point for NER eval
└── my_scripts
    ├── train_entity_encoder.sh
    ├── train_question_encoder.sh
    └── train_ner.sh
</pre>

## Train NER
```
bash train_ner.sh
```
Parameters can be changed in the sh file. Aftering run, the model is saved to the specified path (default is ./models/ner/SMILES_NER.bin)  

## Train ER/EL
ER/EL training needs two steps.
```
bash train_entity_encoder.sh
bash train_question_encoder.sh
```
Input/Output paths can be changed in the sh file. By default, the input folders are ./data/pretrain for the first step and ./data/finetune for the second step; default output folders are ./models/pretrain and ./models/finetune respectively.
Below is an example of the default i/o structure. Files marked with # are input files; files marked with * are final binary files required for Marie system running, which should be put under DATA/EntityLinking in the main project. Refer to https://github.com/cambridge-cares/TheWorldAvatar/blob/dev-marie-and-bert/MARIE_AND_BERT/readme.md to download an example of EntityLinking binary files. 
<pre>
├── project-files...
├── data
    ├── pretrain
         ├── dict.jsonl#
	 ├── train.jsonl#
	 ├── valid.jsonl#
    	 └── test.jsonl#
    ├── finetune
         ├── dict.jsonl#
	 ├── train.jsonl#
	 ├── valid.jsonl#
    	 └── test.jsonl#
    └── ner
	 ├── train.jsonl#
	 ├── dev.jsonl#
    	 └── test.jsonl#
└── models
    ├── pretrain
         ├── ...
	 ├── id2text.json*
	 ├── id2title.json*
	 ├── id2wikidata.json*
    	 └── embeddingbase.pt*
    ├── finetune
         ├── ...
	 ├── pytorch_model.bin*
    	 └── training_params.txt*
    └── ner
         └── SMILES_NER.bin*
</pre>