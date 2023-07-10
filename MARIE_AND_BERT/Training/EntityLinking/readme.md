## Overview
Two models are trained for Entity Linking:
1. A SMILES-NER model for SMILES string recognition.
2. The Entity Extraction module which performs joint Entity Recognition and Entity Linking. We use the method proposed in the BLINK project: https://github.com/facebookresearch/BLINK.

## Setup
Install Python 3.8 and create the virtual environment as described in the [MARIE AND BERT README](../../readme.md#running) 

## Data Preparation
Please refer to [Dataset creation for Entity Linking](../../KGToolbox/EntityLinking/readme.md) to create the dataset required for training the Entity Linking models.


Create a `data` folder with three sub-folder and copy the three groups of files created in the [Data Preparation Step](../../KGToolbox/EntityLinking/readme.md) into them. 

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
###Training on Windows
Follow the [setup guide](setup) to configure the environment. Move the `/data` folder created in  [Data Preparation](#data-preparation) under `MARIE_AND_BERT/Training/EntityLinking`. 

Use `MARIE_AND_BERT/Training/EntityLinking` as root folder to run the following command to train the SMILES NER model:
```
bash scripts/train_ner.sh [valid_file_name] [train_file_name]  [output_path]
```
* `--valid_file_name` Path to the validation `.jsonl` file.
* `--train_file_name` Path to the training `.jsonl` file.
* `--output_path` Path to the output SMILES-NER model.



## Train Entity Extraction
It is recommended to train Entity Linking models  on HPC.
###Training on HPC
1. Get an HPC account, use `git pull` to pull the `MARIE_AND_BERT` repository.
2. Create a folder called `NEL_Training_Marie_and_Bert`. This is to be used as the training workdir.
3. Copy and transfer all the files under  `MARIE_AND_BERT/Marie/EntityLinking` to  `NEL_Training_Marie_and_Bert`.
4. Copy and transfer all the files under `MARIE_AND_BERT/Training/EntityLinking` to  `NEL_Training_Marie_and_Bert`.
5. Copy and transfer the `/data` folder created in [Data Preparation](#data-preparation) into `NEL_Training_Marie_and_Bert`  .
6. Create a python virtual environment with `Python 3.8`
7. Configure the batch file, the following section will give details on batch file for each step. When the environment is created, install the libraries
   using `MARIE_AND_BERT/requirements_linux.txt`.
8. Submit the batch script

First Step (requires subsequent run of two separate python scripts):


```
. /etc/profile.d/modules.sh                # Leave this line (enables the module command)
module purge                               # Removes all modules still loaded
module load rhel8/default-amp              # REQUIRED - loads the basic environment

# =============================== Load extra modules =========================
module load python/3.8  cuda/11.1 cudnn/8.0_cuda-11.1
# virtualenv --system-site-packages ~/marie
source ~/marie/bin/activate
# ============================================================================
#! Full path to application executable:  
application="python trainbi.py"
#! Run options for the application: 
options="--data_path data/step1 --output_path models/step1 --learning_rate 1e-05 --num_train_epochs 1 --max_context_length 256 --max_cand_length 256 --train_batch_size 16 --eval_batch_size 8 --bert_model bert-base-uncased --type_optimization all_encoder_layers --data_parallel"


#! Work directory (i.e. where the job will run):
workdir="/home/[your_CRSid]/[your_training_folder]/NEL_Training_Marie_and_Bert"  # The value of SLURM_SUBMIT_DIR sets workdir to the directory
```

```
. /etc/profile.d/modules.sh                # Leave this line (enables the module command)
module purge                               # Removes all modules still loaded
module load rhel8/default-amp              # REQUIRED - loads the basic environment

# =============================== Load extra modules =========================
module load python/3.8  cuda/11.1 cudnn/8.0_cuda-11.1
# virtualenv --system-site-packages ~/marie
source ~/marie/bin/activate
# ============================================================================
#! Full path to application executable:  
application="python evalbi.py"
#! Run options for the application: 
options="--path_to_model models/step1/pytorch_model.bin --data_path data/step1 --output_path models/step1 --encode_batch_size 8 --eval_batch_size 1 --top_k 8 --save_topk_result --bert_model bert-base-uncased --mode test --zeshel False --data_parallel  --cand_encode_path ,models/step1/embeddingbase.pt"



#! Work directory (i.e. where the job will run):
workdir="/home/[your_CRSid]/[your_training_folder]/NEL_Training_Marie_and_Bert"  # The value of SLURM_SUBMIT_DIR sets workdir to the directory
```
Second Step:
```
. /etc/profile.d/modules.sh                # Leave this line (enables the module command)
module purge                               # Removes all modules still loaded
module load rhel8/default-amp              # REQUIRED - loads the basic environment

# =============================== Load extra modules =========================
module load python/3.8  cuda/11.1 cudnn/8.0_cuda-11.1
# virtualenv --system-site-packages ~/marie
source ~/marie/bin/activate
# ============================================================================
#! Full path to application executable:  
application="python elq/biencoder/train_biencoder.py"
#! Run options for the application: 
options="--output_path models/step2 --path_to_model models/step1/pytorch_model.bin --data_path data/step2 --num_train_epochs 2 --learning_rate 0.00001 --max_context_length 128 --max_cand_length 256 --train_batch_size 16 --eval_batch_size 8 --bert_model bert-base-uncased --mention_scoring_method qa_linear --eval_interval 500 --last_epoch -1 --no_mention_bounds --mention_aggregation_type all_avg --get_losses --dont_distribute_train_samples"


#! Work directory (i.e. where the job will run):
workdir="/home/[your_CRSid]/[your_training_folder]/NEL_Training_Marie_and_Bert"  # The value of SLURM_SUBMIT_DIR sets workdir to the directory
```

###Training on Windows
Follow the [setup guide](setup) to configure the environment. Move the `/data` folder created in  [Data Preparation](data-preparation) under `MARIE_AND_BERT/Training/EntityLinking`. Also use `MARIE_AND_BERT/Training/EntityLinking` as the root folder to run the following commands:


First step:
```
bash scripts/train_entity_encoder.sh [data_path] [output_path]
```
* `--data_path` Path to data files required for the first step of training. Please refer to [Entity Linking Dataset Readme Section 2.1](../../KGToolbox/EntityLinking/readme.md#21-generate-the-trainvaltestjsonl-question-files-required-for-the-first-step-of-entity-extraction-training) for creating these data files. Suggested path is `data/step1`.
* `--output_path` Path to the output folder for the first step of training. Suggested path is `models/step1`.


Second step:
```
bash scripts/train_question_encoder.sh [path_to_first_step_out] [data_path] [output_path]
```
* `--path_to_first_step_out` Path to the output folder of the first step of training. Suggested path following from above is `models/step1`.
* `--data_path` Path to data files required for the second step of training. Please refer to [Entity Linking Dataset Readme Section 2.2](../../KGToolbox/EntityLinking/readme.md#22-generate-the-traintestvalidjsonl-question-files-required-for-the-second-step-of-entity-extraction-training) for creating these data files. Suggested path is `data/step2`.
* `--output_path` Path to the output folder for the second step of training. Suggested path is `models/step2`.



###Training Result


Shown below is an example of the expected file structure after training:

Files marked with `*` are the ones generated in the training process and are the final binary files required for running the Marie system. These binary files should be moved to `DATA/EntityLinking` folder in the main project. Please refer to the [Main README](../../readme.md) to download `EntityLinking.zip` containing an example set of `EntityLinking` binary files. 

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

## Final EntityLinking.zip

The final binary file folder for entitylinking is of the following structure.
Refer to [Data Preparation Step](../../KGToolbox/EntityLinking/readme.md) for how to create training files in the data subfolder. The result are model files created in the last step. 
<pre>
├── EntityLinking
    ├── data
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