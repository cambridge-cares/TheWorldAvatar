## Overview

Multiple models are trained in this system: 
1. Knowledge Graph embedding
2. Relation prediction
3. Score alignment
4. Entity Linking 

<pre>
├── MARIE_AND_BERT
├───├── Training
│   │   ├── Trainers
│   │   │   ├── ComplexScoreModelTrainer.py # complex relation prediction 
│   │   │   ├── ComplexTrainer.py # complex embedding
│   │   │   ├── CrossGraphTrainer.py # score alignment
│   │   │   ├── InferenceTrainerTransEA.py  # TransEA embedding with inference
│   │   │   ├── TransEAScoreModelTrainer.py # TransEA relation prediction
│   │   │   ├── TransEATrainer.py  # TransEA embedding 
│   │   │   ├── TransEScoreModelTrainer.py # TransE relation prediction
│   │   │   ├── TransRAScoreModelTrainer.py # TransRA relation prediction
│   │   │   ├── TransRATrainer.py  # TransR and TransRA embedding 
</pre>

##  Knowledge Graph Embedding

### File requirement
To train the embedding of an ontology, a list of files are required. The files are script created, see [readme.md for dataset creation](../KGToolbox/readme.md) for details. 

1. Triples: `[name]-train.txt`, `[name]-test.txt`, `[name]-valid.txt`. The files are headless tsv files with three columns, subject-predicate-object, separated by `tab`. 

2. Indexing: `entity2idx.pkl`, `idx2entity.pkl`, `rel2idx.pkl`, `idx2rel.pkl`, provide index-to-label and label-to-index dictionaries

3. Neighbour dictionary: `three_hop_dict_index` and `three_hop_dict_label`. 

Once all the files are created, choose the embedding method, the implemented ones are 
`Complex`, `TransE`, `TransEA`,`TransR`, `TransRA`. The preferred embedding method for a typical TWA ontology is `TransRA`.

In folder `MARIE_AND_BERT/Training/Trainers`, `TransRATrainer.py` handles both `TransR` and `TransRA`. `ComplexTrainer.py` handles
`Complex`, `TransEATrainer.py` handles `TransE` and `TransEA`. 

### Configuration on HPC

1. Get an HPC account, use git to pull the `MARIE_AND_BERT` repository. 
2. Create and transfer the files required for training under `MARIE_AND_BERT/DATA/[ontology_name]/[sub-ontology_name]`
3. Create a python virtual environment with `Python 3.8`
4. Configure the batch file, the following example is a batch file for training `OntoSpecies` with 
`Complex` embedding and use `marie` python virtual environment. When the environment is created, install the libraries 
using `MARIE_AND_BERT/requirements_linux.txt`. 
5. Submit the batch script

For more details, please check [https://docs.hpc.cam.ac.uk/hpc/](https://docs.hpc.cam.ac.uk/hpc/). 

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
application="python ComplexTrainer.py"
#! Run options for the application: 
options="-d 50 -proj no -bs 32 -lr 0.01 -alpha 0.01 -epoch 100 -so base_full_selected_role_only -g 1 -resume no -o ontospecies_new -inference yes -test no -gpu_num 1"

#! Work directory (i.e. where the job will run):
workdir="/home/[your_CRSid]/[your_training_folder]/Marie/Training/Trainers"  # The value of SLURM_SUBMIT_DIR sets workdir to the directory
```
Following is the explanation on the purpose of each parameter. 

```
    "-d", "--dimension", help="dimension of embedding"
    "-lr", "--learning_rate", help="starting learning rate"
    "-g", "--gamma", help="gamma for scheduler"
    "-o", "--ontology", help="main ontology used"
    "-so", "--sub_ontology", help="name of the sub ontology (if you want to train multiple versions of embedding for one ontology)"
    "-bs", "--batch_size", help="size of mini batch"
    "-test", "--test_mode", help="if true, the training will use a smaller training set"
    "-proj", "--use_projection", help="if true, use projection in numerical linear regression"
    "-alpha", "--alpha", help="ratio between l_a and l_r"
    "-margin", "--margin", help="margin for MarginRankLoss"
    "-epoch", "--epoch", help="number of epochs"
    "-resume", "--resume", help="resume the training by loading embeddings "
    "-global_neg", "--global_neg", help="whether use all entities as negative samples"
    "-inference", "--inference", help="whether try to do inference with the ontology"
    "-gpu_num", "--gpu_number", help="number of gpus used"
```


## Relation Prediction

### File requirement

1. All the files required for embedding
2. `score_model_training.tsv`, see  [readme.md for dataset creation](./KGToolbox/readme.md) to create the file
   The files need to be placed in `MARIE_AND_BERT/DATA/CrossGraph/[ontology_name]/[sub-ontology_name]`


Relation prediction models can only be trained after the embeddings are trained. The following scripts can be used for training: 

1. `TransEScoreModelTrainer.py` for embeddings with TransE
2. `TransEAScoreModelTrainer.py` for embeddings with TransEA
3. `TransRAScoreModelTrainer.py` for embeddings with TransR and TransRA
4. `ComplexScoreModelTrainer.py` for embeddings wit Complex

Set up the parameters for training within the script and run the script. The parameters are as follows:

1. learning_rate 
2. model_name (output name of the model)
3. resume_training (whether resume from a checkpoint)
4. batch_size 
5. epoch_num
6. gamma
7. test_step (to test after how many epochs)
8. scheduler_step (change the learning rate after how many epochs)




## Score Alignment Model 

### File requirement
1. `cross_graph_alignment_training_updated.tsv`, see [readme.md for dataset creation](./KGToolbox/readme.md) to create this file

Run `MARIE_AND_BERT/Training/Trainers/CrossGraphTrainer.py` to train the model.

## Entity Linking
To create the training dataset for the entity linking model, please see [Entity Linking Dataset Creation readme.md](./EntityLinking/readme.md)
The training of the entity linking model uses the original BLINK library
while the operation of the trained model is optimised in this project.
To train the model, please follow the steps outlined in [Entity Linking Model Training](./EntityLinking/EL_training.md)

