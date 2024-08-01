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

## Cuda configuration
The training scripts are compatible with CPU-based training but if the user wants to training the embedding and models 
locally using GPUs, see [PyTorch CUDA](https://pytorch.org/get-started/locally/) to set up the CUDA environment. 

##  Knowledge Graph Embedding
 
### File requirement
To train the embedding of an ontology, a list of files are required. The files are script created, see [readme.md for dataset creation](../KGToolbox/readme.md) for what 
files are expected for the training. 

Once all the files are created, choose the embedding method, the implemented ones are 
`Complex`, `TransE`, `TransEA`,`TransR`, `TransRA`. The preferred embedding method for a typical TWA ontology is `TransRA`.
In folder `MARIE_AND_BERT/Training/Trainers`, `TransRATrainer.py` handles both `TransR` and `TransRA`. `ComplexTrainer.py` handles
`Complex`, `TransEATrainer.py` handles `TransE` and `TransEA`. 

### Local embedding training

#### Pubchem (CrossGraph/pubchem)

1. Under `Training/Trainers` run `python TransEATrainer.py -d 20 -lr 0.01 -bs 32 -o pubchem -epoch 500 -margin 5`, when the training is done, `ent_embdding.tsv`
and `rel_embedding.tsv` will be created under `DATA/CrossGraph/pubchem`

#### OntoKin (CrossGraph/ontokin)

1. Under `Training/Trainers` run `python TransEATrainer.py -d 80 -lr 0.01 -bs 32 -o ontokin -epoch 500 -margin 5`, when the training is done, `ent_embdding.tsv`
and `rel_embedding.tsv` will be created under `DATA/CrossGraph/ontokin`

#### OntoMoPs (CrossGraph/OntoMoPs)

1. Under `Training/Trainers` run `python TransRATrainer.py -d 50 -lr 0.01 -bs 32 -o OntoMoPs -so numerical_with_implicit -epoch 200 -proj no -alpha 0.01`, 
when the training is done, `ent_embdding.tsv`, `rel_embedding.tsv`, `proj_matrix.tsv`,`attr_embedding.tsv`,`bias_embedding.tsv` will be created under `DATA/CrossGraph/OntoMoPs/numerical_with_implicit`

#### OntoSpecies (CrossGraph/ontospecies_new)

1. Under `Training/Trainers` run `python TransRATrainer.py -d 50 -lr 0.01 -bs 32 -o ontospecies_new -so base_full_no_pref_selected_role_limited_100 -epoch 200 -proj no -alpha 0.01 -global_neg no`, 
when the training is done, `ent_embdding.tsv`, `rel_embedding.tsv`, `proj_matrix.tsv`,`attr_embedding.tsv`,`bias_embedding.tsv` will be created under `DATA/CrossGraph/ontospecies_new/base_full_no_pref_selected_role_limited_100`

#### OntoCompChem (CrossGraph/ontocompchem)

1. Under `Training/Trainers` run `python ComplexTrainer.py -d 80 -lr 0.01 -bs 32 -o ontocompchem -global_neg yes -is_numerical no -epoch 500`, 
when the training is done, `ent_embdding.tsv`, `rel_embedding.tsv`, will be created under `DATA/CrossGraph/ontocompchem`

#### Wikidata (CrossGraph/wikidata_numerical)

1. Under `Training/Trainers` run `python TransRATrainer.py -d 40 -lr 0.01 -bs 32 -o wikidata_numerical -global_neg yes -epoch 500 -proj no -alpha 0.01`, 
when the training is done, `ent_embdding.tsv`, `rel_embedding.tsv`, `proj_matrix.tsv`,`attr_embedding.tsv`,`bias_embedding.tsv`, will be created under `DATA/CrossGraph/wikidata_numerical`

#### Agents (CrossGraph/agents)

1. Under `Training/Trainers` run `python TransRATrainer.py -d 40 -lr 0.01 -bs 4 -o agents -so ontopceagent -global_neg yes -epoch 200 -proj no`, 
when the training is done, `ent_embdding.tsv`, `rel_embedding.tsv`, `proj_matrix.tsv`,`attr_embedding.tsv`,`bias_embedding.tsv`, will be created under `DATA/CrossGraph/agents/ontopceagent`

2. Under `Training/Trainers` run `python TransRATrainer.py -d 40 -lr 0.01 -bs 4 -o agents -so ontothermoagent -global_neg yes -epoch 200 -proj no`, 
when the training is done, `ent_embdding.tsv`, `rel_embedding.tsv`, `proj_matrix.tsv`,`attr_embedding.tsv`,`bias_embedding.tsv`, will be created under `DATA/CrossGraph/agents/ontothermoagent`

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

### Embedding training on HPC

Assume the user is to train the embeddings on the Cambridge HPC:

1. Get an HPC account, use git to pull the `MARIE_AND_BERT` repository in your working directory, assume it is `Work`
2. Create and transfer the files required for training under `Work/MARIE_AND_BERT/DATA/[ontology_name]/[sub-ontology_name]`
3. Create a python virtual environment with `Python 3.8`, assume it is named `marie` 
4. Activate the virtual environment and run `pip install requirements.txt` under `MARIE_AND_BERT`
5. The specific batch files used for training the embeddings of each ontology are provided under `Training/HPC`, to use them the 
user needs to change the workdir parameter to their work directory in `workdir="/home/xz378/FULL_TEST/Training/Trainers" ` 
and the virtual environment name in `source ~/marie/bin/activate` 
6. Submit the batch script by `sbatch [batch_file_name]`

For more details, please check [https://docs.hpc.cam.ac.uk/hpc/](https://docs.hpc.cam.ac.uk/hpc/). 



## Relation Prediction

This step trains the BERT-based model for relation prediction and operator prediction. By following the steps below,
the user will see `bert_[ontology_name]` or `bert_[ontology_name]_operator/bert_[ontology_name]_predict` created 
under the ontology folders or sub-ontology folders.

### File requirement

1. All the files required for embedding and the trained embedding files. 
2. `score_model_training.tsv`, see  [readme.md for dataset creation](./KGToolbox/readme.md) to create the file
   The files need to be placed in `MARIE_AND_BERT/DATA/CrossGraph/[ontology_name]/[sub-ontology_name]` if there is a 
sub ontology folder, otherwise, the files need to be placed in `CrossGraph/[ontology_name]`
 
Relation prediction models can only be trained after the embeddings are trained. The following scripts can be used for training: 

### Pubchem (CrossGraph/pubchem)

1. Under `Training/Trainers` run `python TransEScoreModelTrainer.py -o pubchem -bs 16 --mode prediction --gamma 0.1`

### OntoKin (CrossGraph/ontokin)

1. Under `Training/Trainers` run `python TransEScoreModelTrainer.py -o ontokin -bs 16 --mode prediction --gamma 0.1`

### OntoMoPs (CrossGraph/OntoMoPs)
1. Under `Training/Trainers` run `python TransRAScoreModelTrainer.py -lr 1e-6 -o OntoMoPs -so numerical_with_implicit -g 1 -epoch 300 -m prediction`
2. Under `Training/Trainers` run `python TransRAScoreModelTrainer.py -lr 1e-6 -o OntoMoPs -so numerical_with_implicit -g 1 -epoch 300 -m operator -dict smaller-larger-none`


### OntoSpecies (CrossGraph/ontospecies_new)
 
1. Under `Training/Trainers` run `python TransRAScoreModelTrainer.py -lr 1e-6 -o ontospecies_new -so base_full_no_pref_selected_role_limited_100 -g 1 -epoch 300 -m prediction`
2. Under `Training/Trainers` run `python TransRAScoreModelTrainer.py -lr 1e-6 -o ontospecies_new -so base_full_no_pref_selected_role_limited_100 -g 1 -epoch 300 -m operator -dict smaller-larger-none`


### OntoCompChem (CrossGraph/ontocompchem)

1. Under `Training/Trainers` run `python ComplexScoreModelTrainer.py -o ontocompchem`.  

### Wikidata (CrossGraph/wikidata_numerical)

1. Under `Training/Trainers` run `python TransRAScoreModelTrainer.py -lr 1e-6 -o wikidata_numerical -g 1 -epoch 60 -m joint -dict smaller-larger-about-none -bs 32`

### Agents (CrossGraph/agents)

1. Under `Training/Trainers` run  `python TransEScoreModelTrainer.py -o agents -bs 16  --gamma 0.1 -m agent -epoch 500 -lr 1e-5 -g 0.01`

## Score Alignment Model 

1. Under `Training/Trainers` run `python CrossGraphTrainer.py`

## Entity Linking
To create the training dataset for the entity linking model, please see [Entity Linking Dataset Creation readme.md](./EntityLinking/readme.md)
The training of the entity linking model uses the original BLINK library
while the operation of the trained model is optimised in this project.
To train the model, please follow the steps outlined in [Entity Linking Model Training](./EntityLinking/readme.md)

