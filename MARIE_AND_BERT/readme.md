# Marie and BERT (Marie 3.0)

The ``Marie and Bert`` a.k.a `Marie 3.0` project is developed by [Xiaochi Zhou](xz378@cam.ac.uk)
and [Shaocong Zhang](sz375@cam.ac.uk) and [Mehal Agarwal](mehal.agarwal@cares.cam.ac.uk).

[Kok Foong Lee](kflee@cmclinnovations.com) and [Michael Hillman](mdhillman@cmclinnovations.com) will provide technical support from CMCL and will be charge of deployment and monitoring of performance.

## Architecture

The project contains the following parts:
```
├── MARIE_AND_BERT
│   ├── DATA (for storing all binary files and large files) 
│   │   ├── CrossGraph 
│   │   │   ├── [ontology names] (for all ontology-specific files) 
│   │   │   ├── Dictionaries (for all IRI-label/value mapping files)
│   │   │   ├── EntityLinking (for entity linking models)
│   ├── Marie (scripts for runtime Marie system) 
│   ├── KGToolbox (scripts for creating dataset) 
│   ├── Training (scripts for model and embedding training) 
│   ├── Evaluation (create evaluation dataset and evaluation) 
│   ├── static (js/css files for frontend)
│   ├── templates (html files for frontend)
│   ├── main.py (flask server)
│   ├── Dockerfile 
│   ├── requirements.txt 
```

## Dataset creation 
The dataset for training the KG embedding and models are mostly script 
created. To create or amend the dataset, see [readme.md for dataset creation](./KGToolbox/readme.md)


## Training
Multiple models are trained in this system: 
1. Knowledge Graph embedding
2. Relation prediction
3. Score alignment
4. Entity Linking 

for details of training the models, see [readme.md for embedding and model training](./Training/readme.md)
 
  
## Running 

Install `Python 3.8` and Run `pip install requirements.txt`. It is recommended 
to use conda for creating a virtual environment. 

Required files: 

1. Create a `DATA` folder under `MARIE_AND_BERT`
2. Download CrossGraph.zip from [CrossGraph.zip](http://159.223.42.53:8080/CrossGraph.zip), unzip under `DATA`
3. Download EntityLinking.zip from [EntityLinking.zip](http://159.223.42.53:8080/EntityLinking.zip), unzip under `DATA`
4. For faster setup, download the `bert_pretrained.zip` from [bert_pretrained.zip](http://159.223.42.53:8080/bert_pretrained.zip)
, unzip under `DATA`. Otherwise upon the first run, the scripts will download the BERT pretrained model from hugging face.
5. Download EntityLinking.zip from [EntityLinking.zip](http://159.223.42.53:8080/EntityLinking.zip), unzip under `DATA`
6. Download STOUT V2 model [STOUT V2 model](http://159.223.42.53:8080/models.zip) for Linux system, unzip into `root/.data/STOUT-V2/`, for Windows system,
 
To start the system, run `python main.py`.

 




 