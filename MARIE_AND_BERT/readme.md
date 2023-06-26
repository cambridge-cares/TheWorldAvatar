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

###Required files: 

1. Create a `DATA` folder under `MARIE_AND_BERT`
2. Download CrossGraph.zip from [CrossGraph.zip](http://159.223.42.53:8080/CrossGraph.zip), unzip under `DATA`
3. Download EntityLinking.zip from [EntityLinking.zip](http://159.223.42.53:8080/EntityLinking.zip), unzip under `DATA`
4. For faster setup, download the `bert_pretrained.zip` from [bert_pretrained.zip](http://159.223.42.53:8080/bert_pretrained.zip)
, unzip under `DATA`. Otherwise upon the first run, the scripts will download the BERT pretrained model from hugging face.
5. Download EntityLinking.zip from [EntityLinking.zip](http://159.223.42.53:8080/EntityLinking.zip), unzip under `DATA`
6. Download STOUT V2 model [STOUT V2 model](http://159.223.42.53:8080/models.zip) for Linux system, unzip into `root/.data/STOUT-V2/`, for Windows system,
7. Download the required NLTK datasets by running
```
import nlkt
nltk.download('all')
```
 
To start the system, run `python main.py`.

###Other services
To run the full functions of the Marie system, three other systems are required:

1. The LDF server. See [LDF server readme](../JPS_LDF/README.md) to run it. 
2. The semantic agents. See [PCE agent readme](../Agents/PCEAgent/README.md) 
[Thermal agent readme](../Agents/STDCThermoAgent/README.md) to run them. 


## Docker Deployment

For local deployment，please use `Dockerfile_local`. 
1. run `docker build  --no-cache -t marie .d -f Dockerfile_local .` to build the image
2. run ` docker run -p 5003:80 -d marie:latest`. 

For deployment on Linux server from the scratch:

1. create a folder in the server, assume it is `/home/user1/Marie/DATA`
2. Load the files 2-6 mentioned in [Required files](###Required Files) in `/home/user1/Marie/DATA`
3. Clone the GitHub repository by `git clone https://github.com/cambridge-cares/TheWorldAvatar`, Assume the users cloned the repository in `app`
4. Set up a blazegraph container, see [Blazegraph container](https://github.com/lyrasis/docker-blazegraph) for instructions
5. 
6. `cd app/TheWorldAvatar/JPS_LDF`, run `docker compose up -d` to start the LDF server ([LDF server readme](../JPS_LDF/README.md))
7. `cd app/TheWorldAvatar/Agents/STDCThermoAgent`, run `docker compose up -d`
8. `cd app/TheWorldAvatar/Agents/PCEAgent`, run `docker compose up -d`




 





 