# Marie and BERT (Marie 3.0)

The ``Marie and Bert`` a.k.a `Marie 3.0` project is developed by [Xiaochi Zhou](xz378@cam.ac.uk)
and [Shaocong Zhang](sz375@cam.ac.uk) and [Mehal Agarwal](ma988@cam.ac.uk).

A demonstration webpage is deployed at [Marie Website](http://159.223.42.53:5003/)

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
1. Knowledge Graph Embedding
2. Relation Prediction
3. Score Alignment
4. Entity Linking 

For details on training the models, see [readme.md for embedding and model training](./Training/readme.md) and the [readme.md for entity linking training](./Training/EntityLinking/EL_training.md)
 
  
## Running 

Install `Python 3.8` and Run `pip install requirements.txt`. It is recommended 
to use conda for creating a virtual environment. 

### Required files: 

1. Create a `DATA` folder under `MARIE_AND_BERT`
2. Download CrossGraph.zip from [CrossGraph.zip](http://159.223.42.53:8080/CrossGraph.zip), unzip under `DATA`
3. Download Dictionaries.zip from [Dictionaries.zip](http://159.223.42.53:8080/Dictionaries.zip), unzip under `DATA`
4. For faster setup, download the `bert_pretrained.zip` from [bert_pretrained.zip](http://159.223.42.53:8080/bert_pretrained.zip)
, unzip under `DATA`. Otherwise upon the first run, the scripts will download the BERT pretrained model from hugging face.
5. Download EntityLinking.zip from [EntityLinking.zip](http://159.223.42.53:8080/EntityLinking.zip), unzip under `DATA`
6. Download STOUT V2 model [STOUT V2 model](http://159.223.42.53:8080/models.zip) for Linux system, unzip into `root/.data/STOUT-V2/`, for Windows system,
7. Download `label_dict.js` from [label_dict.js] from (http://159.223.42.53:8080/label_dict.js) and put the file in `MARIE_ANB_BERT/static/js`.
8. Download the required NLTK datasets by running
```python
import nlkt
nltk.download('all')
```
 
To start the system, run `python main.py`.

For debugging purpose, in `main.py` please comment the line

```python
    app.run(host='0.0.0.0', debug=False, port=5003, threaded=False, processes=1)
```

and uncomment the line
```python
    # app.run(host='0.0.0.0', debug=True, port=5003)
```

### Other Services
To run the full functions of the Marie system, three other systems are required:

1. The LDF server. See [LDF server readme](../JPS_LDF/README.md) to run it. 
2. The semantic agents. See [PCE agent readme](../Agents/PCEAgent/README.md) 
[Thermal agent readme](../Agents/STDCThermoAgent/README.md) to create docker containers running them. 


## Docker Deployment

For local deployment，please use `Dockerfile_local`. 
1. run `docker build  --no-cache -t marie .d -f Dockerfile_local .` to build the image
2. run ` docker run -p 5003:80 -d marie:latest`. 
3. The Marie web-interface will then be available at `http://localhost:5003` or `http://127.0.0.1:5003`

For deployment on Linux server from scratch:

1. Create a folder in the server, assume it is `/home/user1/Marie`
2. Clone the GitHub repository by `git clone https://github.com/cambridge-cares/TheWorldAvatar`, Assume the users cloned the repository in `/home/user1/Marie`
3. Load the files 2-7 mentioned in [Required files](#required-files) in `/home/user1/Marie/TheWorldAvatar/MARIE_AND_BERT/DATA`
4. Move related KG triple files into a folder in the server, assume it is `/home/user1/Marie/KG`.
5. Build a blazegraph image, see [Blazegraph container](https://github.com/lyrasis/docker-blazegraph#local-builds) for instructions. Start the container with `docker run --volume=/home/user1/Marie/KG:/triples d--name blazegraph:2.1.5 -d -p [port]:[port] blazegraph-marie`
6. Create `ontospecies.nt` and `ontocompchem.nt` using 

```
python KGToolbox/SPARQLEndpoint/export_triples.py 
--output_dir ontospecies 
--endpoint http://www.theworldavatar.com/blazegraph/namespace/copy_ontospecies_marie 
--output_filename ontospecies.nt
```

and 

```
python KGToolbox/SPARQLEndpoint/export_triples.py 
--output_dir ontocompchem 
--endpoint http://www.theworldavatar.com/blazegraph/namespace/ontocompchem 
--output_filename ontocompchem.nt
```


7. Use the blazegraph GUI/API to create namespaces and upload. Upload `ontospecies.nt` to namespace `ontospecies_old`. Upload `ontocompchem.nt` to namespace `ontocompchem`.

For example, to upload with GUI update page, key in url  `/triples/ontospecies.nt`, then press upload.
8. `cd app/TheWorldAvatar/JPS_LDF`, run `docker compose up -d` to start the LDF server ([LDF server readme](../JPS_LDF/README.md))
9. `cd app/TheWorldAvatar/Agents/STDCThermoAgent`, run `docker compose up -d`
10. `cd app/TheWorldAvatar/Agents/PCEAgent`, run `docker compose up -d`
11. To spin up the Marie container, use `docker build --no-cache -t marie3 .` to build the image and run 
`docker run -p 5003:80  --dns=8.8.8.8 -d marie3:latest`


## Frontend development
It takes more than 15 minutes to spin up the Marie server, as a result, a `mock_main.py` script is implemented to 
provide mock backend responses to support frontend development. To test different types of responses, please
follow the instructions at the top part of the `mock_main.py` script. 


 





 