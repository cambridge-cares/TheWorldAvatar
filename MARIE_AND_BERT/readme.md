# Marie and BERT (Marie 3.0)

The ``Marie and Bert`` a.k.a `Marie 3.0` project is developed by [Xiaochi Zhou](xz378@cam.ac.uk) and [Shaocong Zhang](sz375@cam.ac.uk) and [Mehal Agarwal](ma988@cam.ac.uk).

A demonstration webpage is deployed at [Marie Website](http://159.223.42.53:5003/)

## Architecture

The project contains the following parts:
```
├── MARIE_AND_BERT
│   ├── DATA (for storing all binary files and large files) 
│   │   ├── CrossGraph 
│   │   ├── KG (for storing triple files of ontologies)
│   │   │   ├── [ontology names] (for all ontology-specific files) 
│   │   ├── Dictionaries (for all IRI-label/value mapping files)
│   │   ├── EntityLinking (for entity linking models)
│   │   ├── bert_pretrained (for local copy of bert pretrained models)
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
The dataset for training the KG embedding and models are mostly script created. To create or amend the dataset, see [readme.md for dataset creation.](./KGToolbox/readme.md)


## Training
Multiple models are trained in this system: 
1. Knowledge Graph Embedding
2. Relation Prediction
3. Score Alignment
4. Entity Linking 

For details on training the models, see [readme.md for embedding and model training](./Training/readme.md) and the [readme.md for entity linking training](./Training/EntityLinking/readme.md).
 
  
## Running 

Install `Python 3.8` and run `pip install requirements.txt`. It is recommended to use conda for creating a virtual environment. 

### Required files: 

1. Create a `DATA` folder under `MARIE_AND_BERT`.
2. Download CrossGraph.zip from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0), unzip under `DATA`.
3. Download Dictionaries.zip from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0), unzip under `DATA`.
4. For faster setup, download the `bert_pretrained.zip` from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0), unzip under `DATA`. Otherwise upon the first run, the scripts will download the BERT pretrained model from hugging face.
5. Download EntityLinking.zip from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0), unzip under `DATA`.
6. Download the required NLTK datasets by running
```python
import nltk
nltk.download('all')
```
This step is only required for development purpose, for the deployment of the system, downloading the NLTK datasets is not required. 


7. Download `label_dict.js` from [Dropbox folder](https://www.dropbox.com/sh/bslwl9mr32vz7aq/AAAFWNoYXg_p5V-iGcxZW0VOa?dl=0) and put the file in `MARIE_ANB_BERT/static/js`. 

To start the system, run `python main.py`.

For debugging purpose, in `main.py` please comment the line

```python
    app.run(host='0.0.0.0', debug=False, port=5003, threaded=False, processes=1)
```

and uncomment the line
```python
    # app.run(host='0.0.0.0', debug=True, port=5003)
```
### Recreation of the required files 

1. To recreate the files in `CrossGraph.zip`, please follow the steps in [Dataset creation](KGToolbox/readme.md) section `CrossGraph` to create the required files for training models and follow [Model training](Training/readme.md) to train the models, after dataset creation and embedding and model training, the `DATA/CrossGraph` folder will be populated. 

2. To recreate the files in `Dictionaries.zip`, please read [Dictionary creation](KGToolbox/readme.md) section `Dictionaries (DATA/Dictionaries folder)` to create the dictionary files. After the dictionary creation steps, the `DATA/Dictioanries` folder will be populated.

3. To recreate the files in `EntityLinking.zip`, please first follow the steps in [EntityLinking Dataset creation](KGToolbox/EntityLinking/readme.md) to create training dataset for Entity Linking training and the follow the steps in [EntityLinking Training](KGToolbox/EntityLinking/readme.md) to train the Entity Linking model. After the steps, the `DATA/EntityLinking` folder will be populated.

4. To recreate the files in `bert_pretrained.zip` 
```python
from pytorch_transformers.modeling_bert import BertModel
model = BertModel.from_pretrained('bert-base-uncased')
model.save_pretrained('/tmp/directory/for/models/')
```
The user will need to change `/tmp/directory/for/models` to their folder of choice and move the files to `DATA/bert_pretrained`

5. By following [Dictionary creation](KGToolbox/readme.md) section `Dictionaries (DATA/Dictionaries folder)` subsection `Global dictionary (static/js/label_dict.js)`, the `label_dict.js` file will be created in `static/js/`

  
### Other Services
To run the full functions of the Marie system, three other systems are required:

1. The LDF server. See [LDF server readme](../JPS_LDF/README.md) to run it. 
2. The semantic agents. See [PCE Agent readme](../Agents/PCEAgent/README.md) and [STDC Agent readme](../Agents/STDCThermoAgent/README.md) to create docker containers running them. 

 
## Docker Deployment

### Deploying the Marie system 

For deployment of the Marie system: 

1. Assume the user cloned the `TheWorldAvatar` git repository under `/home/user1/Marie`
2. Load the files 2-5 in [Required files](#required-files) in `/home/user1/Marie/TheWorldAvatar/MARIE_AND_BERT/DATA`
3. Load `label_dict.js` in [Required files](#required-files) in `/home/user1/Marie/TheWorldAvatar/MARIE_AND_BERT/static/js`
4. To spin up the Marie container, use `docker build --no-cache -t marie3 .` to build the image and run `docker run -p 5003:80  --dns=8.8.8.8 -d marie3:latest`

The Marie web-interface will then be available at `http://localhost:5003` or `http://127.0.0.1:5003`

At this point, the Marie system will be running its full functionality but use the LDF server and Agent systems deployed on `http://159.223.42.53`
The deployment requires at least 16 GB of memory allocated to docker. The building of the local image might take over an hour depending on the internet speed. 


### Deploying LDF server and the Agents system  

To deploy the local LDF server (For reaction queries) and the Agents system (For agent queries)

1. Created a folder `/home/user1/Marie/TheWorldAvatar/MARIE_AND_BERT/DATA/KG` . Create `ontospecies.nt` and `ontocompchem.nt` using
```
python KGToolbox/SPARQLEndpoint/export_triples.py 
--endpoint http://www.theworldavatar.com/blazegraph/namespace/copy_ontospecies_marie 
--output_filename ontospecies.nt
```
and
```
python KGToolbox/SPARQLEndpoint/export_triples.py 
--endpoint http://www.theworldavatar.com/blazegraph/namespace/ontocompchem 
--output_filename ontocompchem.nt
```

The script needs to be run under `/home/user1/Marie/TheWorldAvatar/MARIE_AND_BERT` and the files will be created under `MARIE_AND_BERT/DATA/KG`.

2. Build a blazegraph image, see [Blazegraph container](https://github.com/lyrasis/docker-blazegraph#local-builds) for instructions. 
Start the container with `docker run --volume=/home/user1/Marie/MARIE_AND_BERT/DATA/KG:/triples d--name blazegraph:2.1.5 -d -p [port]:[port] blazegraph-marie`

3. Use the blazegraph GUI/API to create and upload namespaces. Upload `ontospecies.nt` to namespace `ontospecies_old`. Upload `ontocompchem.nt` to namespace `ontocompchem`.
For example, to upload with GUI update page, key in url  `/triples/ontospecies.nt`, then press upload.

4. `cd /home/user1/Marie/TheWorldAvatar/JPS_LDF`, run `docker compose up -d` to start the LDF server ([LDF server readme](../JPS_LDF/README.md))

5. `cd /home/user1/Marie/TheWorldAvatar/Agents/STDCThermoAgent`, run `docker compose up -d`

6. `cd /home/user1/Marie/TheWorldAvatar/Agents/PCEAgent`, run `docker compose up -d`

7. Change `http://159.223.42.53:3000/ldfserver/ontokin` to `[you local ip address for LDF server]:3000/ldfserver/ontokin` in 
`MARIE_AND_BERT/Marie/Util/LDFTools/LdfRequest.py`, the local ip address is usually `http://localhost` or `http://127.0.0.1`

8. Change `http://159.223.42.53:5000/api/model/predict?` to `[you local ip address for pce agent]:5000/api/model/predict?` in 
function `invoke_pce_agent`in `MARIE_AND_BERT/Marie/Util/AgentTools/agent_invoker.py` , the local ip address is usually `http://localhost` or `http://127.0.0.1`

9. Change `http://159.223.42.53:5001/api/thermoagent/calculate?` to `[you local ip address for thermo agent]:5001/api/thermoagent/calculate?` in 
function `invoke_thermo_agent`in `MARIE_AND_BERT/Marie/Util/AgentTools/agent_invoker.py` , the local ip address is usually `http://localhost` or `http://127.0.0.1`

10. Follow the steps in [Deploying the Marie system](#deploying-the-marie-system) to build and run the Marie system. 

## Frontend development
It takes more than 15 minutes to spin up the Marie server, as a result, a `mock_main.py` script is implemented to provide mock backend responses to support frontend development. By repeatedly clicking any of the the example questions on the webpage hosted by the mock server, the user can check the visualisation of the six different response types. 

To run the `mock_main.py` a file `chatgpt_api_key.txt` storing the ChatGPT api key should be available under `DATA`. Please go to [ChatGPT](https://openai.com/blog/chatgpt)
to obtain an API key for using ChatGPT. 


 





 