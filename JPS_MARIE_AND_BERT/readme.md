## Marie and BERT (Marie 3.0)
The ``Marie and Bert`` a.k.a `Marie 3.0` project is developed by Xiaochi Zhou (xz378@cam.ac.uk) and Shaocong Zhang (sz375@cam.ac.uk). 

Someone from CMCL for deployment. 

### Architecture
The project contains 5 parts: 
- the script and GUI for curating the dataset for **training and evaluation** (`./Dataset`)
- the scripts for training the four models of the Marie system (`./Training`)
- the scripts for knowledge graph embedding (`./Embedding`)
- the testing and logging components of the system (`./Tests`)
- the code for running the QA system  (`./Marie`)


### Dataset Creation
The dataset is a set of questions with entities 

### Training
This project requires the training of 3 models:
  - Entity linking model (`./Training/EntityLinking`)
  - KG embedding model (`./Training/Embedding`)
  - Scoring model for candidate selection (`./Training/Scoring`)
 
The retraining of the  `entity linking model` should be triggered by any of the following events:

- The amendment or expansion of the training dataset
- The integration of one or more new ontologies
- The renaming of a substantial number of entities

The retraining of the ``KG embedding model`` should be triggered when:
- Significant structural changes to the KG
- Significant expansion of the KG

The retraining of the ``Scoring model`` should be triggered when: 
- The amendment or expansion of the training dataset
- The integration of one or more new ontologies
- The retraining of the ``KG embedding model``

The training of the models are automated by scripts 

TODO: Evaluation of the training, tests for the training code 

### Testing
The testing of the system includes 3 parts: 
#### Development-level testing 

#### Model-compatibility testing 

#### Model-performance testing 

TODO: development-level testing (code) and testing for retrained models. 

Unit tests and overall tests are implemented on top of ``pytest``. Each module contains a `tests` folder that stores
the unit tests for the module. the ``./Tests``folder contains scripts for overall tests.

The GUIs are tested together with the backend by webpage automation on top of ``Selenium``. These tests
are also wrapped in ``pytest`` scripts. 

The automated testing should be performed under the following circumstances:
- Before releasing a new version 
- Periodic testing of the system (weekly?)
- Changes made to endpoints or other data source 

The docker container should trigger an automated testing when deployed. 

CI with GitHub Actions?...

### Logging 
error reports, testing results, user activities ... 
error reports sent to developers and admins...

A dashboard showing the status of the endpoints. 

### Version Control


Version numbers ... git tags ...



### Deployment
The ``Marie and Bert`` project is deployed based on a docker container, under folder `./Docker`


### Data storage 

The training of the models require quite some data... where should they be ... 

The trained models are deployed to TheWorldAvatar maven repository. 


### Documentation


