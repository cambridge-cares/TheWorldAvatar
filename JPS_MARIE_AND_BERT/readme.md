# Marie and BERT (Marie 3.0)
The ``Marie and Bert`` a.k.a `Marie 3.0` project is developed by [Xiaochi Zhou](xz378@cam.ac.uk) and [Shaocong Zhang](sz375@cam.ac.uk).


[Kok Foong Lee](kflee@cmclinnovations.com) and [Michael Hillman](mdhillman@cmclinnovations.com) from CMCL are in charge of deployment. 

## Architecture
The project contains 5 parts: 
- the script and GUI for curating the dataset for **training and evaluation** (`./Dataset`)
- the scripts for training the four models of the Marie system (`./Training`)
- the scripts for knowledge graph embedding (`./Embedding`)
- the testing and logging components of the system (`./Tests`)
- the code for running the QA system  (`./Marie`)


## Dataset
The dataset is created by manually verbalizing automatically generated question templates via the GUI for dataset creation. 
The dataset is a set of about 1000 questions labelled with the head entity and the answer entity. The dataset is divided
into the ``training set``, ``validation set``, and the ``test set`` by the ratio of `7:2:1`. 

The ``training set`` and the `validataion set` are used for the training of the models and the ``test set`` 
is used for the evaluation of the individual models. More importantly, the ``test set`` is also used to evaluate the [overall performance](#overall-performance testing) 
of the QA system. 

## Training
This project requires the training of 3 models:
  - Entity linking model (`./Training/EntityLinking`)
  - KG embedding model (`./Training/Embedding`) * a more detailed document is in (`./Training/Embedding`)
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

The training of the models are automated by the classes ``EntityLinkingTrainer``, ``KGEmbedder``, and ``ScoringFunctionTrainer``. Each 
of the classes for training contains a function ``evaluate()``, which conducts individual performance evaluation of the model trained. 

The same as other code, the code for the training has development-level testing. 

## After-test model deployment
All the models are deployed to our ``Maven repository`` via script. The next section will explain when should the model be deployed and the versioning policy of the models will be explained in the "Version Control"
section. 

## Testing
The testing of the system includes 4 kinds: ``development-level``, ``individual model performance``, ``overall-performance``,``integrated test with GUI``. 

### Development-level testing 

Unit tests and overall tests are implemented on top of ``pytest``. Each module contains a `tests` folder that stores
the unit tests for the module. the ``./Tests``folder contains scripts for overall tests. The development-level testing 
makes sure the code functions as expected. The tests are performed through ``Pycharm IDE`` before releasing and through
docker image before deployment. 

### Individual model performance testing
The ``OverallEvaluator`` class performs evaluation for all the 3 individual models based on the 
``test set`` and produces ``recall``, ``precision``, and ``F1 score`` and save down the report. 

###Overall-performance testing

After the retraining of models, as multiple models are working together, it is possible that despite the improvement 
of the performance of individual model, the overall performance of the QA system drops. 

Therefore, the overall-performance testing is implemented under class ``OverallEvaluator``, which iterate through the test questions, answer the question with ``QAEngine``, and assert whether the 
correct answer is returned. A cumulative score is calculated.  Each time when a model's individual performance score is improved and the 
overall-performance testing score with this new model is improved, the model will be updated and deployed to ``Maven`` through class `ModelUploader`. 

### Integrated test with GUI
The GUIs are tested together with the backend by webpage automation on top of ``Selenium``. These tests
are also wrapped in ``pytest`` scripts. 


### When should the testing be performed  
Testing should be performed under the following circumstances:
- Expansion and changes are made to the training set (individual model performance and overall-performance)
- Before releasing a new version (all 4 kinds)
- Changes made to endpoints or other data source (development-level and overall-performance)
- The docker container should trigger an automated testing (overall-performance and development-level) after building. 


## Logging 
A logging server (`LoggingServer`) is implemented to monitor the activity of the deployed QA system, where the ``QAEngine`` sends the essential information to the logging server.
The logging server also provide information for the dashboard for the admins and developers to check the status of the endpoints by making handshakes to the endpoints and performing
a series of test query. 

The logging server also records user activities and store questions posed by users and their feedback (scoring of the answer). 

If any critical error is detected, the logging server will send the error message to the admins and developers via email. 

## Version Control

The version number of Marie 3.0 system releases follows the convention of ``3.x.y``. Where the major version number 
remains constant. The minor version number ``x`` increment every time any one of the models
get retrained, pass the overall performance test, and get deployed to Maven (so that the ).  

After the release, the corresponding docker image will be tagged with the version number and the 
``model version`` variable in the image will be updated. 

The version of the models follows a simple mechanism: each time any one of the 3 models is updated, the ``model version`` increment by 1 in `<model version>.0.0`. 

## Deployment
The ``Marie and Bert`` project is deployed based on a docker container, under folder `./Docker`

Each time the version number of the Marie release changed, a deployment will be done with the following steps:

1. update the docker image and tag it with the new version number of Marie.
2. push the docker image to the remote image repository (CMCL)
3. Admins from CMCL pulls the image, build the container, make sure the built-in tests are passed and start the container on the chosen machine. 

All the models that the QA system relies on are deployed to Maven in advance after each minor release. 


## Data storage

The data for training the ``entity linking model`` and ``embedding`` requires large storage space. As the training would most likely to happen in Singapore, we suggest that the data should be stored in ``Claudius``. 

The training question set that contains about 1000 questions is relatively small and is critical. As a result, we suggest that this dataset should be version controlled and committed to git. 

The trained models are deployed to TheWorldAvatar maven repository. 




## Measurement for robustness

### Datasource failure
It is likely that the data sources (e.g. SPARQL endpoint, agents) are deployed on different machines.
As a result, the failure or shutdown of other machines might cause the failure of the Marie system. 

Other than the dashboard showing the status of the data sources, we propose several possible solutions: 

1. Redundancy of datasource: For a distributed system, it is almost necessary to have redundancy to maintain the robustness and availability of the system 
2. Full copy of data sources dedicated for Marie: similar to the JPS_LDF server, a dedicated data source has been proven to increase the robustness of queries. 
3. Caching mechanism for keeping at least the example questions functional. 

### Ontology change 

By design, the new design is more robust against minor changes in the ontology. However, some changes within the ontology may still affect the Marie system:

1. IRI change for an entity/relation. Maybe a bulletin board for Ontology changes can help. `Ontology Lookup Service (OLS)` also provides a git-like system for tracing Ontology changes 
2. Introduction of new classes/relations: in theory, the system will be able to handle some of them but the accuracy will be significantly compromised. 
   1. The ``Entity Linking model`` can be easily updated without fine-tuned/retrained to handle the changes
   2. The ``KG Embedding`` needs to be fine-tuned if we chose `KG-BERT` but retrained if we chose others. 
   3. The ``Scoring model``is BERT-based as well; therefore only fine-tuning is required. 

3. Introduction of new entities requires no action. 





## Documentation

This ``readme.md`` file serves as the main document for documenting the project. 

A UML diagram is created to describe the class relations within the system, the UML diagram should be updated to reflect the changes 
of the code. 

All the main functions within the systems should be commented with the necessary information using Docstring following ``PEP 257 (Docstring Conventions)``. 
So that the detailed function document can be automatically created using ``Sphinx`` library. 


The following is an example of the Docstring for a function that get the columns in a spread sheet:

 ```python
 def get_spreadsheet_cols(file_loc, print_cols=False):
    # Gets and prints the spreadsheet's header columns
    # Parameters
    # ----------
    # file_loc : str
    #    The file location of the spreadsheet
    # print_cols : bool, optional
    #    A flag used to print the columns to the console (default is
    #    False)

    # Returns
    # -------
    # list
    #    a list of strings used that are the header columns
    

    file_data = pd.read_excel(file_loc)
    col_headers = list(file_data.columns.values)

    if print_cols:
        print("\n".join(col_headers))

    return col_headers```

