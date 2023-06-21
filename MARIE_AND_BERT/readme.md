# Marie and BERT (Marie 3.0)

The ``Marie and Bert`` a.k.a `Marie 3.0` project is developed by [Xiaochi Zhou](xz378@cam.ac.uk)
and [Shaocong Zhang](sz375@cam.ac.uk).

[Kok Foong Lee](kflee@cmclinnovations.com) and [Michael Hillman](mdhillman@cmclinnovations.com) will provide technical support from CMCL and will be charge of deployment and monitoring of performance.

## Architecture

The project contains the following parts:

- a script and GUI for curating the dataset for **training and evaluation** (`./Dataset`)
- scripts for training the four models of the Marie system (`./Training`)
- scripts for knowledge graph embedding (`./Embedding`)
- testing and logging components of the system (`./Tests`)
- code for running the QA system (`./Marie`)
- a frontend to allow web-based interaction with the QA system (`./Website`)


## Training

This project requires the training of 3 models:

- An entity linking model (`./Training/EntityLinking`)
- A KG embedding model (`./Training/Embedding`) [a more detailed document is in `./Training/Embedding`]
- A scoring model for candidate selection (`./Training/Scoring`)

The `entity linking model` should be retrained after any of the following events:

- The amendment or expansion of the training dataset
- The integration of one or more new ontologies (i.e. new terminology and associated data)
- The renaming of a substantial number of entities (instances, but also concepts)

The ``KG embedding model`` should be retrained after any of the following events:

- Significant structural changes to the KG
- Significant expansion of the KG (i.e. more data)

The ``scoring model`` should be retrained after any of the following events:

- The amendment or expansion of the training dataset
- The integration of one or more new ontologies (i.e. new terminology and associated data)
- The retraining of the ``KG embedding model``

The training of the models are automated by the classes ``EntityLinkingTrainer``, ``KGEmbedder``,
and ``ScoringFunctionTrainer``. Each of the classes for training contains a function ``evaluate()``, which conducts
individual performance evaluation of the model trained.

The same as other code, the code for the training has development-level testing.

### Environment

``Python 3.7 + tensorflow 2.1.0 + cuda 10.1 + CuDnn 7.6``


## After-test model deployment

All the models are deployed to our ``Maven repository`` via script. The next section will explain when should the model
be deployed. The versioning policy of the models will be explained in the "Version Control"
section.

## Testing

The testing of the system includes 5 types of test: ``development-level``, ``individual model performance``
, ``overall-performance``, ``integrated test with website`` and ``performance monitoring tests``.

### Development-level testing

Purpose: To confirm that code functions as expected.

Unit tests and overall tests are implemented on top of ``pytest``. Each module contains a `tests` folder that stores the
unit tests for the module. the ``./Tests``folder contains scripts for overall tests. The tests are performed through ``Pycharm IDE`` before releasing and through docker
image before deployment.

### Individual model performance testing

Purpose: To evaluate the performance of individual models in isolation from the other components of the system.

The ``OverallEvaluator`` class evaluates the entity linking model, KG embedding model and scoring model based on the
``test set`` and produces ``recall``, ``precision``, and ``F1 score``. The results are saved in a report.

### Overall-performance testing

Purpose: To evaluate the system performance (i.e. the combination of all individual models). This is necessary because it is possible that the overall performance of the QA system may be adversely affected after retraining, despite an improvement in the performance of some individual models.

The overall-performance testing is implemented under class ``OverallEvaluator``. This iterates through the
test questions, answer each question using the ``QAEngine``, and asserts whether the correct answer is returned. A cumulative
score is calculated.

The model should be updated and deployed to ``Maven`` using class `ModelUploader` each time a model's individual performance score and the overall-performance score improve.

### Integrated test with website

Purpose 1: To confirm that communication between the frontend and backend functions as expected.
Purpose 2: To confirm that the backend handles concurrency problem correctly. 

The frontend and backend are tested by webpage automation on top of ``Selenium``. The tests also include 
concurrency tests. These tests are also wrapped
in ``pytest`` scripts.




### Performance monitoring tests

Purpose: To monitor ongoing performance after deployment. (On a different server)

The deployed product depends on a series of data sources. The most straight forward 
solutions is to have a script that runs through a list of test questions and confirm 
that the deployed backend returns the correct answer. 

Run through a list of test questions and send emails when failure happens. (Add a hook to the Logwatch.)

### When should the testing be performed

Testing should be performed under the following circumstances:

- After any changes to the training set (individual model performance and overall-performance)
- Before releasing a new version (all tests)
- Changes made to endpoints or other data source (development-level and overall-performance)
- The docker container should trigger automated testing (overall-performance and development-level) after building.

## Logging

A logging server (`LoggingServer`) is implemented to monitor the activity of the deployed QA system, where
the ``QAEngine`` sends the essential information to the logging server. The logging server also provide information for
a dashboard that can be used by admins and developers to check the status of the endpoints by making handshakes to the endpoints
and performing a series of test query.

The logging server also records user activities and store questions posed by users and their feedback (scoring of the
answer).

If any critical error is detected, the logging server will send the error message to the admins and developers via
email.

## Version Control

The version number of Marie 3.0 system releases follows the convention of ``3.x.y``. Where the major version number
remains constant. The minor version number ``x`` should increment every time a model is retrained, passes the
relevant tests and is deployed to Maven.

After release, the corresponding docker image will be tagged with the version number and the
``model version`` variable in the image will be updated.

The version of the models follows a simple mechanism: each time any one of the models is updated,
the ``model version`` increment by 1 in `<model version>.0.0`.

## Deployment

The ``Marie and Bert`` project is deployed based on a docker container, under folder `./Docker`

Each time the version number of the Marie release is changed, the project will be deployed as per the following steps:

1. Update the docker image and tag it with the new version number of Marie.
2. Push the docker image to the remote image repository (CMCL).
3. Admins from CMCL pull the image, build the container, confirm that the built-in tests pass, and start the
   container on the chosen machine.

All the models that the QA system relies on are deployed to Maven after each minor release.

## Data storage

The data for training the ``entity linking model`` and ``embedding`` requires significant storage space. The training data will be stored in Dropbox as one single zip file. 

The training question set that contains about 1000 questions is relatively small and is critical. As a result, we
suggest that this dataset should be version-controlled and committed to git.

The trained models are deployed to TheWorldAvatar Maven repository.

## Measurement for robustness

### Datasource failure

It is likely that the data sources (e.g. SPARQL endpoint, agents) are deployed on different machines. As a result, the
failure or shutdown of other machines might cause the failure of the Marie system.

Other than the dashboard showing the status of the data sources, we propose several possible solutions:

1. Redundancy of datasource: For a distributed system, it is almost necessary to have redundancy to maintain the
   robustness and availability of the system
2. Full copy of data sources dedicated for Marie: similar to the JPS_LDF server, a dedicated data source has been proven
   to increase the robustness of queries.
3. Caching mechanism for keeping at least the example questions functional.

### Ontology change

By design, the new design is more robust against minor changes in the ontology. However, some changes within the
ontology may still affect the Marie system:

1. IRI change for an entity/relation might cause inconsistency between the data source for `Subgraph Extraction` and the
   models might cause inaccurate answers (e.g. the `Entity linking model` might give an out-of-date IRI and
   the `Subgraph Extraction` module will fail to extract the subgraph). Maybe a bulletin board for Ontology changes can
   help. `Ontology Lookup Service (OLS)` also provides a git-like system for tracing Ontology changes. As long as the `Entity linking model`
has its mapping updated (which is easy), the system will run as usual. 

2. Introduction of new classes/relations: In theory, the system will be able to handle some of them but the accuracy
   will be significantly compromised.
    1. The ``Entity Linking model`` can be easily updated without fine-tuned/retrained to handle the changes
    2. The ``KG Embedding`` needs to be fine-tuned if we chose `KG-BERT` but retrained if we chose others.
    3. The ``Scoring model``is BERT-based as well; therefore only fine-tuning is required.

3. Introduction of new entities requires fine-tuning/retraining of the ``KG Embedding``, updating of
   the `Entity linking` internal ID-to-IRI mapping. For optimal results, the `Scoring function` requires retraining.

## Documentation

This ``readme.md`` file serves as the main document for documenting the project.

A UML diagram is created to describe the class relations within the system, the UML diagram should be updated to reflect
the changes of the code.

All the main functions within the systems should be commented with the necessary information using Docstring
following ``PEP 257 (Docstring Conventions)``. So that the detailed function document can be automatically created
using ``Sphinx`` library.

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

{'batch_size': 44, 'ent_hidden_size': 95, 'epochs': 10, 'l1_flag': False, 'learning_rate': 0.006926634126597585, 'margin': 0.081367834713
45943, 'optimizer': 'sgd', 'rel_hidden_size': 249}
