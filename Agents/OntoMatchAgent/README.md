# Description #

This Python code implements agents used for instance matching in the preprint [A Simple and Effective Approach to Unsupervised Instance Matching and its Application to Linked Data of Power Plants](https://como.ceb.cam.ac.uk/preprints/293/). Instance matching compares two data sets and aims to identify instances (data, records) referring to the same real-world entity. It is a crucial step in populating knowledge graphs. In the preprint, we introduce the new unsupervised matching algorithm *AutoCal* which is based on heuristics and statistics. The Python code in subdirectory *ontomatch* contains:

* the code for *AutoCal* (class [``InstanceMatcherWithAutoCalibration``](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/ontomatch/instancematching.py)).
* the wrapper code for supervised *XGBoost* (class [``InstanceMatcherClassifier``](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/ontomatch/instancematching.py)) and [hyperparameter optimization](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/ontomatch/hpo.py)
* code for [pre-processing](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/ontomatch/converter.py), [blocking](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/ontomatch/blocking.py), [scoring](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/ontomatch/scoring.py) and [evaluation](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/ontomatch/evaluate.py)
* a prototypical implementation for adding background knowledge (here, in form of geographic coordinates for power plant entities) to increase the matching performance (see the [Coordination Agent](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/ontomatch/coordinator.py) as starting point)


Moreover, subdirectory *flaskapp* contains the Python code for agent communication via HTTP.


# Installation #

The easiest way to install *ontomatch* is: clone the repository, install Anaconda, create an *ontomatch* environment, add the required packages specified in *requirements.txt* and run the Python setup script.


# How to use #

This section explains how to perform instance matching for the example of power plants in Germany. This is one of six scenarios evaluated in the preprint.

## Preparing instance matching

1. Open Anaconda Command Prompt
2. Activate the *ontomatch* environment
3. Create some directory, say ``tmp_match``
4. Download the zip file with all [scenario data](https://doi.org/10.17863/CAM.82548) from the University of Cambridge data repository
5. Extract the zip file in ``tmp_match``
6. Copy ``$OntoMatchAgent/conf/logging.yaml`` to ``tmp_match``

where ``$OntoMatchAgent`` denotes the local repository path *.../TheWorldAvatar/Agents/OntoMatchAgent*.

## Running instance matching locally for evaluation

If you only want to reproduce *AutoCal*'s performance as evaluated in the preprint, you don't need to start a web server. Instead, for the example scenario of power plants in Germany:

1. Navigate to the directory ``tmp_match``
2. Copy ``$OntoMatchAgent/conf/power_plant_DEU/conf_power_plant_DEU_auto_20.json`` to ``tmp_match``
2. Execute

```console
python $OntoMatchAgent/ontomatch/coordinator.py --logconfdir . --logdir . --datadir ./ontomatch_data/data --config  ./conf_power_plant_DEU_auto_20.json
```

While executing, log data is written to console output and to the log file ``ontomatch.log`` in ``tmp_match``, e.g. to inform the user that the ontology files for both data sets are loaded, the index for token-blocking is created, the maximum similarity vectors for all candidate pairs (here: 5360 pairs) are computed, and *AutoCal* performs instance matching.

The example config file ``conf_power_plant_DEU_auto_20.json`` sets the *maximum token frequency* to 20. The config file is described below. At the end of the log file, you will find values for F1-score, threshold, precision and recall such as

```console
evaluation result on test set minus FN, ml_phase_0.0: max f1=0.89858 for t=0.26, p=0.88412, r=0.91353
...
evaluation result on test set minus FN, ml_phase_0.0 - by estimated threshold: max f1=0.8845 for t=0.325 (estimated=0.325), p=0.92814, r=0.84479, TP=762, FP=59, FN=140
```

As a result, a link file is generated in ``tmp_match`` which contains all matched pairs of power plant entities from both input data sets in terms of *owl:sameAs* triples.

## Running agents in the *Flask* web framework

This section describes how to run OntoMatchAgent as part of the World Avatar such that the agents call each other via HTTP:

1. Open Anaconda Command Prompt
2. Activate the *ontomatch* environment
2. Change to directory  ``$OntoMatchAgent``
3. Execute

```console
python ./flaskapp/wsgi.py
```

The [Coordination Agent](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/ontomatch/coordinator.py) is now waiting for HTTP requests for *AutoCal*. For testing the agent, do the following:

1. Open a second Anaconda Command Prompt
2. Activate the *ontomatch* environment
3. Navigate to the directory ``tmp_match``
4. Copy ``$OntoMatchAgent/conf/power_plant_DEU/conf_power_plant_DEU_auto_5_geo_http_link.json`` to ``tmp_match``
5. Execute
```console
python $OntoMatchAgent/ontomatch/coordinator.py --logconfdir . --logdir . --datadir ./ontomatch_data/data --config  ./conf_power_plant_DEU_auto_5_geo_http_link.json
```

In contrast to running intstance matching locally, the ``http`` parameter in the config file copied in step 4 is set to ``true`` such that all involved agents automatically use HTTP requests instead of calling their Python implementation directly. Moreover, the config file considers only a subset of similarity functions and sets the *maximum token frequency* to 5 to speed up testing. Again, a link file is generated containing all matched pairs. However, the matching result is not evaluated since no ground truth matching file is configured.


# Configuration #

The Python script ``coordinator.py`` is called with the following parameters:

```console
--logconfdir        the directory containing logging.yaml
--logdir            the directory for writing ontomatch.log
--datadir           the main directory for reading data
--config            the JSON config file
```

The config file is a JSON file containing several blocks for configuration. In the following, we will explain each block for the config file [conf_power_plant_DEU_auto_5_geo_http_link.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/OntoMatchAgent/conf/power_plant_DEU/conf_power_plant_DEU_auto_5_geo_http_link.json) used above.

The first block only specifies the random seed:

```console
"seed": 1
```

The second block contains two parameters:

```console
"http": true,
"add_knowledge": "ontomatch.knowledge.geocoding"
```

If ``http`` is ``true``, agents requests each other via HTTP; otherwise they call directly their Python implementation. If ``add_knowledge`` is set to ``ontomatch.knowledge.geocoding``, geographic coordinates are linked as background knowledge to the input data sets. If it is set to ``null``, no background knowledge is linked.

The third block specifies both input data sets (here, for the scenario of power plants in Germany):

```console
"src": "./data/power_plant_DEU/kwl.ttl",
"tgt": "./data/power_plant_DEU/gppd_DEU.ttl"
```

``src`` and ``tgt`` denote the source and target path of the input data sets which are given in [Turtle](https://en.wikipedia.org/wiki/Turtle_(syntax)) syntax here. If ``coordinator.py`` was called with parameter ``--datadir``, the string ``./data`` in both paths is replaced by the string value given for ``--datadir``.

The fourth block refers to token-blocking:

```console
"max_token_occurrences_src": 5,
"max_token_occurrences_tgt": 5,
"blocking_properties": ["name", "isOwnedBy/hasName", "address/addressLocality"]
```

``max_token_occurrences_src`` and ``max_token_occurrences_tgt`` should have the same value and are synonymous to the term *maximum token frequency* used in the preprint. Parameter ``blocking_properties`` defines string-valued properties from the data sets that are used for creating the token index. Actually, we allow property chains instead of properties but discard the prefixes of the properties to ease up configuration.

The next block determines the similarity features. It defines triples consisting of ``prop1``, ``prop2`` and ``sim`` and also allows property chains:

```console
{"prop1": "hasYearOfBuilt/hasValue/numericalValue", "prop2": "hasYearOfBuilt/hasValue/numericalValue", "sim": 0, "pos": 0},
{"prop1": "designCapacity/hasValue/numericalValue", "prop2": "designCapacity/hasValue/numericalValue", "sim": 1, "pos": 1},
{"prop1": "realizes/consumesPrimaryFuel", "prop2": "realizes/consumesPrimaryFuel", "sim": 2, "pos": 2},
{"prop1": "name", "prop2": "name", "sim": 5, "pos": 5},
{"prop1": "isOwnedBy/hasName", "prop2": "isOwnedBy/hasName", "sim": 5, "pos": 10}
```

``prop1`` and ``prop2`` refer to a property or property chain of the source data set and target data set, resp. ``sim`` determines the similarity function which is applied to calculate the similarity between values of ``prop1`` and ``prop2``. The ``sim`` values have the following meaning:

* 0 = absolute error between two numerical values
* 1 = relative error between two numerical values
* 2 = equality
* 3 to 8 refer to similarity functions for string values:
    * 3 = edit distance
    * 5 = weighted cosine similarity
    * 7 = binary cosine similarity
    * 8 = cosine similarity between neural embeddings for two string values

Parameter ``pos`` can be ignored but was helpful when evaluating many varying configurations of the same scenario for the ablation study in the preprint: It allows to calculate the similarity values on a large set of candidate pairs only once. The corresponding similarity vectors are stored and loaded in subsequent runs of AutoCal. ``pos`` denotes the corresponding position in the similarity vector.

The next block defines the AutoCal-specific parameter ``delta`` (i.e. the bin width) and should be set to its default value 0.025:

```console
"delta": 0.025
```

The last block contains four optional parameters for post-processing:

```console
"dump": "./scores",
"test_file": "./ontomatch_data/conf/power_plant_DEU/split_kg_mtf_20.csv",
"evaluation_file": "./data/power_plant_DEU/matches_power_plant_DEU.csv",
"link_file": "./linked_power_plants.owl"
```

The ``dump`` directory is used to store intermediate results and final scores. ``test_file`` is a CSV file which defines which candidate pairs belong to the training and test set, resp. This is only relevant in comparison with other methods such as XGBoost as used as binary matching classifier in the preprint. ``evaluation_file`` denotes the ground truth for matching. If given, the F1-score, precision and recall are evaluated on the test set defined by ``test_file``. Finally, ``link_file`` is generated with all entity pairs a and b matched by AutoCal; it contains triples of the form (IRI of a, owl:sameAs, IRI of b).


# Authors #

Andreas Eibeck, Shaocong Zhang


# Useful links #

* Preprint [A Simple and Effective Approach to Unsupervised Instance Matching and its Application to Linked Data of Power Plants](https://como.ceb.cam.ac.uk/preprints/293/)
* OntoMatchAgent is part of [The World Avatar](http://theworldavatar.com/) at [CARES](https://www.cares.cam.ac.uk/)
* [Knowledge graphs](https://como.ceb.cam.ac.uk/research/cps/) at the [Computational Modelling Group](https://como.ceb.cam.ac.uk/)
* [Universal Digital Twin and Knowledge graphs](https://cmclinnovations.com/digitalisation/knowledge-graphs/) at [CMCL](https://cmclinnovations.com/)
