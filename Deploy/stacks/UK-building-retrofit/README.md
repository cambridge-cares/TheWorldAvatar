# UK Building Retrofit

This project folder provides a set of instructions for creating an interoperable environment to integrate UK-wide building energy performance, energy consumption, fuel poverty, and public health records. The use case is to quantify building retrofit urgency and determine optimal retrofit pathways across multiple geographic scales.

## Stack Deployment

The [StackDeployment] folder contains a detailed guide for deploying a [Docker] Stack tailored for the UK building retrofit use case. It provides an [SPARQL] endpoint for semantic representation and information extraction.


## Utilities

The [Utilities] folder contains geospatial analysis models to identify energy consumption [hotspots], an approach to assess retrofit urgency considering energy, socioeconomic, and health factors based on The Technique for Order of Preference by Similarity to Ideal Solution ([TOPSIS]), and a [decision_tree_model] for what-if analysis of fabric-first versus system-led retrofit improvements.


<!-- Links -->
[StackDeployment]: StackDeployment
[Utilities]: Utilities
[TOPSIS]: https://en.wikipedia.org/wiki/TOPSIS
[SPARQL]: https://en.wikipedia.org/wiki/SPARQL
[hotspots]: https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/hot-spot-analysis.htm
[decision_tree_model]: https://xgboost.readthedocs.io/en/latest/index.html
[Docker]: https://www.docker.com/