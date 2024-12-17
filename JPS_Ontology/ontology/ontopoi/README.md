## Introduction ##

This documentation provides an overview of the development process for the OntoPOI ontology, detailing the methodology and tools used to build a structured representation of Points of Interest (POIs).

## Development ##

- **ontopoi.owl**: The ontology in the `ontopoi.owl` file contains all 9 top-level groups, 52 mid-level categories, and 600 lowest-level classes available in the [Points of Interest Classification Scheme](https://www.dropbox.com/scl/fi/krzpch9kkobpo2vek7np1/points-of-interest-classification-schemes-v3.4.pdf?rlkey=etc51hicq2ys19jh8nd45fk8i&st=4cyp2b3v&dl=0) published by Ordnance Survey. 
  - To create the ontology, the [class extractor](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-AI-for-Healthcare/Deploy/stacks/AI4Healthcare/AI4Healthcare_Common-Script) was executed to extract the groups, categories, and classes, representing them in a [TBox CSV file-based template](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/converter) as ontological classes.
  - The extractor also defined subclass relationships, linking each category to its group and each class to its category. The resulting data was saved to `ontopoi.csv`, then converted into an OWL format using the [TBox Generator](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/converter), which generated the `ontopoi.owl` file.

- **ontopoi-with-properties.owl**: This OWL file extends the base ontology (`ontopoi.owl`) by incorporating object and datatype properties. These additions are essential for producing a semantic description of the UK Points of Interest dataset as published by Ordnance Survey.

## Authors ##
Feroz Farazi (msff2@cam.ac.uk), October 2024