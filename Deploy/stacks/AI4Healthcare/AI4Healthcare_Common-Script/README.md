## Introduction ##

This documentation outlines the scripts (provided in the .py files) developed to process data and class files provided by the relevant project partner. It describes how to run the scripts.

## Scripts ##

Below are descriptions of the scripts used to process data or ontological elements:

**Class Extractor:**<br/>
The `class_extractor.py` file extracts categories, subcategories and classes from the Points of Interest Classification Scheme published by Ordnance Survey in a pdf file. To run this Python module, copy the points of interest classification schemes file from the CoMo shared dropbox from the path of "CoMo shared\_CoMo_Developments\data\ai4healthcare\points-of-interest-classification-schemes-v3.4.pdf" into a data folder. Assuming that the name of the folder is 'data' and the path of the 'data' folder is "C:/Users/<YOUR_USER_NAME>/Documents/data". Following the copy, the path of the points of interest classification schemes file will be "C:/Users/<YOUR_USER_NAME>/Documents/data/points-of-interest-classification-schemes-v3.4.pdf". If you want to name the output CSV file that will contain the output ontological classes and relationships as OntoPointsOfInterests, then run the `class_extractor.py` module with the following command:
```
python .\class_extractor.py C:/Users/<YOUR_USER_NAME>/Documents/data/points-of-interest-classification-schemes-v3.4.pdf OntoPointsOfInterests.csv
```

## Authors ##
Feroz Farazi (msff2@cam.ac.uk), August 2024