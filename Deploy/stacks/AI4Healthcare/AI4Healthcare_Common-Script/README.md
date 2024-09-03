## Introduction ##

This documentation outlines the scripts (provided in the .py file) developed to process data and class files provided by the relevant project partner. It describes how to create a virtual environment, install the required libraries and run the scripts. It is strongly recommended to create and activate a virtual environment before running the Python script.

## Creating and activating a virtual environment ##
To create a virtual environment named `classextract`, run the following command:
```
python -m venv classextract
```

Activate the virtual environment by executing the following command:
```
.\classextract\Scripts\activate
```

## Installing libraries ##
To install the required libraries, run the following commnand:
```
pip install -r .\requirements.txt
```

## Scripts ##

Below are descriptions of the scripts used to process data or ontological elements:

**Class Extractor:**<br/>
The `class_extractor.py` file extracts categories, subcategories and classes from the Points of Interest Classification Scheme published by Ordnance Survey in a pdf file. To run this Python module, copy the points of interest classification schemes file from the CoMo shared dropbox from the path of "CoMo shared\_CoMo_Developments\data\ai4healthcare\points-of-interest-classification-schemes-v3.4.pdf" into a data folder. Assuming that the name of the folder is 'data' and the path of the 'data' folder is "C:/Users/<YOUR_USER_NAME>/Documents/data". Following the copy, the path of the points of interest classification schemes file will be "C:/Users/<YOUR_USER_NAME>/Documents/data/points-of-interest-classification-schemes-v3.4.pdf". If you want to name the output CSV file that will contain the output ontological classes and relationships as OntoPointsOfInterests, then run the `class_extractor.py` module with the following command:
```
python .\class_extractor.py C:/Users/<YOUR_USER_NAME>/Documents/data/points-of-interest-classification-schemes-v3.4.pdf OntoPointsOfInterests.csv
```

## Authors ##
Feroz Farazi (msff2@cam.ac.uk), August 2024