## Introduction ##

This documentation outlines the `class_extractor.py` script developed to process the Points of Interest (POI) Classification Scheme published by Ordnance Survey. It explains how to create a virtual environment, install the required libraries, and run the script. It is strongly recommended to create and activate a virtual environment before running the Python script.

## Creating and Activating a Virtual Environment ##
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

Below is a description of the script used to process ontological classes from the POI Classification Scheme:

**Class Extractor:**<br/>
The `class_extractor.py` file extracts groups, categories and classes from the POI Classification Scheme published in a PDF file. To run this Python module:
1. Copy the Points of Interest Classification Scheme file from the CoMo shared Dropbox, located at `"CoMo shared\_CoMo_Developments\data\ai4healthcare\points-of-interest-classification-schemes-v3.4.pdf"` into a local folder, such as `data`.
2. Assuming the `data` folder is located at `"C:/Users/<YOUR_USER_NAME>/Documents/data"`, the path to the file will be `"C:/Users/<YOUR_USER_NAME>/Documents/data/points-of-interest-classification-schemes-v3.4.pdf"`.

If you would like the output CSV file containing the extracted ontological classes and relationships to be named `ontopoi.csv`, run the `class_extractor.py` module with the following command:
```
python .\class_extractor.py C:/Users/<YOUR_USER_NAME>/Documents/data/points-of-interest-classification-schemes-v3.4.pdf ontopoi.csv
```
The `class_extractor.py` script also generates a unique PointX Classification code for each class and saves the mappings between classes and codes in the `class_complete_code_map.csv` file.

## Authors ##
Feroz Farazi (msff2@cam.ac.uk), August 2024