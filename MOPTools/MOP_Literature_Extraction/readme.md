# MOP_Literature_Extraction

## Overview

MOP_Literature_Extraction is a Python-based pipeline designed to process and extract key information from scientific literature related to materials and synthesis procedures. The project leverages machine learning models and knowledge graph techniques to extract and structure data from scientific papers.

## Features

- Extracts synthesis procedures from scientific literature
- Utilizes OpenAI models for text processing
- Integrates with knowledge graphs for structured data storage
- Supports PDF and text-based inputs

## Installation

### Prerequisites

Ensure you have the following installed:

- Python 3.8+
- `pip` package manager

Furthermore update the required parameters and iris in parameters.py and predefined_iris.py.

## Usage
1. **Configure parameters:**  
   Populate the `parameters.py` file with information specific to your set up.  
   Also make sure to check the hard coded Iris in the predefined_iris.py file. 

2. **Set up environment files:**  
   For each required Knowledge Graph (KG) connection, fill out the corresponding `.env` file. Use `OntoMOPConnectionExample.env` as a reference.  
   - For the master’s thesis, connections were established to **OntoMOPs, OntoSpecies, and OntoSynthesis**.  

3. **Secure sensitive parameters:**  
   Copy and complete `secret_parameter_example.py`, ensuring that it is never committed to GitHub.  

4. **Initialize the folder structure:**  
   Run `setup_structure.py` to create the necessary directory setup.  

5. **Prepare input data:**  
   Place the research papers to be processed in the `BatchName_pdf` folder.  

6. **Run the pipeline:**  
   Execute the main pipeline either **all at once** or **one function at a time** for step-by-step processing.  



## Module Descriptions

A more detailed module description can be found in the docs folder.


