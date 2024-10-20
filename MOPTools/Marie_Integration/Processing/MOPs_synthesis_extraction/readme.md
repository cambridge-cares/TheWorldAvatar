# MOPs synthesis extraction
## Description
This project is a Python-based tool that automates the synthesis data extraction for the OntoSyn ontology using LLMs.

## Installation (not yet done)
To install the necessary packages, run:

```bash
pip install -r requirements.txt
```
## File overview
- setup_structure: Provides the folder structure to process papers with the MOPs_synthesis_extraction. Make sure to run it before starting the extraction process. The name of the batch of folders is specified in the parameters file with the BATCH_NAME.
- main_pipeline: main file with a detailed step by step description on how to process papers. Works on a folder structure basis.
- llm_extraction.py: Provides the functions to 
