# Data generation for Marie

## Description

## Installation

### Prequisites
- `python3`
- `python3-venv`

### Setup steps

1. Create a virtual environment and install required dependencies by executing `./install_script_pip.sh -v -i` in a bash shell. 
1. Activate the virtual environment.

   `(Windows)`

   ```cmd
   $ data_generation_venv\Scripts\activate.bat
   (data_generation_venv) $
   ```

   `(Linux)`

   ```cmd
   $ source data_generation_venv\bin\activate
   (data_generation_venv) $
   ```

## Usage

### Create a dataset for training

The following command creates a train, dev, and test sets using the question and query templates are located under the `/templates` directory. 

```
python data_generation/create_training_data.py
```

Each leaf directory corresponds to a template, comprising of
- `question_templates.txt`: a list of question templates,
- `query_template.txt`: the full SPARQL query template,
- `query_compact_template.txt`: the compact SPARQL query template.

The three files must contain the same set of arguments, and the every argument must be one of the following, with an optional numerical suffix:
- `PropertyName` (and `PropertyName1`, `PropertyName2`, etc.)
- `IdentifierName`
- `species`
- `ChemClass`
- `Use`
- `value`, `minvalue`, `maxvalue`

## Authors

Laura Pascazio (<lp521@cam.ac.uk>), Dan Tran (<nmdt2@cam.ac.uk>)