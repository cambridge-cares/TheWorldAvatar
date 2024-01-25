# Data generation for Marie

## Installation

### Prequisites
- `python3`
- `python3-venv`

### Setup steps

1. Create a virtual environment and install required dependencies by executing `./install_script_pip.sh -v -i -e` in a bash shell. 
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

### Dataset creation from ABox

Training data can be created from the ABox of the OntoSpecies knowledge graph, provided that there are a sufficiently large number of species (>100) whose data properties provide a good spread over the ontology's schema. Example usage:

```python
import json
from datetime import datetime

from data_generation.create_training_data_from_kg import DatasetFromKgMaker


kg_endpoint = "http://localhost:9999/blazegraph/namespace/kg/sparql"
example_maker = DatasetFromKgMaker(kg_endpoint=kg_endpoint)
examples = example_maker.make_examples()

with open(f'train.json', "w") as f:
    json.dump(examples, f, indent=4)
```

#### [DEPRECATED] Generation from templates

The following command creates a train, dev, and test sets using the question and query templates located under the `/templates` directory. 

```
python data_generation/create_training_data.py
```

The format of the output dataset is as follows.
```json
[
   {
      "template_name": "string",
      "question": "string",
      "sparql_query": "string",
      "sparql_query_compact": "string"
   }
]
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

### Paraphrasing of natural language questions

The natural language questions in the generated dataset can be paraphrased using OpenAI's ChatGPT API to further enhance linguistic variability. 

1. Export the OpenAI's API key as an environment variable.

   ```
   export OPEN_API_KEY=<API-KEY>
   ```

1. Command to perform paraphrasing. The script sends a paraphrasing request to OpenAI's server for every question; for every batch of 10 questions, the paraphrased texts are saved into disk as a json file named `<index>.json`, where `<index>` is the index of the first question in the output file. This is to help with the resumption of the paraphrasing job in case of server faults.

   ```
   python data_generation/create_training_data_from_kg/paraphrase.py <path-to-input-dataset-file> <path-to-output-dataset-file> --start_index <start-index-for-resumption>
   ```

### Data augmentation

Word-level data augmentation methods&mdash;synonym substitution, random insertion, random swap, random delete&mdash;are implemented with adaptations based on https://arxiv.org/pdf/1901.11196.pdf. 

1. Download relevant `nltk` resources in a Python shell. 

   ```python
   import nltk
   nltk.download("punkt")
   nltk.download("stopwords")
   ```
   
1. Command to perform data augmentation.

   ```
   python data_generation/augment_data.py --input_path data/train.json --output_path data/train_augmented.json --alpha 0.1 --n_aug 4
   ```

## Tests

Execute `pytest`.

## Authors

Laura Pascazio (<lp521@cam.ac.uk>), Dan Tran (<nmdt2@cam.ac.uk>)