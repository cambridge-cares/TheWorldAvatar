# Data Generation

## Setup

### Prerequisites

- `python3`
- `conda` (recommended)

### Installation

```
pip install -r requirements.txt
```

## Dataset generation

The dataset generation pipeline comprises three main steps:

1. Generation of canonical question and SPARQL pairs from ABox.
1. Paraphrasing of canonical questions via OpenAI API.
1. Combine the paraphrases with the canonical examples and split them into train, dev, and test sets.

### Generation of canonical examples from ABox

Pairs of canonical questions in natural language and SPARQL queries can be obtain by executing the following command. 

```
python -m locate_then_ask.<domain>.generate --endpoint <endpoint>
```

For example, to generate the dataset for OntoSpecies, the command is as follows.

```
python -m locate_then_ask.ontospecies.generate --endpoint <endpoint>
```

## Rephrasing of natural language questions

First, obtain a the API key from OpenAI and add it as an environment variable, `export OPENAI_API_KEY=<openai_api_key>`. The paraphrasing step can then be executed as follows, which creates a `.csv` file in the same directory and with the same filename as the `.json` dataset file.

```
python -m paraphrase <path_to_dataset> --domain [ontospecies|ontokin|ontocompchem]
```

## Sampling of natural language questions and splitting of datasets

For every example, a single natural language question is sampled from the pool of the canonical question and its paraphrases. This is then combined with the corresponding knowledge graph domain and SPARQL query.

```
python combine_then_split.py --filepath-examples <path_to_dataset> --filepath-paraphrases <path_to_paraphrases> --dirpath-out <path_to_output_directory>
```