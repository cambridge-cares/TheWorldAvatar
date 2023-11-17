# owl-to-ds

## Setup

- `python3`
- `pip install -r requirements.txt`

## Generate dataset

```
python -m locate_then_ask.ontospecies.generate
python -m locate_then_ask.ontokin.generate --endpoint <endpoint> --user <username> --pw <password> --repeats <number_of_repeats>
```

## Rephrase canonical questions

```
export OPENAI_API_KEY=<openai_api_key>
python -m paraphrase <path_to_dataset> --domain [ontospecies|ontokin]
```

## Combine paraphrases with SPARQL queries and create train, dev, test splits

```
python combine_then_split.py --filepath-examples <path_to_dataset> --filepath-paraphrases <path_to_paraphrases> --dirpath-out <path_to_output_directory>
```