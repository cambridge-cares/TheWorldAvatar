# Dataset generation

## Generate new datasets

1. Navigate to the `/MARIE_SEQ2SEQ` directory.
1. Execute `python create_training_data.py`

## Question and query templates

Question and query templates are located under the `/templates` directory. Each leaf directory corresponds to a template, comprising of
- `question_templates.txt`: a list of question templates,
- `query_template.txt`: the full SPARQL query template,
- `query_compact_template.txt`: the compact SPARQL query template.

The three files must contain the same set of arguments, and the every argument must one of the following, with an optional numerical suffix:
- `PropertyName` (and `PropertyName1`, `PropertyName2`, etc.)
- `IdentifierName`
- `species`
- `ChemClass`
- `Use`
- `value`, `minvalue`, `maxvalue`
