# Data generation for Marie

## Description

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

### Data augmentation

Adapted from https://arxiv.org/pdf/1901.11196.pdf.

```python
import nltk
nltk.download("punkt")
nltk.download("stopwords")
```

```
python data_generation/augment_data.py --input_path data/train.json --output_path data/train_augmented.json --alpha 0.1 --n_aug 4
```

## Compact query mapping rules

<table>
<tr>
<th></th>
<th>Original</th>
<th>Compact</th>
</tr>

<tr>
<td>select label</td>
<td>

```
SELECT DISTINCT ?label
```

</td>
<td>

```
SELECT DISTINCT
```

</td>
</tr>

<tr>
<td>select property</td>
<td>

```
SELECT DISTINCT <...> ?{PropertyName}Value ?{PropertyName}UnitValue ?{PropertyName}ReferenceStateValue ?{PropertyName}ReferenceStateUnitValue
```

</td>
<td>

```
SELECT DISTINCT <...> ?{PropertyName}Value
```

</td>
</tr>

<tr>
<td>species pattern</td>
<td>

```
?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .

?SpeciesIRI ?hasIdentifier ?IdentifierIRI .
?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
?Identifier rdfs:subClassOf os:Identifier .

FILTER( ?species = "{species}")
```

</td>
<td>

```
?SpeciesIRI ?hasIdentifier ?species .
FILTER( ?species = "{species}")
```

</td>
</tr>

<tr>
<td>property pattern</td>
<td>

```
?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
OPTIONAL{{?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .}}
```

</td>
<td>

```
?SpeciesIRI os:hasProperty{PropertyName} ?{PropertyName}Value .
```

</td>
</tr>

<tr>
<td>identifier pattern</td>
<td>

```
?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}IRI .
?{IdentifierName}IRI os:value ?{IdentifierName}Value .
```

</td>
<td>

```
?SpeciesIRI os:hasIdentifier{IdentifierName} ?{IdentifierName}Value .
```

</td>
</tr>

<tr>
<td>use pattern</td>
<td>

```
?SpeciesIRI os:hasUse ?UseIRI .
?UseIRI os:value ?UseValue .
```

</td>
<td>

```
?SpeciesIRI os:hasUse ?UseValue .
```

</td>
</tr>

<tr>
<td>chemclass pattern</td>
<td>

```
?SpeciesIRI os:hasChemicalClass* ?x .
?x ?y ?z .
?z rdfs:subClassOf* ?ChemicalClassIRI .
?ChemicalClassIRI rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue .
```

</td>
<td>

```
?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .
```

</td>
</tr>

<tr>
<td>all properties</td>
<td>

```
?SpeciesIRI ?hasPropertyName ?PropertyNameIRI .
?PropertyNameIRI  rdf:type ?PropertyName .
?PropertyName rdfs:subClassOf os:Property .
?PropertyNameIRI os:value ?PropertyNameValue ; os:unit ?PropertyNameUnitIRI ; os:hasProvenance ?PropertyNameProvenanceIRI . 
?PropertyNameUnitIRI rdfs:label ?PropertyNameUnitValue .
OPTIONAL{{?PropertyNameIRI os:hasReferenceState ?PropertyNameReferenceStateIRI . 
?PropertyNameReferenceStateIRI os:value ?PropertyNameReferenceStateValue ; os:unit ?PropertyNameReferenceStateUnitIRI .
?PropertyNameReferenceStateUnitIRI rdfs:label ?PropertyNameReferenceStateUnitValue .}}

BIND(strafter(str(?PropertyName),'#') AS ?PropertyLabel) 
```

</td>
<td>

```
?SpeciesIRI ?hasPropertyName ?PropertyNameValue .
```

</td>
</tr>

<tr>
<td>all identifiers</td>
<td>

```
?SpeciesIRI ?hasIdentifierName ?IdentifierNameIRI .
?IdentifierNameIRI  rdf:type ?IdentifierName .
?IdentifierName rdfs:subClassOf os:Identifier .
?IdentifierNameIRI os:value ?IdentifierNameValue .

BIND(strafter(str(?IdentifierName),'#') AS ?IdentifierLabel)
```

</td>
<td>

```
?SpeciesIRI ?hasIdentifierName ?IdentifierNameValue . 
```

</td>
</tr>

</table>


## Tests

Execute `pytest`.

## Authors

Laura Pascazio (<lp521@cam.ac.uk>), Dan Tran (<nmdt2@cam.ac.uk>)