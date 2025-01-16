# Compact query mapping rules

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
?UseIRI rdfs:label ?UseValue .
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