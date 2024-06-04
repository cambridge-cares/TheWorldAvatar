TBoxGeneration will not create the range for `hasStartTime` and `hasEndTime` in the generated OWL file, even though the range has been defined in `OntoSeaLevel.csv`.

After generating `OntoSeaLevel.owl` with TBoxGeneration, please add the following to `hasStartTime` and `hasEndTime` in the generated `OntoSeaLevel.owl`:
```
<rdfs:range>
    <owl:Class>
        <owl:unionOf rdf:parseType="Collection">
            <rdf:Description rdf:about="http://www.w3.org/2001/XMLSchema#gYear"/>
            <rdf:Description rdf:about="http://www.w3.org/2001/XMLSchema#date"/>
            <rdf:Description rdf:about="http://www.w3.org/2001/XMLSchema#dateTime"/>
        </owl:unionOf>
    </owl:Class>
</rdfs:range>
```