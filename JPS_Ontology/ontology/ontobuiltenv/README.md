# The Ontology for the Built Environment (OntoBuiltEnv)
## The existing issue
OntoBuiltEnv uses several external ontologies, such as `Ontology of units of Measure (OM)`, but it redefined some classes and properties of external ontologies, such as `om:hasValue`, `om:hasNumericalValue`, `om:hasUnit`. The example of re-definition of `om:hasValue` is shown in the following. 
```
    <!-- http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue -->


    <owl:ObjectProperty rdf:about="http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue">
        <rdfs:domain>
            <owl:Class>
                <owl:unionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://www.ontology-of-units-of-measure.org/resource/om-2/AmountOfMoney"/>
                    <rdf:Description rdf:about="http://www.ontology-of-units-of-measure.org/resource/om-2/Area"/>
                    <rdf:Description rdf:about="http://www.ontology-of-units-of-measure.org/resource/om-2/Height"/>
                    <rdf:Description rdf:about="https://www.theworldavatar.com/kg/ontobuiltenv/AveragePricePerSqm"/>
                </owl:unionOf>
            </owl:Class>
        </rdfs:domain>
        <rdfs:range rdf:resource="http://www.ontology-of-units-of-measure.org/resource/om-2/Measure"/>
        <rdfs:isDefinedBy rdf:resource="http://www.ontology-of-units-of-measure.org/resource/om-2/"/>
        <rdfs:label>has value</rdfs:label>
    </owl:ObjectProperty>
```
This kind of definition is not correct. We should not redefine external ontology, which might have trouble for the using of external ontology.

Currently, the re-definiton of external ontologies was done in a year ago and has been used in some existing agents. To avoid breaking the existing works, we can not fix this issue right now. But it is an open `TO-DO` item.

## Solution in Sea level rise project
In current OntoBuiltEnv, `om:hasValue` and `om:hasNumericalValue` has been re-defined, so we can not use them directly with `om:Cost`. The relationships to describe `om:Cost` are shown in the following.
```
om:Cost a om:Quantity
om:Quantity om:hasValue om:Measure
om:Measure om:hasNumericalValue <value>
```
