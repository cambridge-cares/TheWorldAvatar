#!/bin/bash

curl -X POST -H 'Content-type: application/xml' -d '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE properties SYSTEM "http://java.sun.com/dtd/properties.dtd">
<properties>
<entry key="com.bigdata.rdf.sail.namespace">storerouter</entry>
<entry key="com.bigdata.rdf.store.AbstractTripleStore.quads">false</entry>
<entry key="com.bigdata.rdf.store.AbstractTripleStore.axiomsClass">com.bigdata.rdf.axioms.OwlAxioms</entry>
<entry key="com.bigdata.rdf.sail.truthMaintenance">true</entry>
</properties>' http://localhost:3838/blazegraph/ui/namespace
