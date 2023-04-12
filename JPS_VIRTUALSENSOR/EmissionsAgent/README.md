# Emissions Agent
This agent extends the DerivationAgent class and is meant to be called by the derivation framework. The derivation should be a synchronous one.

## Input
IRI with the rdf:type http://www.theworldavatar.com/kg/dispersion/Ship

## What it does
Queries ship speed using the given ship IRI, send a GET request to http://${STACK_NAME}_python-service:5000/getEmissions with the speed value to obtain emissions values. Creates the new triples for all the emissions values, to be added by the derivation framework.

