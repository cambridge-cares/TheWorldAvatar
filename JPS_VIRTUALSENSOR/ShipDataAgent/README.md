# Ship data agent
This agent extends the DerivationAgent class and is meant to be called by the derivation framework. The derivation should be a synchronous one.

## Input
IRI with the rdf:type http://www.theworldavatar.com/kg/dispersion/Ship

## What it does
Adds a set of fixed values (at the moment) for energy efficiency, CII, specific CO2 emissions, and specific fuel consumption. This can be extended to do actual calculations.
