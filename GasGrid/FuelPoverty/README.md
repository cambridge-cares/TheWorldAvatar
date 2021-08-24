## This folder contains work relating to the addition of fuel poverty statistics to the knowledge graph.

the ```fuel_poverty_ontology``` folder contains the owl file that defines the ontology linking to ONS statistical regions

The ontology is as follows:

<p align="center">
  <img src="fuel_poverty_ontology.png" width="350">
</p>



the ```data``` folder contains the data required to populate a knowledge graph using this ontology

```fuel_poverty_population.py``` takes the file in this data folder and associates the contained fuel poverty statistics to each output area.
The program performs SPARQL update queries to a specified endpoint. This is done in batches of areas at a time for various reasons.
- If I was to improve this I would query for the output areas first as opposed to constructing the IRI based on the LSOA code in the spreadsheet. 
- I would also not hardcode the start and end dates but construct them based on the spreadsheet.
