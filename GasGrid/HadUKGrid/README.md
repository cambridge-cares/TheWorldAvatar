## This folder contains work relating to the addition of HadUK-Grid climate data to the knowledge graph.

the ```climate_ontology``` folder contains the owl file that defines the ontology linking climate measurements to ONS statistical regions

the ```haduk_files``` folder contains the data required to populate a knowledge graph using this ontology, this is omitted in github but can be found on Vienna.

```hadukgrid_inputs_agent.py``` is technically a standard agent as it first relies on querying output areas and respective polygons. 
The program then opens and interprets netCDF climate files, and _slowly_ associates each output polygon with a set of grid points, over which temperature values are aggregated. 


