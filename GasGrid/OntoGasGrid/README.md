## OntoGasGrid
---
This folder contains the code to generate semantic descriptions of the UK gas network, associated infrastructure as well as gas and electricity consumption. 

Specific ```README``` files can be located within each folder. 

**To instanciate a representation from scratch I would follow this order:**

1. ```grid_component_owl_generator```
2. ```pipeline_owl_generator```

This creates OWL files of infrastructure which are stored within the ```abox-tbox``` folder.

3. Upload ONS output area triples located in the data folder of the 279 project folder. Or don't do this, and don't worry about it. It's needed for some of the output stuff.
4. ```statistical_elec_population``` and ```statistical_gas_population``` associates electrical and gas consumption to these areas. 
	-  Because of the way the code works, it doesn't rely on areas being already represented in the knowledge graph.
	- Ideally I would query the ONS endpoint, match up areas with codes in the excel files, then upload to that IRI.
	- But when you know that the IRI of an area is just a namespace with the LSOA code at the end you can just construct it directly from the excel file.
	- Uploading the ONS triples to the same knowledge graph means you don't have to do federated queries etc. when you eventually need polygons or properties. Anyway this code will work with or without the areas, whether it should or not, I don't know.

5. ```real_time_terminal_population``` will associate real time instantaneous flowrates to gas terminals.
	- Gas terminal IRIs are hardcoded here
	- To change this would require a property similar to 'csv_name' to be able to match up the instance of a gas terminal with the name that appears next to its flowrates in the .csv downloaded from the National Grid.
6. ```use_cases/nearest_local_distribution``` queries for polygons and local distribution offtakes and associates the closest offtake to each area. The assumption here is that the gas for an area comes from the closest local distribution offtake. This code links ONS areas to infrastructure. 
