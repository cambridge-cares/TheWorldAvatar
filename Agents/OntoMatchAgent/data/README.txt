Each domain directory contains 
* a subdirectory "original"
* the corresponding pre-processed RDF graphs in Turtle format (.ttl)
* Pickle files (.pkl) for the 'OntologyWrapper' for accelerating debugging etc. 
* a file 'matches_<domain>.csv' containing the ground truth of matches

Additionally, kwl_geo.ttl/.pkl (in directory power_plant_DEU) and dukes_geo.ttl/.pkl (in directory power_plant_GBR) are the RDF graphs enriched with geo coordinates


Where did the original data came from?

./power_plant_DEU/original
* globalpowerplantdatabasev120: downloaded on 02/06/2021 from https://datasets.wri.org/dataset/globalpowerplantdatabase
* Kraftwerksliste_CSV_2020_4.csv: downloaded on 08/07/2021 from 
	
./power_plant_GBR/original
* downloaded on 04/06/2021 from https://www.gov.uk/government/statistics/electricity-chapter-5-digest-of-united-kingdom-energy-statistics-dukes	

./bibliograph/original
* downloaded on 05/11/2021 from https://dbs.uni-leipzig.de/de/research/projects/object_matching/benchmark_datasets_for_entity_resolution

./product/original
* downloaded on 05/11/2021 from https://dbs.uni-leipzig.de/de/research/projects/object_matching/benchmark_datasets_for_entity_resolution 

./restaurant/original
* downloaded on 05/11/2012 from https://www.cs.utexas.edu/users/ml/riddle/data.html 