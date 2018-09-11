### OLS-Docker

This is a dockerized version of the EMBL-EBI Ontology Lookup Service that can be found at http://www.ebi.ac.uk/ols/index.

### Prepare
First steps before running are:
- cloning this repository with git and installing [docker](https://www.docker.com/).
- Edit the obo-config.yaml with your specific ontologies.
- Edit ols-config.yaml.sample with your specific ontologies and rename  to ols-config.yaml. 
More info on how the configuration file should look, can be found at [here](http://www.ebi.ac.uk/ols/docs/installation-guide) under Configurations.

### Build and run
The following commands can be used to build and run the images (Big ontologies can take a lot of time and memory, NCBITAXON takes 22G of memory). 
The docker is configured to use 4G of memory. This can be changed in the settings (`MEMORY_USE "-Xmx4g -Xms4g"`). 

  1. `docker build -t ols .`
  2. `docker run -d -p 8080:8080 -t ols`
  3. Access the OLS through your browser on http://localhost:8080  
