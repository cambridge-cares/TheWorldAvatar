# Using Ontop and GeoServer to serve PubChem data

## Prerequisites

* WSL2 installed (Windows only)
* Docker installed
* Docker configured to use WSL2 (should be the default if WSL2 was installed first) (Windows only)
* Add your user account to the "docker" group, this removes the need to use `sudo` to run the docker(-compose) commands later.
* A [PubChem XML file](https://ftp.ncbi.nlm.nih.gov/pubchem/Compound/CURRENT-Full/XML/) 


## Converting XML files to CSV
1. In a WSL2 terminal go to the directory containing the files for the xml2csv convertor tool:

        cd TheWorldAvatar/Deploy/stacks/ontop+geoserver/data_sets/pubchem

2. Put the dowloaded XML file in the following directory.

        ./shared_data/pubchem

3. Run the xml2csv convertor by executing the following command. Once this step is done, there will be a csv file generated in the above-mentioned directory. (Note: depending on the size of the file, this process could take up to ~20 minutes (for an 18 GB XML file))
       
        docker-compose -f docker-compose.xml2csv.yml


4. After xml2csv conversion, run the transform_obda.sh in the ontop directory to create the required mapping between the SQL table and the pubchem ontology:
        
        ./ontop/transform_obda.sh

5. As the final step before uploading the CSV files to PostgreSQL tables on Ontop, run `generate_env_file.sh` in the scripts directory to create the `.env` file required by the geoserver and uploader services in the main parent directory (TheWorldAvatar/Deploy/stacks/ontop+geoserver) using the following:

        ./scripts/generate_env_file.sh

6. If you already have your CSV files stored in the shared_data directory, there is no need to follow these steps. 

## Uploading the csv files to PostgreSQL tables on Ontop
Please refer to the `README.md` file in the main parent directory at:

        TheWorldAvatar/Deploy/stacks/ontop+geoserver

and follow the steps accordingly.

## Running the servers by providing ontologies:
The previous instructions are provided for only using a mapping file between the triples and PostgreSQL tables. If it is required to also upload a TBox (ontology) to make sure that the mapping file is correct, an extra step must be taken btween step 5 and 6. First, copy your ontology (.owl file) in the tbox directory (./shared_data/pubchem/tbox). Second, run the following command to copy the owl file to ontop/inputs folder in the parent ONTOP+GEOSERVER directory and generate the required ontop.env file for building the ontop server:

        ./scripts/generate_ontop_env_file.sh

Finally, proceed with rest of the instructions at step 6. 