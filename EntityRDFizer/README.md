###  _Author - Feroz Farazi (msff2@cam.ac.uk), 17 May 2021_

This project is designed to convert entities of any domain and their data and metadata into RDF.
It requires the entities and their data to be provided as inputs in an ABox CSV template, that is
filled in with data. A group of ABox CSV template files are provided under the following URL:
https://github.com/cambridge-cares/TheWorldAvatar/tree/master/JPS_Ontology/KBTemplates/ABox

How to convert a single ABox CSV template file or all ABox CSV template files from a folder is
described below:   

* __Single file conversion__:To convert a single ABox CSV template file, call the 
function convert_into_rdf(input_file_path, output_file_path) from the following python module:
EntityRDFizer/converter/ABoxTemplateCSVFileToRDF.py

* __Multiple file conversion__:To convert all ABox CSV template files from a folder, call the
function convert(input_folder_path, output_folder_path) from the following python module:
EntityRDFizer/converter/ABoxTemplateCSVFilesToRDF.py
