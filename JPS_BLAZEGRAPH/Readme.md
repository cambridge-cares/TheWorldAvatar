# Blazegraph Project
### Authors
* [Feroz Farazi](msff2@cam.ac.uk)

### Ontology Upload

Copy TheWorldAvatar/JPS_BLAZEGRAPH/target/JPS_BLAZEGRAPH-1.0.0-SNAPSHOT-jar-with-dependencies.jar at the same level
of this Readme file and rename this as ontology-uploader.jar.

For uploading ontologies to any instance of Blazegraph, run the startUpload.bat file with three arguments as follows:
>startUpload.bat <Endpoint_URL> <Namespace/Repository_Name> <Path_to_the_ontologies>

Let's assume that you have the following values of these arguments:
<Endpoint_URL> = http://localhost:8080/blazegraph
<Namespace/Repository_Name> = ontocompchem
<Path_to_the_ontologies> = C:/data/kb

In this case, run the following command:
>startUpload.bat http://localhost:8080/blazegraph ontocompchem C:/data/kb

### Important Note

The uploader creates the following three log files:

1. imported-file.log: maintains a list of already imported files to skip them in the next run.
2. non-rdf-or-owl-file.log: maintains a list of files, which are neither RDF nor OWL, to inform users about the existence of non-RDF or OWL files at the path to the ontologies to upload.
3. import-error-detailed.log: reports any error that occurs during the import.

__Users must delete the imported-file.log file or its content to upload files that have already been uploaded.__