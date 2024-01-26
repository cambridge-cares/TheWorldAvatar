## Deploying and using new versions of the dependencies

Each of the three dependencies in this directory are managed using separate Maven configurations.
The descriptor.xml specifies the content and format (zip) of each dependency, while the pom.xml contains the artifact's metadata and describes how to package and deploy it.

To upload a new version of one of the artifacts:
1. Ensure you have [Maven](https://maven.apache.org) installed and configured with appropriate settings to allow upload to the World Avatar Maven repo. See [this readme](../../Deploy/examples/maven_dependency/deploy/README.md) for instructions.
2. Place the new files/directories in the dependency sub-directory.  The required content is:
```
  ./custom_node_modules
    /node_modules (directory)

  ./hdt_data
    /ontocompchem_clean.hdt (file)
    /ontokin_clean.hdt (file)

  ./trained_models
    /model_jps (directory)
    /models_wiki (directory)
```
3. Increment the artifact's version number in pom.xml.

4. On the command line, in this directory, run
```
  mvn install
```

5. Check that the contents of the zip file in ./target are what you expect. When you're happy, upload the zip file to the repository with
```
mvn deploy
```
6. To use the new dependency when deploying, edit the corresponding version number under the *dependencies* node in the [Java servlet's pom file](../../JPS_LDF/components/LDF_SERVER_JAVA/Server/pom.xml).


## Modifying dependency content
If the content and/or directory structure of any of the three dependencies changes, please modify the appropriate descriptor.xml **and this README**.