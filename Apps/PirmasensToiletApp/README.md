# Pirmasens Amenities App

This repo contains the code for the Pirmasens Amenities App, which is intended to display the amenities in Pirmasens and their information such as operating hours and costs. At the moment, only toilets and Wasgau (markets) are available. It will briefly explain the data requirements and instructions for deployment.

## 1. Requirements

### 1.1 Ontology

The app requires the following ontologies:

1. `OntoCityToilets` - a preliminary draft ontology that has yet to be reviewed; Defined in the `./stack-data-uploader-inputs/inputs/data/pirmasens_toilets/tbox/` directory.

The following are related ontologies that need not be imported:

1. [DCAT](https://www.w3.org/TR/vocab-dcat-3/) ontology
2. [Schema](https://schema.org/) ontology
3. [Vcard](http://www.w3.org/2006/vcard/ns#) ontology

### 1.2 Data sources

1. Toilets - Self collected into a `csv`
2. Wasgau (Market) - Self collected into a `csv`

Any data access required should be contacted through someone working on the repository.

### 1.3 Agents

1. Feature Info Agent - Required for retrieving information
2. Routing Agent - Required to map route to the amenity based on current location

## 2. Deployment

### 2.1 Backend Services

The app will require a running [The World Avatar stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) as the backend. Data specified in [this section](#12-data-sources) should be uploaded using the [Stack Data Uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader).

### 2.2 Android deployment

This section is under construction.

#### 2.2.1 Mapbox token

Your mapbox token should be in [`settings.gradle`](./gradle.properties) and also [`developer-config.xml`](./core/utils/src/main/res/values/developer-config.xml).

#### 2.2.2 Phone and stack communication

If you are running on emulator use `HOST_LOCALHOST` in [`NetworkConfiguration.java`](./core/network/src/main/java/uk/ac/cam/cares/jps/network/NetworkConfiguration.java), otherwise you should have android and stack on same network. You can use `ipconfig` or other related commands to find the ip address of your pc.

### 2.3 Web Visualization

If you run the backend of the android application, you can also see the following web visualization at http://localhost:3838/visualisation/:
