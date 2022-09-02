# Example Digital Twin Visualisation

This example visualisation has been put together to demonstrate the intended use of the centralised Digital Twin Visualisation Framework (DTVF). This framework has been designed to make it easier for users not experienced with Typescript (or the mapping libraries) to quickly & easily put together a new Digital Twin visualisation. It is intended for developers to use this example visualisation to gain an understanding of the DTVF before attempting to create their own visualisation; to do that, this example can be copied and used as a starting point.

This centralised framework expects users to have structured their data in a particular format, and provide a number of associated metadata files. Before starting, ensure your data meets this format by reading the ["Visualisation Framework"](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations) page on the GitHub wiki. This page also details how the framework functions, and how to configure your visualisation to use it.

Note that this example has no version number as it is not intended for release; instead it should be updated to reflect the most recent version of the framework itself. It's also worth noting that this example uses the currently hosted version of the framework library (i.e. from the CMCL KG website), not the files within the repository.

## Considerations

As an example visualisation, the setup here may not fully correspond to the way in which finished visualisations are deloyed. Please bear the following in mind when using this example.

### Stack Architecture

In most deployed visualisations, an online stack of microservices will provide data endpoints through which data can be queried/loaded onto the visualisation. This stack will also host a number of files the visualisation uses to configure itself; namely the `visualisation.json` file (which defined which sources and layers are loaded), the `icons.json` file (which defines the location and names of any icons used), and the `links.json` file (which adds a number of optional links to external resources).

In this example, no online stack is used. Data is loaded from GeoJSON files that are hosted locally (i.e. within the visualisatition's webserver) rather than pointed towards a stack's endpoints. The visualisation's local webserver is also used to host the aforementioned JSON files. 

In the future, when a stable stack is running, another example may be added to demonstrate how to connect a visualisation to an external stack.

### Mapping Capabilities

Whilst the framework has been designed to (theoretically) support different mapping libraries, at the time of writing only MapBox is used. This means that data is limited to 2D representation (plus extruded polygons), ruling out more complex 3D data (i.e. BIM). As such, only styling features supported by MapBox can be used.

There is a plan to add support for CesiumJS in the framework, but but this process has not yet begun.

### Styling

Styling is currently provided by MapBox and is limited to their capabilities. Styling options are defined by the developer within the `visualisations.json` file and passed straight to the MapBox API; this means that anything supported by the [MapBox styling documentation]("./data/sample-set-1") can be controlled by the user without code changes. For more information on how to style layers, see the GitHub wiki.

## Sample Data

A small amount of sample data has been committed to demonstrate the power of the DTVF to visualisate different data types. Please do not make changes to the sample data without consulting the original developer.

At the time of writing, the sample data sets include:

- **cambridge**:
  - Based in and around Cambridge, this data set mimics a single stack that contains data on college locations and buildings.
  - The colleges layer demonstrates how clustering can be achieved.
  - Each college feature links to a sample set of metadata and timeserise (also contained in local GeoJSON files).
- **singapore**:
  - Based in Singapore, this data set includes details of rail lines and stations.
  - No metadata or timeseries present in this data set.
  - Shows examples of setting up data-driven styling within the visualisation.json file.


## Building the Image

The `docker` folder contains the required files to build a Docker Image for the example visualisation; the `Dockerfile` file contains the instructions to build an Image; before making any changes to it, please consult the application's developer or the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

- The example visualisation installed within the Docker image will be based on the current commit of this repository, please ensure it is the correct one.
- A valid MapBox API must be provided in your "index" file (see the "Visualisation Framework" wiki page for more).
- A connection to the internet is required to contact remote resources and use the mapping libraries.
- At the time of writing, the JS and CSS files comprising the framework are stored within the example visualisation (in the `js/framework` and `css/framework` directories). In the future, these files will be separated from the example, hosted remotely (perhaps on the kg.cmclinnovations.com site), then imported as remote resources in any visualisation. Until that happens, developers creating a new visualisation can take a copy of the entire `example-dt-vis` directory and use that as a base.

Once the requirements have been addressed, the image can be built using the following methods.

- To build the Image:
  - `docker-compose -f ./docker/docker-compose.yml build --force-rm`
- To generate a Container (i.e. run the Image):
  - `docker-compose -f ./docker/docker-compose.yml up -d --force-recreate`
