# Developing the TWA-VF

This `/library` directory houses the Typescript, CSS, and HTML files that make up the framework along with some configuration files for compilation. The result of the build process is a Docker image that contains the compiled TWA-VF files along with an Apache web server; this allow users to then attach their content via a Docker volume mounted to `/var/www/html`.

## Development requirements

To develop the TWA-VF the following software and knowledge is required:

* Software on local machine:
  * A working WSL installation (if using Windows).
  * A working Docker installation.
* Required knowledge:
  * Docker
  * HTML & CSS
  * JavaScript/Typescript
  * Mapbox API
  * Cesium API

Before attempting any development on the framework, it is recommended that prospective developers go through the provided TWA tutorials/use cases as well as some examples provided by Mapbox and Cesium (to at least understand their core concepts).

### Optional

The following requirements are optional and only required if developers wish to compile the code on their local machine rather than relying on the Docker image to handle it for them.

* An installation of [Node.js and npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm).
* An installation of a [Typescript](https://www.typescriptlang.org/) compiler.
* An installation of [Grunt](https://gruntjs.com/)
  
This allows the compilation to run on the local machine and the final JS and CSS files to be written to the `output` directory, ready for whatever local testing is needed. Local compilation can be triggered by running the `npm install && tsc && grunt package` command from this directory.

## Runtime requirements

To function correctly, visualisations using this framework also needs to include the below JavaScript libraries. Note that at the time of writing, the TWA-VF is a client based library so each of these requirements is imported as a remote, client-side resource via the head section of the visualisation's `index.html` file.

* [Mapbox GL JS](https://docs.mapbox.com/mapbox-gl-js/api/) or [Cesium JS](https://cesium.com/platform/cesiumjs/)
* [JQuery & JQuery UI](https://jquery.com/)
* [ChartJS](https://www.chartjs.org/)
* [Moment & ChartJS Moment Adapter](https://momentjs.com/)
* [Turf](https://turfjs.org/)
* [Hummingbird Treeview](https://github.com/hummingbird-dev/hummingbird-treeview)

An example of the required import statements should be available in the example [Mapbox](../example-mapbox-vis/webspace/index.html) and [Cesium](../example-cesium-vis/webspace/index.html) visualisations.

## Architecture

> [!NOTE]
> It is worth noting that the current version of the TWA-VF is still a work-in-progress, and was iteratively built upon older code generated under some tight deadlines for demonstration purposes. To that end, the architecture and general quality of the code has room for improvement; plans for a ground-up redesign of the code are in place (as well as adding tests and DevOps features), but have yet to happen in earnest.

The TWA-VF has been developed to use an architecture agnostic to any specific mapping provider wherever possible. To that end, a central `Manager` class is used as point of access, generic `DataGroup`, `DataSource`, and `DataLayer` classes define the data to be displayed by the visualisation, and a variety of UI handlers generate and control the custom visualisation controls.

Where mapping provider specific behaviour is required, concrete instances of the `MapHandler` class are used to directly interface with the JavaScript libraries provided by those providers. At the time of writing, these are the `MapHandler_Mapbox` and `MapHandler_Cesium` classes. In-code documentation should shed more details on the behaviour of specific functions.

## Process

When launched from a correctly integrated HTML file, the TWA-VF follows a set pattern of initialisation and integration with the chosen mapping provider. At the time of writing, the general flow of events is as follows:

* UI elements are created from the static HTML in the visualisation's index file.
* An instance of the manager class is initialised.
* User name & API keys are registered.
* Data source & layer definitions are loaded.
* A blank map is initialised by calling the provider's libraries.
* Default content of the side panel is cached so it can be returned to later.
* Any custom icons or links are loaded.
* Data source and layer definitions are converted to provider specific sources and layers.
* New sources and layers are added to the map for display.
* Event handlers are registered to capture mouse movement/clicks.

## Deployment

The intended workflow of creating TWA visualisations is to use a standard base image that contains the compiled TWA-VF library and an Apache web server to host content.

The base image can be built from this directory by running the `build.sh` script. Note that this will result in a local image tagged with the version number currently set within the `VERSION` file.

Building and pushing the image for a release should be left to the automated GitHub actions, these will build the image and push it (with a number of tags) once a PR has been merged into the `main` branch.

### Automated actions

The following automated GitHub actions have been setup for the TWA-VF (all defined within the [.github](../../../.github) directory).

* Check TWA-VF Version
  * This runs when there's a push action to any non-draft PRs that target the main branch.
  * Checks that the version of the TWA-VF is not the same as the version on the main branch, and does not have the `-SNAPSHOT` qualifier.
* Release the TWA-VF
  * This runs after a PR is approved and the branch merged into main.
  * Performs the following as part of the release process:
    * Reads and parses the version from the `./library/VERSION` file.
    * Builds the TWA-VF Docker image.
    * Pushes the image to the registry with multiple tags.
    * Generates a ZIP of compiled JS & CSS files and uploads as a GitHub release. 
    * Converts HTML content for release email (based on changelog contents, uses Python).
    * Send release email via SMTP connection to CMCL email server.
      * This is done using the `CMCL_MAIL_USERNAME` and `CMCL_MAIL_PASSWORD` repository secrets.

## Planned changes

An overview of bug reports, feature requests, and open PRs can be see using the [TWA Visualisation Framework](https://github.com/orgs/cambridge-cares/projects/1) project. Any new reports or requests should be linked to this project to ensure that it contains a complete overview of all related information.

### Issues

Bugs should be reported as GitHub issues using the `TWA-VF:` prefix along with a short name for the issue. A detailed description of the issue along with reproduction steps, and if possible, [an image of the issue](https://gist.github.com/NawalJAhmed/2168f7659c08b6a033e7f6daf8db69a6).

Issue reporting a bug should also use the provided `bug` tag and link to the TWA Visualisation Framework project.

### Features

Feature requests should be added in a similar manner to bug reports, using the provided `enhancement` tag instead. After filing, any non-trivial feature should be discussed with the development team and the issue updated with the (hopefully clearly scoped) requirements that the feature needs to meet, along with the planned technical approach (if known). 

### Overhaul

A future overhaul of the framework is planned; namely to upgrade to a more platform-like system, comprised of multiple configurable pages and views, built using some of the more widely used JavaScript frameworks (Node.js, React etc.).

Plans for this upgrade are not currently available on this public repository, developers that need access to these documents should check the `Visualisations` channel within the 4Cs section on Microsoft Teams, or contact CMCL directly.

## Support

For support on developing the TWA-VF, please contact the support team at [CMCL](mailto:support@cmcl.io).