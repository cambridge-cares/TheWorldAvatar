# Digital Twin Visualisation Framework (DTVF)

A central framework for Digital Twin Visualisations (the Digital Twin Visualisation Framework, or DTVF) has been created so that in most cases, the process of creating a new visualisation to display pre-generated data is as simple as possible. The goal is that a developer that is inexperienced with Typescript (or the Javascript libraries we're using) can get a reasonable visualisation of the data by simply ensuring the data meets a set format and providing some basic metadata files.

This directory houses the Typescript, CSS, and HTML files that make up the framework along with some configuration files to make them available for deployment/hosting. When creating visualisations, the hosted version of this framework should be used; files from this directory should not be copied into/directly linked to visualisation implementations.

This directory also houses Docker configuration files to create an base image that can be used to create individual visualisations. A volume containing the webspace files simply needs to be added (as a volume) to the `/var/www/html/` directory; a example of this can be see in the Docker configuration files for the two example visualisations.

For more details on the framework, see the [Digital Twin Visualisations](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations) page on the GitHub wiki. For an example implementation of the framework, see the example implementations in the parent directory of this one in the repository.

### How to use the framework

This README has been put together for developers wishing to understand the technial workings of the DTVF. For details on how to import, configure, and use the framework, please read the documentation starting on the [GitHub wiki](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations) page.

## Requirements

To function correctly, whichever visualisation implementation is using this framework also needs to include the following JS libraries. Note that at the time of writing, the DTVF is a client based library so each of these requirements is imported as a remote, client-side resource via the head section of the visualisation's `index.html` file.

- [Mapbox GL JS](https://docs.mapbox.com/mapbox-gl-js/api/)
- [JQuery & JQuery UI](https://jquery.com/)
- [ChartJS](https://www.chartjs.org/)
- [Moment & ChartJS Moment Adapter](https://momentjs.com/)
- [Turf](https://turfjs.org/)
- [SunCalc](https://github.com/mourner/suncalc)
- [Hummingbird Treeview](https://github.com/hummingbird-dev/hummingbird-treeview)

## Development

A number of configuration files are also present that allow the generation of a Docker container that can be used as a live development environment. The container houses [TypeScript](https://www.typescriptlang.org/) and [Grunt](https://gruntjs.com/) installations. These can be used for active development, compile-time checks/linting, or generation of deployable files.

To spin up the container and use it as a development environment via VSCode...

- Run the `docker-compose up develop` command from within this directory.
- Select the `Remote Explorer` menu within VSCode.
  - You many need to install the `Docker` extension if you haven't done so already.
- Select the `Attach to Container` button for the `dtvf-devel` container.
- Once a new VSCode window appears (and the loading completes), you can now open a file/folder within the container and begin development.
- The entire `library` directory has been set up to share between your local machine and the container. This means that any changes you make within the container, will persist to your local filesystem.

## Compilation

To compile and combine the DTVF library into deployable JS files...

- Run the `docker-compose up compile` command from within this directory.
- The container will compile and process the typescript files for deployment, then shutdown.
  - If successful, single JS and CSS files will be generated within the `output` directory.
  - These can then be uploaded to a web server to make them publicly available.
    - To upload these to the KG website server, please contact the support team at CMCL.


## Deployment of Base Image

To build and deploy the base DTVF image, so that others can use it to create visualisations...

- Run the `docker-compose build deploy` command from within this directory.
- The image should then be built and tagged as `ghcr.io/cambridge-cares/dtvf-base-image:latest`
- Use the `docker push ghcr.io/cambridge-cares/dtvf-base-image:latest` command to upload the image.
  - This requires having set up a GitHub token and storing it using the `docker login` command. 