# The World Avatar (TWA) Visualisation Platform

A central framework for The World Avatar (TWA) Visualisations (the TWA Visualisation Platform, or TWA-ViP) has been created to standardise and simplify the visualisation process. The goal is that a developer does not have to worry about web design, and can set up a reasonable web visualisation displaying landing pages, descriptions, dashboards, and geospatial maps, with some basic configuration files. 

The contents of this repository have been structured into Development and Production. If you are a developer who is adding a new feature, fixing an existing bug, or just interested in finding out more, please read the [Development](#1-development) section. If you are setting up a visualisation for your use cases, please read the [Production](#2-production) section.


## 1. Development
Information on the source code and its architecture can be found in the [code](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform/code) directory. Briefly, the TWA Visualisation Platform takes the form of a [Next.js](https://nextjs.org/) project written using [TypeScript](https://www.typescriptlang.org/), utilising both client and server-side codes. 

The development process can occur locally or in a Docker container. Please do note that it is faster to develop the platform locally, and instructions are available in the [code](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform/code#development-requirements) directory. 

For Docker development, please ensure the following:
1) Docker is installed
2) Update the `MAPBOX_USERNAME` and `MAPBOX_API_KEY` environment variables in the `docker-compose.dev.yml` accordingly to your [Mapbox](https://www.mapbox.com/) credentials

Once the above steps have been completed, run the command `docker compose -f 'docker-compose.dev.yml' up -d` in this directory. The development server will be set up at port 80 on your local machine. Any code changes will be propagated, but may require a browser refresh from time to time.

## 2. Production
The platform is intended to be run on Docker as part of the [TWA stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), and other production workflows are out of the scope of this document. Developers will need to set up several configurations in the `upload` directory to get a minimal visualisation - namely, `landing-page.md`, `data.json`, `map-settings.json`, and `ui-settings.json`. Please read the documentation in the `upload` directory for the specific configuration syntax.

### 2.1 Docker Deployment
For a standalone deployment, first add the `MAPBOX_USERNAME` and `MAPBOX_API_KEY` environment variables in the `docker-compose.yml` accordingly to your [Mapbox](https://www.mapbox.com/) credentials. Start the container by running the command `docker compose up -d`.


### 2.2 Stack Deployment
For deployment on the [TWA stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), please spin up the stack with the `visualisation` service as documented [here][https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#example---including-a-visualisation]. The key steps are as follows:

1) The `mapbox_username` and `mapbox_api_key` are available as Docker secrets
2) In the stack config file, `visualisation` is included as part of the `services` `includes` list
3) Specify the directory holding the configuration files that should be mapped to a volume called `uploads`
4) Populate this directory with your require visualisation configuration files
5) Start the stack as per usual