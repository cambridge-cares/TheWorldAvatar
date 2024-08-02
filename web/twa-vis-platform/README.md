# The World Avatar (TWA) Visualisation Platform

A central framework for The World Avatar (TWA) Visualisations (the TWA Visualisation Platform, or TWA-ViP) has been created to standardise and simplify the visualisation process. The goal is that a developer does not have to worry about web design, and can set up a reasonable web visualisation displaying landing pages, descriptions, dashboards, and geospatial maps, with some basic configuration files. The platform is the next stage of the Visualisation Framework, and can be almost deployed following the same steps.

The contents of this repository have been structured into Development and Production. If you are a developer who is adding a new feature, fixing an existing bug, or just interested in finding out more, please read the [Development](#1-development) section. If you are setting up a visualisation for your use cases, please read the [Production](#2-production) section. When releasing the platform, please read the [Releasing](#3-release) section.

A tutorial including a sample directory setup has been included in the [example](./example/) directory. Please have a look if you are looking to setup on this new platform.

## Table of Contents

- [The World Avatar (TWA) Visualisation Platform](#the-world-avatar-twa-visualisation-platform)
  - [Table of Contents](#table-of-contents)
  - [1. Development](#1-development)
    - [1.1 Authorisation](#11-authorisation)
  - [2. Production](#2-production)
    - [2.1 Docker Deployment](#21-docker-deployment)
    - [2.2 Stack Deployment](#22-stack-deployment)
  - [3. Release](#3-release)

## 1. Development

Information on the source code and its architecture can be found in the [code](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform/code) directory. Briefly, the TWA Visualisation Platform takes the form of a [Next.js](https://nextjs.org/) project written using [TypeScript](https://www.typescriptlang.org/), utilising both client and server-side codes.

The development process can occur locally or in a Docker container. Please do note that it is faster to develop the platform locally, and instructions are available in the [code](./code#3-local-development-workflow) directory.

On the other hand, Docker deployment is simplified and requires minimal setup. In order to start a Docker container, please ensure the following:

1. Docker is installed
2. Create files within this directory (containing the docker configurations) for `mapbox_username` and `mapbox_api_key` according to your [Mapbox](https://www.mapbox.com/) credentials. This will be passed as Docker secrets when the container is started.

Once the above steps have been completed, run the command `docker compose -f 'docker-compose.dev.yml' up -d` in this directory. The development server will be set up at `port 3000` on your local machine at `localhost:3000`. Any code changes will be propagated, but may require a browser refresh from time to time.

### 1.1 Authorisation

To secure your viz app with a keycloak authentication server, set the relevant environment variables in the [local node environment file](.code/.env.local) or the relevant compose file in this directory. If running in a stack, the variables will be set in the service spec file. The relevant variables are:

```sh
KEYCLOAK=true|false ## whether or not to use kc authentication on the server
PROTECTED_PAGES=/page,/otherpage ## pages that a user must be logged in to see
ROLE_PROTECTED_PAGES=/role,/protected,/pages ## pages that require a user to have a given REALM or CLIENT role
ROLE=protected ## the role required for the above list
```

The [`keycloak.json` file](./code/keycloak.json) must also be correctly configured with the realm name, its address, and the client used for this app. By default it is configured for the sample auth server committed in [auth](/auth/), but it should be edited if another auth server is in use.

See the [documentation in the auth folder](./auth/README.md) to spin up a dev keycloak server for testing.

## 2. Production

The platform is intended to be run on Docker as part of the [TWA stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), and other production workflows are out of the scope of this document. Developers will need to set up several configurations in the `upload` directory to get a minimal visualisation - namely, `landing-page.md`, `data.json`, `map-settings.json`, and `ui-settings.json`. Please read the documentation in the `upload` directory for the specific configuration syntax.

In order to modify the uploaded documents, the container will build the app after the container has started. Thus, users should expect to wait for a few minutes before the visualisation appears on the webpage.

Do note to pass a `BASE_PATH` environment variable if you are using any subpaths. Read [this section](./code#16-reverse-proxy-urls) for more details.

### 2.1 Docker Deployment

For a standalone deployment, first create files within this directory (containing the docker configurations) for `mapbox_username` and `mapbox_api_key` according to your [Mapbox](https://www.mapbox.com/) credentials. This will be passed as Docker secrets when the container is started. Start the container by running the command `docker compose up -d`. The container will be running on your local machine at `localhost:80`.

### 2.2 Stack Deployment

For deployment on the [TWA stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), please spin up the stack with the `visualisation` service as documented [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#example---including-a-visualisation). The key steps are as follows:

1. The `mapbox_username` and `mapbox_api_key` are available as Docker secrets
2. In the stack config file, `visualisation` is included as part of the `services` `includes` list
3. Specify the directory holding the configuration files that should be mapped to a volume called `webspace` or your preference
4. Populate this directory with your require visualisation configuration files
5. Start the stack as per usual

> Custom Service

At the moment, the `visualisation` service defaults to the [Visualisation Framework](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework). To deploy the TWA ViP, please set up a custom service. A minimal example is available in the [tutorial](./example/), specifically `example/vip.json`.

If you wish to use other subpaths, please note to update the `BASE_PATH` environment variable to match the external path property.

## 3. Release

Github Actions has been configured to automatically compile, build, and release the platform when the pull request has been merged. However, before merging the pull request, update the `CHANGELOG.md` and `VERSION` accordingly. Look at the [Wiki](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Versioning) for the versioning format.

Once merged, a release email will be sent to the mailing list based on the `CHANGELOG.md`.
