# The World Avatar (TWA) Visualisation Platform

A central framework for The World Avatar (TWA) Visualisations (the TWA Visualisation Platform, or TWA-ViP) has been created to standardise and simplify the visualisation process. The goal is that a developer does not have to worry about web design, and can set up a reasonable web visualisation displaying landing pages, descriptions, dashboards, and geospatial maps, with some basic configuration files. The platform is the next stage of the Visualisation Framework, and can be [almost deployed following the same steps](#1-precursor).

## Table of Contents

- [The World Avatar (TWA) Visualisation Platform](#the-world-avatar-twa-visualisation-platform)
  - [Table of Contents](#table-of-contents)
  - [1. Precursor](#1-precursor)
  - [2. Development](#2-development)
  - [3. Production](#3-production)
    - [3.1 Docker Deployment](#31-docker-deployment)
    - [3.2 Stack Deployment](#32-stack-deployment)
  - [4 Authorisation](#4-authorisation)
  - [5. Release](#5-release)

## 1. Precursor

As the visualisation platform is intended to be customisable, [configuration files](./doc/config.md) must be included to customise the platform for specific user requirements. If there are any features or functionality you will like to see, please contact the CMCL team or create a new Github issue. Note that these files must be volume-mounted into the Docker container at `/twa/public/`, with specific instructions provided in the relevant deployment sections.

If you are a developer who is adding a new feature, fixing an existing bug, or simply interested in learning more, please read the [Development](#2-development) section. If you are setting up a visualisation for your use cases, please read the [Production](#3-production) section.

For any authorisation capabilities, refer to the [Authorisation](#4-authorisation) section. When releasing the platform as a developer, be sure to review the [Releasing](#5-release) section.

Additionally, there is a tutorial in the [example](./example/) directory, including a sample directory setup. Please check it out if you are setting up the platform for the first time.

## 2. Development

Information on the source code and its architecture can be found in the [code](./code/) directory. Briefly, the TWA Visualisation Platform takes the form of a [Next.js](https://nextjs.org/) project written using [TypeScript](https://www.typescriptlang.org/), utilising both client and server-side codes.

The development process can occur locally or in a Docker container. Please do note that it is faster to develop the platform locally, and instructions are available in the [code](./code#3-local-development-workflow) directory.

On the other hand, Docker deployment is simplified and requires minimal setup. In order to start a Docker container, please ensure the following:

1. Docker is installed
2. Create files within this directory (containing the docker configurations) for `mapbox_username` and `mapbox_api_key` according to your [Mapbox](https://www.mapbox.com/) credentials. This will be passed as Docker secrets when the container is started.
3. Set up the custom [configuration files](./doc/config.md) in the `code/public` directory. Create the `public` directory if it is not there. Sample configuration files can be found at the [example](./example/) directory.
4. Set up the [authorisation server](#4-authorisation) and update the relevant environment variables at `docker-compose.dev.yml` if required.

Once the above steps have been completed, run the command `docker compose -f 'docker-compose.dev.yml' up -d` in this directory. The development server will be set up at `port 3000` on your local machine at `localhost:3000`. Any code changes will be propagated, but may require a browser refresh from time to time.

## 3. Production

The platform is intended to be run on Docker as part of the [TWA stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), and other production workflows are out of the scope of this document. Developers will need to set up several [configuration files](./doc/config.md) in the a directory for bind mounting to get a minimal visualisation. Please read the [documentation](./doc/config.md) for the specific configuration syntax and directory structure. Sample [configuration files](./example/) are also available.

In order to modify the uploaded documents or configurations, the container will build the app after the container has started. Thus, users should expect to wait for a few minutes before the visualisation appears on the webpage.

### 3.1 Docker Deployment

This deployment section is for a standalone Docker container:

1. Create files within this directory (containing the docker configurations) for `mapbox_username` and `mapbox_api_key` according to your [Mapbox](https://www.mapbox.com/) credentials. This will be passed as Docker secrets when the container is started.
2. Set up the custom [configuration files](./doc/config.md) in the `code/public` directory. If you wish to use other file paths, please update the `volumes` value in `docker-compose.yml` accordingly.
3. Set up the [authorisation server](#4-authorisation) and update the relevant environment variables in `docker-compose.yml` if required.
4. If the app will be running behind nginx at somewhere other than a top level domain, specify that path as an `ASSET_PREFIX` environment  variable. e.g. if your app will be hosted at `subdomain.theworldavatar.io/my/viz/app`, then set `ASSET_PREFIX` to `/my/viz/app` in the docker compose file, and nginx should point directly to the `host:port` running the docker container of your app.

> [!IMPORTANT]  
> `ASSET_PREFIX` must start with a slash but not end with one, as in the example above

1. Start the container by running the command `docker compose up -d`. The container will be running on the host machine (whichever the command was run from) at port `80`.

### 3.2 Stack Deployment

For deployment on the [TWA stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), please spin up the stack with the `visualisation` service as documented [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#example---including-a-visualisation). The key steps are as follows:

1. The `mapbox_username` and `mapbox_api_key` are available as Docker secrets
2. Copy the [custom visualisation service config](./example/vip.json) to the `stack-manager/inputs/config/services` directory
3. In the stack config file, `visualisation` is included as part of the `services` `includes` list
4. If the app will be running behind nginx at somewhere other than a top level domain, specify that path as an `ASSET_PREFIX` environment  variable in the service spec file. e.g. if your app will be hosted at `subdomain.theworldavatar.io/my/viz/app`, then set `ASSET_PREFIX` to `/my/viz/app` in `visualisation.json`, and nginx should point directly to the `host:port` running the docker container of your app.

> [!IMPORTANT]  
> `ASSET_PREFIX` must start with a slash but not end with one, as in the example above.

>[!NOTE]
> For typical self-hosted TWA deployment, `ASSET_PREFIX` must contain both the top level nginx path, and the stack level nginx path. e.g. if the app is deployed in a stack at `theworldavatar.io/demos/app`, then `ASSET_PREFIX` should be set to `demos/app/visualisation` to account for the `visualisation` path added by the stack level nginx.

5. Specify the directory holding the configuration files that should be mapped to a volume called `webspace` or your preference
6. . Populate this directory with your require visualisation configuration files
7. Set up the [authorisation server](#4-authorisation) and update the relevant environment variables at `docker-compose.yml` if required.
8. Start the stack as per usual

> Custom Service

At the moment, the `visualisation` service defaults to the [Visualisation Framework](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework). To deploy the TWA ViP, please set up a custom service. A minimal example is available in the [tutorial](./example/vip.json).

## 4 Authorisation

To secure your viz app with a Keycloak authentication server, set the relevant environment variables in the [local node environment file](.code/.env.local) or the relevant compose file in this directory. If running in a stack, the variables will be set in the service spec file. The relevant variables are:

```sh
KEYCLOAK=true|false ## whether or not to use kc authentication on the server
PROTECTED_PAGES=/page,/otherpage ## pages that a user must be logged in to see
ROLE_PROTECTED_PAGES=/role,/protected,/pages ## pages that require a user to have a given REALM or CLIENT role
ROLE=viz:protected ## the role required for the above list
```

alternatively, in the docker `docker-compose.yml` or `docker-compose.dev.yml`

```yml
KEYCLOAK: true|false ## whether or not to use kc authentication on the server
PROTECTED_PAGES: page,/otherpage ## pages that a user must be logged in to see
ROLE_PROTECTED_PAGES: /role,/protected,/pages ## pages that require a user to have a given REALM or CLIENT role
ROLE: viz:protected ## the role required for the above list
```

The [`keycloak.json` file](./code/keycloak.json) must also be correctly configured with the realm name, its address, and the client used for this app. By default, it is configured for the sample auth server committed in [auth](/auth/), but it should be edited if another auth server is in use.

[!NOTE]  
> Crucial information necessary for users to succeed. The most important thing is that the Keycloak server IP address is routable from inside the viz docker container, and outside. The safest way to do this is to specify the IP directly. Sometimes `host.docker.internal` works, but it is often not set in the DNS hosts file of the host machine.

[!NOTE]
> Client roles work better for API-protecting resources than the realm roles. As in the example above, use a role like `<client>:<role>`. See the [documentation in the auth folder](./auth/README.md) to spin up a dev Keycloak server for testing.

## 5. Release

Github Actions has been configured to automatically compile, build, and release the platform when the pull request has been merged. However, before merging the pull request, update the `resources/CHANGELOG.md` and `resources/VERSION` accordingly. Look at the [Wiki](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Versioning) for the versioning format.

Once merged, a release email will be sent to the mailing list based on the `resources/CHANGELOG.md`.
