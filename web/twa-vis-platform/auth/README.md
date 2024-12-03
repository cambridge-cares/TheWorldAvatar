# Authorisation Server Config

This authorisation stack contains Keycloak for role and access management to an application, a redis-like store for fast in memory session management of your web app, and a database to store the Keycloak data (now including sesions for persistent user sessions through redeployment).

There are two keycloak servers in this file, a development and production server with sensible config as tested within the TWA framework. 
In general, the dev server should be used first to test client interaction, and to set up configuration settings in the keycloak UI to be exported and used to set the production server.
Production server requires some extra configuration of TLS, database integration, session store management, hosting (nginx or relevant proxy management). 
The purpose of this compose project is to take care of most of this, however you will need to specify the location of the auth server.
Refer to the [keycloak guides](https://www.keycloak.org/guides) for detailed guidance on server administration and securing apps.

- Use the compose file to spin up this auth stack.
- Create a .env file in this directory and specify the admin usernames and passwords in the compose file. These are:
    - `KEYCLOAK_BOOTSTRAP_ADMIN` default is set to `admin`
    - `KEYCLOAK_BOOTSTRAP_ADMIN_PASSWORD`, default is `theworldavatar`

> [!NOTE]  
> Since Keycloak 26.0 these have changed, and will be prompted to change in the UI after spinning up. 
> This dispenses with the need to have sensitive admin login stored as an environment variable.

- `POSTGRES_PASSWORD`, default `theworldavatar`
- `PGADMIN_LOGIN_EMAIL` default: `user@example.com`
  - **N.B** you can also create a `postgres-password` file in this directory, uncomment the POSTGRES_PASSWORD_FILE line in the `compose` file and set the postgres password via a docker secret, but this is probably pointless since Keycloak does not support docker secrets so must be passed in as an environment variable anyway. This will be updated if keycloak adds secret support.
  - Run `docker compose up` in this directory.

## Keycloak container

This directory contains the needed info to spin up a *dev config* and a sample *prod config* for Keycloak authorisation of a viz app. This is not suitable for production deployment, instead a production Keycloak server should be used.
There is a sample realm imported by this project called 'twa-test'. 
It can be used to test securing of applications, or you may create your own realm in the UI and export it for the prod server. 
Do not commit exported realms anywhere.

- First, enable Keycloak authorisation by setting the KEYCLOAK environment variable to true in your viz-app's docker compose file or `twa-vis-platform/code/[.env|.env.local]`  (if running a local node server in development).s
- This is a Keycloak dev container. This is *not* suitable for production but is useful for testing the authentication flow of your web app and to create realm settings to be later exported.
- The Keycloak admin console will be running at `http://localhost:8081`.
- There is a `twa-test` sample realm in this directory that is imported on startup. Use the keycloak UI to create a your own one specific to your use case. This can be later exporteed and imported for the production deployment.
- Users must be created manually. Set a name and password. Assigning a user a `protected` role will allow them to access the role-protected pages.
- Ensure that the `keycloak.json` file correctly points to the address of the auth server. This can be `localhost` if running a node server on the bare metal but should be a host that is valid from within the web container. This can be `host.docker.internal` (docker's alias for your host machine) the direct hostname or its IPv4 address
  - **NB** `hosts.docker.internal` only exists in your machine's `hosts` file if docker is installed. If this name does not resolve, you probably need to specify `localhost` or another IP
- The Keycloak UI also needs to know where the web app is running. This is specified in the UI of the Keycloak admin console when you click on clients > job-portal. This is set to `http://localhost:3000` by default but should be changed if your app is running somewhere else.

## Database

The compose file is correctly configured to spin up a postgres database inside the docker network. Keycloak will access it via its url at `jdbc:postgresql://postgres/keycloak`.

This should not require any further configuration.

## PGAdmin

A PGAdmin container is included as a part of this stack, mainly for debug purposes.
You can use it to check if realms and users are being correctly stored in postgres. 
You will need to add your postgres server using the host `postgres` (visible inside the docker network), and database 'keycloak'.

You can also connect to this via adminer if you prefer, by starting an adminer container and connecting to the database on port `5432` which is forwarded to the server host by default (`localhost:5432`). 
This assumes you do not have another database forwarding to localhost.

## Dragonfly

Dragonfly is a fork of redis.
The store is in place to store and cache sessions.
 This will run at port `6379` but you will need to specify the host of the machine running it for a viz app inside a docker container to work. 
 This, like the keycloak server url in your `keycloak.json` will likely need to be the local IP (191.xx.xxx.xxx) or host.docker.internal sometimes works.

This will default to localhost if the node server is running on the bare metal.