# Authorisation Server Config

This directory contains the needed info to spin up a *dev config* for keycloak authorisation of a viz app. This is not suitable for production deployment, instead a production keycloak server should be used.

- First, enable keycloak authorisation by seting the KEYCLOAK environment variable to true in your docker compose file or .env file (if running a local node server in development)

- Use the compose file to spin up a dev container of a keycloak instance.
  - Set the master username and password in the compose file, they are `admin` and `password` by default
  - Run `docker compose up` in this directory.
- Keycloak admin console will be running at `http://localhost:8081`.
- Create a new realm called BNL by clicking the dropdown below the logo in the top left of the admin console then 'create realm'.
  - Use the `realm.json` file in this directory to create the realm by dragging it into the box.
- Now go to your realm from the dropdown in the top left where the realm was created. Click on clients > import client and use the `client.json` to create it.
- Go to Realm Roles on the left side bar, if there is no role called `protected`, then make one. (Other roles can be created and used to configure resource access in `server.js`)
- Users must be created manually. Set a name and password. Assigning a user a `protected` role will allow them to access the role protected pages.
- Ensure that the `keycloak.json` file correctly points to the address of the auth server. This can be `localhost` if running a node server on the bare metal, but should be a host that is valid from within the web container. This can be `host.docker.internal` (docker's alias for your host machine) the direct hostname or its IPv4 address
  - **NB** `hosts.docker.internal` only exists in your machines `hosts` file if docker is installed. If this name does not resolve, you probably need to specify `localhost` or another IP
- The Keycloak UI also needs to know where the web app is running. This is specified in the UI of the keycloak admin console when you click on clients > job-portal. This is set to `http://localhost:3000` by default but should be changed if your app is running somewhere else.
