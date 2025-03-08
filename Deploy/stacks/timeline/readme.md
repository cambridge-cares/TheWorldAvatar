# Starting the stack

## Test stack

Stack name should be `timeline-test`.

Prepare these secret files in the [stack secret folder](./stack-manager/test/inputs/secrets)

- geoserver_password
- postgis_password
- keycloak_admin_password_test
- keycloak_admin_username_test

Majority of KeyCloak settings are set using environment variables in [keycloak-test.json].

Set value of KC_DB_PASSWORD to match value in postgis_password.

Set value of KC_HOSTNAME_ADMIN and KC_HOSTNAME to external URL of KeyCloak depending on where it is deployed, e.g. `http://localhost:58085/keycloak`

Preconfigured realm and client settings can be found in dropbox `IRP3 CAPRICORN shared folder\_TWA_Shared_Data\Timeline` (accessible by CARES member only). Copy the files from the folder to the stack [data folder](test/stack-manager/inputs/data). They are required for user agent since [version 2.0.0](https://github.com/orgs/cambridge-cares/packages/container/package/user-agent). Provided files are as follows:
- user_agent_keycloak.json
- keycloak/data/timeline-realm.json

> If the stack is deployed in remote server, please update 
> - `auth-server-url` in [user_agent_keycloak.json](test/stack-manager/inputs/data) accordingly
> - `rootUrl` for user-agent client from `localhost:58085` to the server address in [timeline-realm.json](test/stack-manager/inputs/data/keycloak/data)

You may need to change the permissions of the keycloak startup script, i.e.

```bash
chmod +x ./test/stack-manager/inputs/data/keycloak_start_test.sh
```

Use port 58085 for test stack when deploying the test stack

```bash
./stack.sh start timeline-test 58085
```

At the time of writing, keycloak will fail to start in the first startup due to the absence of the keycloak schema. Create the schema manually in PostGIS, execute this SQL command in adminer:

```sql
CREATE SCHEMA IF NOT EXISTS keycloak
```

then restart stack manager again:

```bash
./stack.sh start timeline-test
```

## Production stack

Stack name should be `timeline`.

Prepare these secret files in [](./test/stack-manager/inputs/secrets)

- geoserver_password
- postgis_password
- keycloak_admin_password
- keycloak_admin_username
- privkey.pem
- fullchain.pem

fullchain.pem is the https certificate file for KeyCloak and privkey.pem is the https certificate key file for KeyCloak. To generate the files and set up https, please refer to [HTTPS setup](#https-setup).

Populate value of KC_DB_PASSWORD in [keycloak-prod.json].

You may need to edit the permissions of keycloak_start.sh, i.e.

```bash
chmod +x ./prod/stack-manager/inputs/data/keycloak_start.sh
```

Then start the stack

```bash
./stack.sh start timeline
```

At the time of writing, keycloak will fail to start in the first startup due to the absence of the keycloak schema. Create the schema manually in PostGIS, execute this SQL command in adminer:

```sql
CREATE SCHEMA IF NOT EXISTS keycloak
```

then restart stack manager again:

```bash
./stack.sh start timeline
```

### HTTPS setup

Files for HTTPS setup are provided in [prod/https/](prod/https/). Please follow this [guide](https://mindsers.blog/en/post/https-using-nginx-certbot-docker/) to generate the certificate and complete the setup. The certificate generated will expire every 3 months and require manual reneration with the current setup.

## Import data from Singapore stack

Few of the feature info agent queries relies on imported data from the Singapore stack. Run the following SQL scripts to import data, be sure to replace the IP address and password.

```sql
CREATE EXTENSION postgres_fdw;

CREATE SERVER foreign_server
    FOREIGN DATA WRAPPER postgres_fdw
    OPTIONS (host 'REPLACE WITH DO IP', port '3840', dbname 'postgres');

CREATE USER MAPPING FOR postgres
    SERVER foreign_server
    OPTIONS (user 'REPLACE WITH USER', password 'REPLACE WITH PASSWORD');

IMPORT FOREIGN SCHEMA public
    LIMIT TO (buildings_layer)
    FROM SERVER foreign_server
    INTO public;
```

## Visualisation setup for test stack

1. Create these two files with the appropriate contents in ./test/vis
   - mapbox_username
   - mapbox_api_key
2. Replace <HOST_ADDRESS> in [./test/vis/vis-files/public/config/data.json](./test/vis/vis-files/public/config/data.json) with the appropriate address (address to access the stack). Also replace <HOST_ADDRESS> (value of REACT_APP_SERVER_URL) in docker-compose.yml.
3. Replace <KEYCLOAK_URL> in vis-files/keycloak.json with the URL of the KeyCloak server, needs to be an address that can be accessed from client and the server.
   1. The configuration assumes a realm called `timeline` exists and a client called `desktop-vis` is set up correctly with the correct redirect urls.
4. Upload [./shacl/timeline.ttl](./shacl/timeline.ttl) to the kb namespace on Blazegraph
5. Download contents of https://github.com/TheWorldAvatar/viz/tree/main/code/public/images/defaults into [./test/vis/vis-files/public/images/defaults](./test/vis/vis-files/public/images/defaults).
6. Run `docker compose up -d` in [./test/vis/](./test/vis/).
7. Visualisation will be accessible at port 3000, e.g. http://[IP_ADDRESS]:3000

[keycloak-test.json]: ./test/stack-manager/inputs/config/services/keycloak-test.json
[keycloak-prod.json]: ./prod/stack-manager/inputs/config/services/keycloak-prod.json