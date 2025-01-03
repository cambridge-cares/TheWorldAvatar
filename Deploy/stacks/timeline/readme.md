# Starting the stack

## Test stack

Stack name should be `timeline-test`.

Prepare these secret files in [](./stack-manager/test/inputs/secrets)

- geoserver_password
- postgis_password
- keycloak_admin_password_test
- keycloak_admin_username_test

Majority of KeyCloak settings are set using environment variables in [keycloak-test.json].

Set value of KC_DB_PASSWORD to match value in postgis_password.

Set value of KC_HOSTNAME_ADMIN and KC_HOSTNAME to external URL of KeyCloak depending on where it is deployed, e.g. `http://localhost:58085/keycloak`

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

Prepare these secret files in [](./stack-manager/prod/inputs/secrets)

- geoserver_password
- postgis_password
- keycloak_admin_password
- keycloak_admin_username
- privkey.pem
- fullchain.pem

fullchain.pem is the https certificate file for KeyCloak and privkey.pem is the https certificate key file for KeyCloak. To generate the files, please refer to <https://mindsers.blog/en/post/https-using-nginx-certbot-docker/>.

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
5. Run `docker compose up -d` in [./test/vis/](./test/vis/).
6. Visualisation will be accessible at port 3000, e.g. http://[IP_ADDRESS]:3000

[keycloak-test.json]: ./stack-manager/test/inputs/config/services/keycloak-test.json
[keycloak-prod.json]: ./stack-manager/test/inputs/config/services/keycloak-prod.json