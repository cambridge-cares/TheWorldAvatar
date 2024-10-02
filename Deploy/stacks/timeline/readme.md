# Starting the stack

Use port 58085 for test stack when deploying

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

where <STACK_NAME> should be replaced by either "timeline-test" or "timeline".

## Import data from Singapore stack

One of the feature info agent queries relies on imported data from the Singapore stack. Run the following SQL scripts to import data, be sure to replace the IP address and password.

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
