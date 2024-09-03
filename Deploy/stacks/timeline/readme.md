Use port 58085 for test stack when deploying
```
./stack.sh start timeline-test 58085
```

At the time of writing, keycloak will fail to start in the first startup due to the absence of the keycloak schema. Create the schema manually in PostGIS, execute this SQL command in adminer: 

```
CREATE SCHEMA IF NOT EXISTS keycloak
```

then restart stack manager again:

```
./stack.sh start timeline-test
```

Execute the following to set up access agent:
```
./../../../Agents/SensorLoggerMobileAppAgent/copy.sh start timeline-test
```
where <STACK_NAME> should be replaced by either "timeline-test" or "timeline".
