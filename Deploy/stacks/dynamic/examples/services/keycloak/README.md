# Example Keycloak Configurations
This folder contains example keycloak configuration.

## Data Folder
`themes/`: contains example CARES email theme for keycloak. Check [here](https://www.keycloak.org/docs/23.0.4/server_development/#_themes) for more information.

`keycloak_start.sh`: Set admin credentials and start keycloak in the container.

## Secret Folder
`keycloak.conf`: The following configurations need to be setup before starting the Keycloak service.

- `hostname-url` and `hostname-admin-url` are for [endpoint configurations](https://www.keycloak.org/server/hostname) behind [reverse proxy](https://www.keycloak.org/server/reverseproxy). Click the link for more information.

- `https-certificate-key-file` and `https-certificate-file` are for [TLS](https://www.keycloak.org/server/enabletls) configuration with PEM certificates.

## Keycloak Container Set Up
1. Copy files from /data to [stack manager inputs data folder](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data).
2. Copy files from /secret to [stack manager secret folder](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/secrets).
3. Copy `keycloak.json` (located at Deploy/stacks/dynamic/stack-clients/src/main/resources/com/cmclinnovations/stack/services/built-ins/keycloak.json) to [stack manager inputs service config folder](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services)
4. Update `hostname-url` and `hostname-admin-url` in `stack-manager/inputs/secrets/keycloak.conf`
5. PEM Certificate Set up
   1. Generate PEM certificate files `keycloak.pem` and `keycloak.key` for Keycloak TLS setup. 
   2. Copy the PEM certificate files to stack manger secret folder
6. Create `keycloak_admin_username` and `keycloak_admin_password` in stack manger secret folder. This will be the credential for the Keycloak Admin Portal.

## Keycloak Resource Client Set Up
The above steps will set up the Keycloak container in stack. To configure authorization on individual agent, please refer to the following steps and resources:
1. Configure the agent with Keycloak Java servlet filter adapter
   - [Java servlet filter adapter: set up authorization on endpoints](https://www.keycloak.org/docs/latest/securing_apps/#_servlet_filter_adapter)
   - A working examples can be found at [BMSUpdateAgent web.xml](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/BMSUpdateAgent/BMSUpdateAgent/src/main/webapp/WEB-INF/web.xml)
   - This adapter has been [deprecated](https://www.keycloak.org/2022/02/adapter-deprecation.html). Alternatives haven't been tested with JPS agents.
2. Register resource client in Keycloak.
   - [Server administration: create and manage realms, users and clients](https://www.keycloak.org/docs/23.0.4/server_admin/)
   - [Step by step guide on managing resource servers](https://www.keycloak.org/docs/23.0.4/authorization_services/#_resource_server_overview)
   - [Client configuration reference](https://www.keycloak.org/docs/23.0.4/server_admin/#con-basic-settings_server_administration_guide)

3. Update `Dockerfile` of the agent 
   1.  Build agent project in exploded file structure form by changing mvn task from `clean package` to `clean compile war:exploded`
   ```
    RUN --mount=type=cache,target=/root/.m2/repository mvn clean compile war:exploded
   ```
   2. Copy the exploded file compiled to tomcat
   ```
    COPY --from=builder /root/BMSUpdateAgent/target/bms-update-agent $CATALINA_HOME/webapps/bms-update-agent
   ```
   - Example `Dokerfile` can be found at [BMSUpdateAgent Dockerfile](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/BMSUpdateAgent/Dockerfile)
3. Build image and push to registry
4. Include the `keycloak.json` adapter file downloaded in step1 as a secret of stack-manager. Update `auth-server-url` to the internal address (`http://<STACK_NAME>-keycloak:8080/keycloak/`) of the keycloak container, because JPS agents run with reverse proxy.
   - Example `keycloak.json` file can be found at [BMSUpdateAgent bms_update_agent_keycloak.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/BMSUpdateAgent/stack-manager-input/secrets/bms_update_agent_keycloak.json)
   - Example stack manager client config at [BMSUpdateAgent bms_update_agent.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/BMSUpdateAgent/stack-manager-input/services/bms_update_agent.json)


## Troubleshoot
- [Admin console not loading and hostname related issues](https://github.com/keycloak/keycloak/issues/14666)
- [Failed to verify token audience in resource client](https://www.keycloak.org/docs/latest/server_admin/#audience-support)
