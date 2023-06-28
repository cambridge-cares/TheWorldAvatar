# Example Keycloak Configurations
This folder contains example keycloak configuration.

Copy files from /data to [stack manager inputs data folder](https://github.com/cambridge-cares/TheWorldAvatar/tree/1584-asset-management-app/Deploy/stacks/dynamic/stack-manager/inputs/data).

Copy files from /secret to [stack manager secret folder](https://github.com/cambridge-cares/TheWorldAvatar/tree/1584-asset-management-app/Deploy/stacks/dynamic/stack-manager/inputs/secrets).

## Data Folder
`themes/`: contains example CARES email theme for keycloak.

## Secret Folder
`keycloak.conf`: The `hostname`, `hostname-port` and `https-key-store-password` in should be configured before launching the Keycloak container.