# Asset Management App
This is an app designed to assist in the management of assets and their relevant information.
- User login
- Query assets information via QR code
- filter queried information for each asset
- Add new assets
- QR code printing

Features planned but not developed yet:
- Edit existing assets information
- Manage the maintenance schedule of assets
- Mailbox for notifications and alerts regarding assets that are assigned to the user
- Inventory sweep to assist in asset auditing

## Development Setup
### Stack
This app serves as a frontend application and should be connected to a running stack for the complete functionalities.
The stack should include the following services:
- [AssetManagerAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AssetManagerAgent)
- Keycloak

Please launch the corresponding services in the stack.

### App
#### Login
Please add in the client configuration and auth service information in [auth_config.json](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Apps/AssetManagementApp/core/login/src/main/res/raw/auth_config.json) in the core/login module.

`authorization_endpoint_uri`, `token_endpoint_uri`, `registration_endpoint_uri` and `user_info_endpoint_uri` can be left blank and the app will retrieve these uri with the `discovery_uri` provided.

#### Network Endpoint
Please add the stack address to `host` in [endpoint_config.xml](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Apps/AssetManagementApp/core/network/src/main/res/values/endpoint_config.xml).
> Example of `host` http://localhost:3838.

If testing is to be carried out, add the test stack address to `test_host` in [endpoint_config.xml](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Apps/AssetManagementApp/core/network/src/main/res/values/endpoint_config.xml).
> Example of `host` http://localhost:3838.