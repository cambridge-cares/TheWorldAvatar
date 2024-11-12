# Login Module
A reusable module which handles the user login function with authentication provider (eg. Keycloak). For more information please refer to [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Apps/Modules/login).

## Quick Setup
### Login
Please add in the client configuration and auth service information in [auth_config.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Apps/TimelineApp/core/login/src/main/res/raw/auth_config.json) in the core/login module

`authorization_endpoint_uri`, `token_endpoint_uri`, `registration_endpoint_uri` and `user_info_endpoint_uri` can be left blank and the app will retrieve these uri with the `discovery_uri` provided.
