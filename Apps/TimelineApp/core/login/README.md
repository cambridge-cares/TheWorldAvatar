# Login Module
A reusable module which handles the user login function with authentication provider (eg. Keycloak). For more information please refer to [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Apps/Modules/login).

## Quick Setup
### Login
Please add in the client configuration and auth service information in [auth_config.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Apps/TimelineApp/core/login/src/main/res/raw/auth_config.json) in the core/login module

`authorization_endpoint_uri`, `token_endpoint_uri`, `registration_endpoint_uri` and `user_info_endpoint_uri` can be left blank and the app will retrieve these uri with the `discovery_uri` provided.
 
Please use Keycloak version 25.0.6 and include the user ID n the Keycloak JWT token by: 
- Opening the Keycloak Admin Console in your browser and logging in
- Select the realm 'timeline' from the dropdown menu
- Go to clients tab on the left hand side. 
- Select twa-timeline-app, then client scopes. 
- Under client scopes select twa-timeline-app-dedicated and select add mapper. 
  - Select by configuration
  - Scroll to select Subject (sub)
  - Ensure add to access token and add to introspection are toggled 
  - Name the mapper
  - Click save to apply the changes. 
- To debug the access token you can print it to console and use jwt.io to check if the gisub claim now contains the user ID instead of the default user identifier. 