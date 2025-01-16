# Timeline App
This is an app for recoding sensor data and visualise user trajectories.
- User login
- Visualise trajectories
- Record sensor data, including location, acceleration, light ambient and noise level etc.

## Development Setup
### Stack
This app serves as a frontend application and should be connected to a running stack for the complete functionalities.
The stack should include the following services:
- SensorLoggerMobileAppAgent
- AccessAgent
- UserAgent
- TrajectoryQueryAgent
- Keycloak

Please launch the corresponding services in the stack.

### App
#### Login
Please add in the client configuration and auth service information in [auth_config.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Apps/TimelineApp/core/login/src/main/res/raw/auth_config.json) in the core/login module

`authorization_endpoint_uri`, `token_endpoint_uri`, `registration_endpoint_uri` and `user_info_endpoint_uri` can be left blank and the app will retrieve these uri with the `discovery_uri` provided.

#### Network Endpoint
Please add the stack address to `host_with_port` in [network_config.xml](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Apps/TimelineApp/core/utils/src/main/res/values/network_config.xml) in core/utils nodule.
> Example of `host_with_port` http://localhost:3838

#### Mapbox
Please configure your development machine as described in [here](https://docs.mapbox.com/android/maps/guides/install/#configure-credentials).

Add the Mapbox public key to [developer_config.xml](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Apps/TimelineApp/core/utils/src/main/res/values/developer_config.xml).


