# Utils Module

A module that contains shared general resources (eg. network configuration, deep navigation link, dev credentials, strings) and functions (eg. provide RequestQueue and host RepositoryCallback). 

## Setup
### Network Endpoint
Please add the stack address to `host_with_port` in [network_config.xml](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Apps/TimelineApp/core/utils/src/main/res/values/network_config.xml) in core/utils nodule.
> Example of `host_with_port` http://localhost:3838

### Mapbox
Please configure your development machine as described in [here](https://docs.mapbox.com/android/maps/guides/install/#configure-credentials).

Add the Mapbox public key to [developer_config.xml](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Apps/TimelineApp/core/utils/src/main/res/values/developer_config.xml).