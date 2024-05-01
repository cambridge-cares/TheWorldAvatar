# Network Module

The network module is crucial for fetching and managing data from remote APIs to populate the Android application.
Each type of data has its own network source class, providing a clear separation of concerns and allowing for easy maintenance and scalability.
Moreover, these network sources are designed to be singleton instances, ensuring efficient memory usage and consistent behavior across the application.

Notes:

- It is recommended to keep any local data sources in a separate module
- Using a single instance of the `RequestQueue` object is advised to streamline network operations and prevent issues. This instance is provided by the `utils` module.
- Unit tests have been excluded as this module requires classes provided by the Android framework (`android.*` or `org.json.*`) that are not available in the local JVM

## Dependency Graph

```mermaid
    stateDiagram-v2
        classDef current font-weight:bold,fill:#146a7d,stroke-width:2px, stroke:#a1c4cb

        cd: core-data
        cm: core-model
        cn: core-network
        cu: core-utils

        cm --> cn:::current
        cu --> cn
        cn --> cd
```
