# Data Module

The Data Module corresponds to the repository level in the App Architecture.

The `di/` package is for dependency injection handled by Dagger Hilt. Effectively, repository dependencies that manage the data are injected and stored as singleton instances. This allows them to persist data throughout the application's entire lifecycle.

The `RepositoryCallback` interface serves as the interface between the ViewModel and the Repository:

- It is created and defined in ViewModel from the `feature` module and passed to the repository.
- It defines what the ViewModel should do when the repository receives results or errors from the network.
- Given the separation of concerns, different ViewModel objects can share data with each other through the repository.
