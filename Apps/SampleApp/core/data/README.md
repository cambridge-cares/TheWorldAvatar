# Data Module

The Data Module corresponds to the repository level in the App Architecture.

The `di/` package is for dependency injection handled by Dagger Hilt. Effectively, repository dependencies that manage the data are injected and stored as singleton instances. This allows them to persist data throughout the application's entire lifecycle.

The `RepositoryCallback` interface serves as the bridge between the ViewModel and the Repository, so that any network responses within the Data Layer are properly passed to the UI layer for further handling. It is moved to utils because repositories in other individual modules may depend on this to send data in between UI layer and data layer as well.

- The interface is implemented in the ViewModel within the `feature` module and passed to the repository.
- It defines what the ViewModel should do when the repository receives results or errors from the network.
- Given the separation of concerns, different ViewModel objects can share data with each other through the repository.

## Testing

We recommend creating an interface for the repository to facilitate the creation of test doubles when unit testing the view models. In this tutorial, `GenericRepository` is the interface that will be implemented. 

This tutorial also demonstrates how to create test doubles of the network sources using the interface for unit testing. The test doubles are found in the `test.java.uk.ac.cam.cares.jps.data.testdouble` subpackage. Test doubles should return hardcoded values and need not call any endpoints.
