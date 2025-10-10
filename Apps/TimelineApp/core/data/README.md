# Data Module
A module to host application specific data module level components.

## Overview
This module defines repositories and dependency injection (DI) specifications for handling user 
rajectories and dates-related data in an Android application. It utilizes Dagger Hilt for dependency injection, for efficient management of data sources and user login details.

The module provides two key repositories:

- TrajectoryRepository: Responsible for managing trajectory-related requests.

- DatesWithTrajectoryRepository: Manages requests related to dates that contain trajectory data.

- AppPreferenceRepositoru: Manages request related to app preference. It requires LoginRepository for user ID to get the preference file for the current user.


## Structure
- DataModule (DI Module): Defines the dependency injection specifications using Dagger Hilt. It provides instances of TrajectoryRepository and DatesWithTrajectoryRepository to the rest of the application.

- DatesWithTrajectoryRepository: Provides access to date-related data that contain trajectory information. 
It interacts with:
    - DatesWithTrajectoryNetworkSource: Handles the network requests to fetch dates that contain trajectory data.
    - LoginRepository: Manages user authentication and retrieves user information.

- TrajectoryRepository: Responsible for managing trajectory-related data. It interacts with:
    - TrajectoryNetworkSource: Handles the network requests to fetch trajectory data
    - LoginRepository: Manages user authentication and retrieves user information.

## How It Works
1. LoginRepository: The repositories depend on the LoginRepository to authenticate and retrieve user information.

2. Network Sources: Once the user is authenticated, network sources (TrajectoryNetworkSource and DatesWithTrajectoryNetworkSource) are used to fetch data from the server.

3. Callbacks: When data has been fethced, it is returned to the UI layer through callback interfaces (RepositoryCallback), enabling asynchronous communication between the repository and the UI components.