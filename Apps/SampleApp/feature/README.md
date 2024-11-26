# Feature Module

This module contains the sub-modules (i.e. interfaces) that users will view and interact with when using the app.

- Home Module
  - Contains the initial view (`HomeFragment`) when starting the app
  - `HomeFragment` contains two buttons to navigate to `PhotoFragment` and `TodoFragment` in the other feature modules
  - Demonstrate deep link navigation without other feature dependencies
- Photo Module
  - An empty feature module for demonstrating the cross module navigation.
- To Do Module
  - `TodoFragment` shows todo item retrieve from internet.
  - Demonstrate
    - Data transfer workflow
    - In module navigation with action
    - Data binding