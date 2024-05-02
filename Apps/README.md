# The World Avatar App Ecosystem
This repository contains the mobile interfaces developed for users to interact with the knowledge model within The World Avatar. A mobile interface brings greater accessibility and convenience to users with data access on the go.

 This guide is designed to document the standard practices, architectures, and common features for The World Avatar contributors. A template app is available [here](SampleApp/). 

# 1 Getting Started
## 1.1 IDE Recommendation 
We highly recommend using Android Studio as the Integrated Development Environment (IDE) for any app development in The World Avatar. Android Studio provides powerful tools and features specifically tailored for Android app development. Please participate in the [code lab](https://developer.android.com/codelabs/build-your-first-android-app) to familiarise yourself with the IDE.
We also recommend to install the SonarLint plugin for linting purposes.

## 1.2 App Requirements
* Java 17 (jdk17)
* Gradle

## 1.3 Architecture
The apps adopt the [Modern App Architecture](https://developer.android.com/topic/architecture) following the official documentation. Briefly, The World Avatar mobile interfaces utilises only the Data and UI layer. The Data layer contains the business logic of how the app creates, stores, and changes data, and is intended to interact with the knowledge model in The World Avatar. The UI layer displays the application data on the screen via state holders and UI elements for user interactions. Please read the [official documentation](https://developer.android.com/topic/architecture) for more details.

We also briefly introduce some terms specific to Android development:
* Activity

An `Activity` is the basic building block of an Android app's user interface. Each activity serves as an entry point for user interactions through a single screen representation, for example, a login screen, settings screen, and etc.
Activities are responsible for managing their own lifecycle and UI components, handling user input, and responding to system events. 
The TWA apps follow a Single Activity paradigm, where all app functionality is available within a single activity for each application. This simplifies app architecture, navigation flow, and code maintenance.

* Fragment

A `Fragment` is a modular section of an activity's user interface and behavior that MUST be hosted by an activity or another fragment. 
Fragments can be thought of as "sub-activities" that can be combined within an activity to build a multi-pane UI, allowing flexible and reusable UI components across different screen sizes and orientations.

* Application Context

The `Application Context` is a global context provided by the Android system, representing the entire application. 
It provides access to application-specific resources and services, such as accessing files, databases, preferences, and starting activities. 
Unlike an activity context, the application context does not depend on the lifecycle of an activity and can be used safely throughout the entire application.

## 1.4 Directory Structure
The application is structured as modules that can be injected for various purposes to ensure consistency, reusability, and maintainability. At the top level, common build configurations are available at the root, and three modules are available:
* `app`: Serves as the entry point into the application that integrates functionality from the other two modules
* `core`: Contains foundational components and utilities that are shared across the entire application.
    * `data`: Provides data management functionalities, including data sources, repositories, and data access logic. It typically calls the network module to execute data retrieval. 
    * `model`: Defines the data models and entities used throughout the application. 
    * `network`: Handles network operations, such as API requests and responses.
    * `ui`: Contains reusable UI components, themes, and styles that are used across multiple features. 
    * `utils`: Houses utility classes and helper functions that provide common functionalities across the application.
* `feature`: Encapsulate specific features or user-facing functionalities of our application.

Each module directory will have the following files:
* `src`
  * `main` 
      * `java/sub.package.names.any.thing.goes`: Contains code
      * `res`: Contains various resources for designing the UI, such as layout XML files, drawable images, string values, colors, dimensions, and more.
      * `AndroidManifest.xml`: Describes essential information about the app to the Android system such as package name, permissions, services, and more
  * `test/java/sub.package.names.any.thing.goes`: Contains unit tests
  * `androidTest/java/sub.package.names.any.thing.goes`: Contains instrumented tests
* `build.gradle` : Build settings for the module
* Proguard configuration files to shrink, optimise, and obfuscate code for release.

Do note that the internal dependency graph are available in each module's `build.gradle` file to understand how each module is connected to each other.

## 1.5 Test
At present, we are exploring testing strategies for our apps and expect that this section will continue to be updated over time as we explore and consolidate best practices.

Any testing configuration and dependencies should be added to the common build settings at `<root>/common.gradle`. This will allow the test dependencies to be build across all modules. Please ensure that the `org.json:json` package is included as `testImplementation` to ensure that all related code can run. By default, JSON objects are not accessible during the testing mode.

At the moment, users have to individually run the tests at the module level in Android Studio. We are still looking into setting up a test suite to automate this aspect. 
To do so, right click on the module directory in Android Studio and select `Run 'All Tests'` to run the tests for that module.

## 1.6 Next steps
Continue on to the [template app](SampleApp/) for a tutorial on developing a new mobile interface within The World Avatar. If you are experiencing any technical difficulties, the [troubleshooting](#2-troubleshooting) section is available.

# 2 Troubleshooting
## 2.1 Build Compilation
### Incompatible Java dependencies
If you received a similar error as follows, please upgrade your gradle build settings and Project Gradle JDK to Java 17. Java 8 may not be supported in some dependencies.
```
No matching variant of dependency was found. The consumer was configured to find a library for use during runtime, compatible with Java 8, packaged as a jar, and its dependencies declared externally, as well as attribute 'org.gradle.plugin.api-version' with value '8.0' but:              - 
Incompatible because this component declares a component for use during compile-time, compatible with Java 17 and the consumer needed a component for use during runtime, compatible with Java 8
```
### Unable to find required JDK tools
`Error: Kotlin could not find the required JDK tools in the Java installation. Make sure Kotlin compilation is running on a JDK, not JRE.`

Ensure that your Project Gradle JDK is set to a valid Java 17. If all else fails, set your JAVA_HOME environment variable on the local machine to the corresponding installation path.

If this does not work, please ensure that all your gradle build settings are set to Java 17 as well.

## 2.2 Pairing with Physical Device
### USB
The recommended method to connect your physical device to the Android Studio development machine is through a USB cable. 

Procedure:
1) The USB cable must allow data transfer capabilities. Some cables are only for charging. 
2) The USB debugging option in the Developer Settings on your physical device must be checked with permissions to install apps via USB. 
Please check how to access your Developer Settings on your hardware through a Google Search as every device model has unique instructions.

### WIFI
Procedure for pairing via WIFI:
1) Ensure the physical device and development machine are on the same internet connection
2) Set your WIFI or Ethernet to a private network profile. (May not be necessary but might require further configuration to work on a public network profile)
3) Enable Wireless Debugging in the Developer Settings, along with permissions to install apps on your physical device. However, this option is only valid for devices with at least Android 11. It will be difficult to update devices without official updates to Android 11. If the device cannot be updated, please use USB debugging instead. 
4) Within the terminal in the Android Studio IDE, run `adb pair ip-address:port`. It will prompt for the pairing code displayed on your phone. Do note that you should input the debugging IP address, not the phone’s IP address. 
5) Once paired, run `adb connect ip-address`. This phone’s IP address is different from the debugging IP address used in the previous step.
