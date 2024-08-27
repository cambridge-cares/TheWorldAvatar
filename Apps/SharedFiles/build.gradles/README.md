# Build Gradle Files

This folder contains the shared build files which can be applied in sub-modules. Having these configurations in separate files from the modules `build.gradle` helps with library version management, such that developer can modify the library version in a single place.

- `common.gradle`
    - The file contains the basic configuration for a TWA Android app module. It should be applied in all TWA Android app modules.
- `ui.gradle`
    - It is a file for UI related modules and mainly used in feature modules.
    - It contains settings of view binding and data binding and UI related libraries such as navigation and material.

## Setup
1. Copy and paste these two files in the root level of the project folder
2. For each module, modify the module level `build.gradle` to the following format
    ```groovy
    apply plugin: 'com.android.library'
    apply from: "${project.rootDir}/common.gradle"
    apply from: "${project.rootDir}/ui.gradle"

    android {
        namespace '<MODULE-NAMESPACE>'
    }

    dependencies {
        // other additional dependencies (libraries and in-project modules)
    }
    ```

Please refer to [TWASampleApp](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Apps/SampleApp) for a complete example.