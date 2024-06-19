# Login Module

This module contains two parts: core and feature. It integrates [AppAuth for Android](https://github.com/openid/AppAuth-Android) to turn the app to an OAuth2.0 and OpenId Connect client and communicate with authentication provider (eg. Keycloak) for authentication.

## Setup

### 1. Import
1. Click `File > Project Structure...` in the menu bar of the android studio

> Import :core:login
2. Open the `Modules` tab and click the `+` to add new modules under `core`
3. In the pop up `Create New Module` window, click `Import...`
4. Select [/core/login](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Apps/Modules/core/login) as source location
5. Set the `Module name` to `:core:login`
    1. You may see warning about missing `:core:utils` module. Ignore this warning while importing and fix the error in code after import.

> Import :feature:login
6. Click the `+` to add new modules under `feature`
7. In the pop up `Create New Module` window, click `Import...`
8. Select [/feature/login](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Apps/Modules/feature/login) as source location
9. Set the `Module name` to `:feature:login`

10. Click `Finish` and wait for the module to be imported

### 2. Config
> Authentication Provider Config
1. Register the app as a client in the auth provider and fill the `res/raw/auth_config.json` accordingly. More information for configuring `auth_config.json` can be found [here](https://github.com/openid/AppAuth-Android/blob/master/app/README.md).
2. Add the redirect scheme you used in the `auth_config.json` to `manifestPlaceHolder` in the `common.gradle`
    ```groovy
        android {
            ...

            defaultConfig {
                ...

                manifestPlaceholders = [
                        'appAuthRedirectScheme': ''
                ]
            }
        }
    ```
> Navigation
3. Add `login_fragment_link` in `/core/utils/src/main/res/values/navigation_link.xml`. It should be set to the deep link of the login fragment.
    ```xml
    <string name="login_fragment_link"></string>
    ```
4. Modify `LoginFragment.java` to navigate to the desired page after login.
5. Add `:feature:login` to `/app/build.gradle`
6. Include login map to the `/app/src/main/res/navigationapp_navigation.xml`
    ```xml
    <include app:graph="@navigation/login_map"/>
    ```
    And change the `app:startDestination` to the login map
    ```xml
    <navigation 
    ...
    app:startDestination="@id/login_map">
    ```
