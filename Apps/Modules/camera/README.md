# Camera Module

This module contains a base camera preview fragment and its layout. The camera fragment is built with [CameraX library](https://developer.android.com/media/camera/camerax) and has implemented the following functions:
- shows preview of the camera 
- on/off flash light
- zoom in/out
- change focus point function
- check and request for camera permission

Extend this base camera class to use the result of capture (eg. QR code scanning, save image capture, text recognition etc.).

## Setup 
Since multiple feature modules can extend the base camera fragment for different use cases, it is recommended to import this module to the `core` module.

1. Click `File > Project Structure...` in the menu bar of the android studio
2. Open the `Modules` tab and click the `+` to add new modules under `core`
3. In the pop up `Create New Module` window, click `Import...`
4. Select this module as source location
5. Set the `Module name` to `:core:camera`
6. Click `Finish` and wait for the module to be imported

## Extend Camera Fragment

The base camera fragment need to be extended for the capture result to be used for different use cases. Navigating directly to this fragment and click the capture button won't have any effect. Here are some helpful links:
- [QR code scanning example](https://github.com/cambridge-cares/TheWorldAvatar/tree/1584-asset-management-app/Apps/AssetManagementApp/feature/qrscan)
- [Google ML Kit](https://developers.google.com/ml-kit): provide various vision APIs such as barcode scanning, face detection and text recognition