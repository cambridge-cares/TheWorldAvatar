# **SensorLogger**

## **1.  Description**

The SensorLogger mobile app is an easy-to-use data logger, designed to track the various in-built sensors on an android phone. Supported sensors and measurements include:

* Device Acceleration (Accelerometer)
* Gravity Vector (Accelerometer)
* Device Rotation (Gyroscope)
* Magnetic Field Direction (Magnetometer)
* Atmospheric Pressure (Barometer)
* Location Data:
  * Latitude and Longitude 
  * Barometric Altitude 
  * Device Speed 
  * Bearing
* Ambient Light Level (Light sensor)
* dBFS (Microphone)


## **2. Understanding Timestamps**

All the sensor data that is recorded and exported have synchronised timestamps, which means that the data can be cross-referenced easily by comparing the timestamps.

* The "time" field in the payload follows the UNIX epoch timestamp of measurement, reported in nanoseconds. To convert them into 
understandable timestamps, use tools like [this](https://www.epochconverter.com/).
* Note that the accuracy of the timestamps depends on the device's accuracy.


## **3. Understanding Units**

* Time: Nanoseconds since epoch.
* Accelerometer:
  * x, y, and z in meters per second squared (m/s^2^)
  * g is assumed to be 9.8067 m/s^2^
* Gravity:
  * x, y, and z in meters per second squared (m/s^2^)
  * g is assumed to be 9.8067 m/s^2^
* Gyroscope:
  * x, y, and z in radians per second (rad/s)
* Magnetometer:
  * x, y, and z in micro teslas (μT)
* Barometer:
  * Pressure is recorded in Hectopascal (hPa)
* Light Sensor:
  * Measured in lux
* Microphone:
  * Takes values in decibels and calculates decibels relative to full scale (dimensionless).
* Location:
  * Latitude, in degrees. Positive values are north of the equator (-90 to 90). 
  * Longitude, in degrees. Positive values are east of the meridian line (-180 to 180). 
  * Altitude, in meters (m) above mean sea level. 
  * Speed, in meters per second (m/s). 
  * Bearing, in degrees. 
  * Horizontal Accuracy, in meters for radius of certainty to the 68^th^ percentile CI. 
  * Vertical Accuracy, in meters for altitude accuracy to the 68^th^ percentile CI. 
  * Bearing Accuracy, in degrees to 68^th^ percentile CI. Negative values mean it is invalid. 
  * Speed Accuracy, in m/s to 68^th^ percentile CI.


## **4. Payload Format**

Once the recording starts, sensor data is recorded and logged into a pre-determined JSON format, which is stored in memory. In this format, the contents are an array of objects, and each object 
represents a single record. All records for a sensor are grouped together and are then sorted according to a pre-defined order of sensors. Below is an example JSON payload:

```json
[
  {
    "name": "accelerometer",
    "time": 1714450132726000000,
    "x": 4.7575297355651855,
    "y": 8.639883995056152,
    "z": 3.483351469039917
  },
  {
    "name": "light",
    "time": 1714378661375000000,
    "lux": 6947.4
  },
  {
    "name": "location",
    "time": 1714356064586000000,
    "latitude": 38.0981,
    "longitude": -121.4599,
    "altitude": 44.66,
    "speed": 0,
    "bearing": 0,
    "horizontalAccuracy": 18.902,
    "bearingAccuracy": 0,
    "speedAccuracy": 0,
    "verticalAccuracy": 2.867
  }
] 
```


## **5. Pre-requisites**
Before running the application, ensure the following prerequisites are met:

### 1. Update POST Request URL
For the `sendPostRequest` method in the `MainActivity` class, the URL string must be updated in the `strings.xml` file. This URL is used for sending sensor data from the app to the server.

### 2. Configure the `network_security_config.xml` file
Ensure the `network_security_config.xml` file contains the correct domain or IP address for your server. This step is crucial to allow your app to communicate securely over the network. Add or update the domain under the `<domain-config>` tag.

### 3. Mapbox Token & Secret Key
The Mapbox SDK requires a token to access mapping functionalities. [Refer to this document](https://docs.mapbox.com/android/maps/guides/install/#configure-your-secret-token) to learn how to set up your secret token. 
Add your secret token to the app’s `gradle.properties` file as follows:
```MAPBOX_DOWNLOADS_TOKEN=YOUR_SECRET_TOKEN_HERE```
Similarly, add the same secret token to `mapbox_access_token` in the `strings.xml` file.



## **6. Live Data Streaming**

Sensor Logger supports pushing live data via HTTP. All you have to do is simply toggle on the required sensors and press the **Start Recording** button. All enabled sensors during a recording will be streamed every 2000ms (2 seconds) to the specified URL. 
To display the streamed data, you will need to set up a webserver. There are 2 ways you can view your recorded data:

1. Follow [this guide](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/SensorLoggerMobileAppAgent) and set up a stack and access agent locally to test the app.
2. Send your data to RequestBin. Refer to [this guide](https://requestbin.com/docs/#create-an-endpoint) to setup a RequestBin account and get started.

HTTP Push sends a POST request to the supplied URL with the request body of the following format:

```json
{
    "messageId": 1,
    "sessionId": "identifier",
    "deviceId": "identifier",
    "payload": [
        {
            "name": "accelerometer",
            "time": 1698501144401773000,
            "<other fields depending on sensor>"
        }
    ]
} 
```
The `messageId` always starts at 1 for each separate recording and is incremented for each payload sent. The `messageId` is useful because the messages can be received out-of-order, which may need to be handled depending on the use-case. 
The `sessionId` is randomly generated, and the `deviceId` is the same for all messages from a single device. However, note that if the app is uninstalled and reinstalled, the `deviceId` will change. The `time` is in UTC epoch nanoseconds.

