/***
 * Template .ino file for ESP32 based edge devices.
 * This file is to be used as a template for any sensor attached to an ESP32 to be connected to Thingsboard.
 * By default, ESP32 NodeMCU is used in the template (ESP32 Dev Module board).
 * DO install ESP32 support for Arduino IDE beforehand (https://github.com/espressif/arduino-esp32).
 * MQTT is chosen as the default protocol.
 * 
 * Serial debugging is not provided in this template as Serial comms may be needed for other purposes.
 * 
 * The following libraries are required to be installed for Arduino IDE
 *  - Thingsboard (https://github.com/thingsboard/thingsboard-arduino-sdk)
 *  - 
 * 
 * @author Michael Teguh Laksana
*/

#include <WiFi.h>
#include <ThingsBoard.h>
//To prevent double include in getter.h and sender.h
#define THINGSBOARD true

#include "credentials.h"

WiFiClient client;
ThingsBoard tb(client);

//GET EXT LIB

#include "getter.h"
#include "sender.h"

//DEFINE PIN LABEL

//TEMPLATE GLOBAL

/// @brief Initalizes WiFi connection,
// will endlessly delay until a connection has been successfully established
void InitWiFi() {

  // Attempting to establish a connection to the given WiFi network
  WiFi.begin(WIFI_SSID, WIFI_PASS);
  while (WiFi.status() != WL_CONNECTED) {
    // Delay 500ms until a connection has been succesfully established
    delay(500);
  }
}

/// @brief Reconnects the WiFi uses InitWiFi if the connection has been removed
/// @return Returns true as soon as a connection has been established again
bool reconnect() {
  // Check to ensure we aren't connected yet
  const wl_status_t status = WiFi.status();
  if (status == WL_CONNECTED) {
    return true;
  }
  // If we aren't establish a new connection to the given WiFi network
  InitWiFi();
  return true;
}

/// @brief setup all input and output pin
void setupPin () {
    //TEMPLATE PIN
    return;
}

/// @brief get data from all sensors
void getData () {
    //TEMPLATE GET
    return;
}

/// @brief sends telemetry to Thingsboard server for all fields
void sendTB () {
    //TEMPLATE TELEMETRY
    return;
}


void setup () {
    //setup Wifi
    InitWiFi();

    //setup pin
    setupPin();

    //setup tb
    ThingsBoard tb(client);
}


void loop () {
    //reconnect wifi
    if(!reconnect()){
        return;
    }

    //reconnect TB
    if (!tb.connected()) {
        // Reconnect to the ThingsBoard server,
        // if a connection was disrupted or has not yet been established
        if (!tb.connect(TB_SERVER, TOKEN, TB_PORT)) {
            return;
        }
    }

    //get sensor data
    getData();

    //send sensor data
    sendTB();

    tb.loop();
}
