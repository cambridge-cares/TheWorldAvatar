#include "esp_wpa2.h"
#include <WiFi.h>
#include <ThingsBoard.h>
#include "time.h"

/*
Proximity sensor for fumehood
An arduino script for checking if the fumehood is under use
Uses HCSR04
to detect the distance of the use to the fumehood.
Connects to Thingsboard to communicate with the Thingsboard agent.

v2.0: detects the distance and send it to Thingsboard. Sends at an interval, and only the distance is sent

@author: Michael Teguh Laksana
*/


#define ECHOPIN 27
#define TRIGGERPIN 25

#define TOKEN "EXAMPLE_TOKEN_HERE"
#define THINGSBOARD_PORT 1883

//connect to lab wifi
const char* ssid = "EXAMPLE_SSID_HERE";
const char* pass = "EXAMPLE_PASSWORD_HERE";

const float MAX_DIST = 350; //cm
const float MIN_DIST = 2; //cm

float dist = MAX_DIST;
const int numRead = 10; //number of reading done in 1 second
const char* fieldName = "EXAMPLE_FIELDNAME_HERE";
WiFiClient client;

unsigned long lastSend;

char thingsboardServer[] = "THINGSBOARD_IP_HERE";
ThingsBoard tb(client);

void setup() {
  Serial.begin(115200);
  delay(10);
  Serial.print(F("Connecting to network: "));
  Serial.println(ssid);
  WiFi.disconnect(true);
  Serial.println("Disconnects prev connection");
  WiFi.mode(WIFI_STA);

	
  Serial.println("Begin connection...");
  /*
  //connnect to EAP
	WiFi.begin(ssid, WPA2_AUTH_PEAP, EAP_ANONYMOUS_IDENTITY, EAP_IDENTITY, EAP_PASSWORD);
  */

  //connect to other wifi
  WiFi.begin(ssid, pass);

  WiFi.setAutoReconnect(true);  

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(WiFi.status());
    Serial.println(F("."));
  }

  Serial.println("Connection begun");

  pinMode(ECHOPIN, INPUT);
  pinMode(TRIGGERPIN, OUTPUT);
  lastSend = 0;

  Serial.println("START---");
}

float getDistance(int tPin,int ePin){
  
  float duration, distance;
    
  digitalWrite(tPin, LOW);  
  delayMicroseconds(5); 
  digitalWrite(tPin, HIGH);  
  delayMicroseconds(10);  
  digitalWrite(tPin, LOW);   
  duration = pulseIn(ePin, HIGH, 24000L);  
  // duration is automatically set to 0 if no pulse is received. Hence, set to MAX_DIST instead.
  distance = duration == 0 ? MAX_DIST : (duration/2) *0.0343;  
  return distance;

}

void sendData (float avgDist) {
  Serial.println("SENDING DATA TO THINGSBOARD");
  tb.sendTelemetryFloat(fieldName, avgDist);
  Serial.println("DATA SENT TO THINGSBOARD");
}

void reconnect() {
  // Loop until we're reconnected
  while (!tb.connected()) {
    Serial.print("Connecting to ThingsBoard node ...");
    // Attempt to connect (clientId, username, password)
    if ( tb.connect(thingsboardServer, TOKEN) ) {
      Serial.println( "[DONE]" );
    } else {
      Serial.print( "[FAILED]" );
      Serial.println( " : retrying in 2 seconds" );
      // Wait 2 seconds before retrying
      delay( 2000 );
    }
  }
}

void loop() {

  //Average reading and if avgDist < threshold -> inUse = true
  if ( !tb.connected() ) {
    reconnect();
  }

  for (uint16_t loops = 0; loops < numRead; loops++){
    dist += getDistance(TRIGGERPIN, ECHOPIN);
    delay(1000/numRead);  
    }
    
    dist = min(dist/numRead, MAX_DIST);
    Serial.println(dist);
    Serial.println("--------");


  if ( millis() - lastSend > 1000 ) { // Update and send only after 1 seconds
    sendData(dist);
    lastSend = millis();
  }


  tb.loop();
}
