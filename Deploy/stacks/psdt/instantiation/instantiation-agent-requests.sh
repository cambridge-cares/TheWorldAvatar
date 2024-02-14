#!/bin/bash

## House 45 utilities
curl -X POST --header "Content-Type: application/json" -d '{"clientProperties":"TIMESERIES_CLIENTPROPERTIES"}' localhost:3838/historical-house45-utilities-agent/retrieve

## Thingspeak solar sensor
curl -X POST --header "Content-Type: application/json" -d '{"agentProperties":"THINGSPEAK_AGENTPROPERTIES","apiProperties":"THINGSPEAK_APIPROPERTIES","clientProperties":"THINGSPEAK_CLIENTPROPERTIES"}' http://localhost:3838/thingspeak-agent/retrieve

## Hugo Ball (NB iriPrefix can be changed to a full URI)
curl -X POST --header "Content-Type: application/json" -d '{"timeHeader":"TimeStamp", "iriPrefix":"hugoball/", "addTriple":"False"}' http://localhost:3838/historical-pump-data-instantiation-agent/run

## Pumping stations (NB iriPrefix can be changed to a full URI)
curl -X POST --header "Content-Type: application/json" -d '{"timeHeader":"Year", "iriPrefix":"pump/", "addTriple":"True", "multiTSColIndex": "0"}' http://localhost:3838/historical-pump-data-instantiation-agent/run

## District heating
curl -X POST --header "Content-Type: application/json" -d '{"endpoint":"http://psdt-access-agent:8080/districtheating"}' http://localhost:3838/district-heating-agent/performheatupdate

## Sewage system
curl -X POST --header "Content-Type: application/json" -d '{"endpoint":"http://psdt-access-agent:8080/sewagenetwork"}' http://localhost:3838/sewage-network-agent/performsewageupdate

## ZIMEN measurement station
curl -X POST --header "Content-Type: application/json" -d '{"agentProperties":"HISTORICAL_AGENTPROPERTIES","connectorProperties":"HISTORICAL_CONNECTORPROPERTIES","clientProperties":"HISTORICAL_CLIENTPROPERTIES"}' http://localhost:3838/historical-pirmasens-station-agent/retrieve

## Solarkataster
curl -X POST --header "Content-Type: application/json" -d '{"table":"solarthermie","chunk":50}' http://localhost:10101/solarkataster_agent/run

## IfcOwlConverter
curl -X POST --header "Content-Type: application/json" -d '{"uri":"https://www.theworldavatar.com/kg/ps/measurementstation-ZIMEN/BIM/"}' http://localhost:3838/ifcowlconverter/
curl -X POST --header "Content-Type: application/json" -d '{"uri":"https://www.theworldavatar.com/kg/ps/house45/BIM/"}' http://localhost:3838/ifcowlconverter/
curl -X POST --header "Content-Type: application/json" -d '{"uri":"https://www.theworldavatar.com/kg/ps/Leibniz/BIM/"}' http://localhost:3838/ifcowlconverter/
curl -X POST --header "Content-Type: application/json" -d '{"uri":"https://www.theworldavatar.com/kg/ps/HugoBall/BIM/"}' http://localhost:3838/ifcowlconverter/

## Ifc2OntoBim
curl -X POST --header "Content-Type: application/json" -d '{"uri":"https://www.theworldavatar.com/kg/ps/measurementstation-ZIMEN/BIM/", "isIfcOwl":true}' http://localhost:3838/ifc2ontobim-agent/convert
curl -X POST --header "Content-Type: application/json" -d '{"uri":"https://www.theworldavatar.com/kg/ps/house45/BIM/", "isIfcOwl":true}' http://localhost:3838/ifc2ontobim-agent/convert
curl -X POST --header "Content-Type: application/json" -d '{"uri":"https://www.theworldavatar.com/kg/ps/Leibniz/BIM/", "isIfcOwl":true}' http://localhost:3838/ifc2ontobim-agent/convert
curl -X POST --header "Content-Type: application/json" -d '{"uri":"https://www.theworldavatar.com/kg/ps/HugoBall/BIM/", "isIfcOwl":true}' http://localhost:3838/ifc2ontobim-agent/convert

## Ifc2Tileset
curl -X POST --header "Content-Type: application/json" -d '{"assetUrl":"./zimen"}' http://localhost:3838/ifc2tileset-agent/api
curl -X POST --header "Content-Type: application/json" -d '{"assetUrl":"./house45"}' http://localhost:3838/ifc2tileset-agent/api
curl -X POST --header "Content-Type: application/json" -d '{"assetUrl":"./leibniz"}' http://localhost:3838/ifc2tileset-agent/api
curl -X POST --header "Content-Type: application/json" -d '{"assetUrl":"./hugoball"}' http://localhost:3838/ifc2tileset-agent/api

## CEA
curl -X POST --header "Content-Type: application/json" -d '{"iris":["http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"]}' http://localhost:3838/cea-agent/run

