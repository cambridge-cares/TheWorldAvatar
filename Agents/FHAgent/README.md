# FHAgent : Fumehood Occupancy Agent

An agent designed to handle distance data from proximity sensor and determine fumehood occupancy.
Takes in a distance timeseries instance from the knowledge graph and determine the occupancy based on a tally system.
The distance data is provided by an ESP32 microcontroller connected to a Thingsboard server. 
A [Thingsboard agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ThingsBoardAgent) will instantiate the distance timeseries in the KG.




