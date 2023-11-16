# Fumehood agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding fumehood occupancy status and toggle the fumehood.
The agent will retrieve the sensor readings from the Thingsboard (TB) server, labelled `avgDist`, and determine the fumehood occupancy(`occupiedState`).

This agent is derived from the [ThingsboardInputAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-1505-proximity-sensor-for-lab_FHAgent/Agents/ThingsBoardAgent).
This agent has the basic same functionality to the ThingsBoardInputAgent. 
The difference is lies in the agents ability to convert sensor reading to occupancy status before instantiating the timeseries and the fumehood toggle function.
