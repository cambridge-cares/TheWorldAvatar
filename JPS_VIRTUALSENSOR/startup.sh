#!/bin/bash
# starts up all required components of virtual sensor
(cd ../Deploy/stacks/dynamic/stack-manager/ && ./stack.sh start ship-stack)
(cd ../Agents/WeatherAgent/ && ./stack.sh start ship-stack)
(cd ShipInputAgent/ && ./stack.sh start ship-stack)
(cd EmissionsAgent/ && ./stack.sh start ship-stack)
(cd PythonService && ./stack.sh start ship-stack)
(cd DispersionInitialiser/ && ./stack.sh start ship-stack)
(cd Episode && ./stack.sh start ship-stack)