#!/bin/bash
# starts up all required components of virtual sensor
(cd ../Agents/WeatherAgent/ && ./stack.sh start ship-stack)
(cd ShipInputAgent/ && ./stack.sh start ship-stack)
(cd EmissionsAgent/ && ./stack.sh start ship-stack)
(cd PythonService && ./stack.sh start ship-stack)
(cd DispersionInitialiser/ && ./stack.sh start ship-stack)
(cd AermodAgent && ./stack.sh start ship-stack)