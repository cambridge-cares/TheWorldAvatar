#!/bin/bash
(cd AermodAgent && ./stack.sh build)
(cd DispersionInteractor && ./stack.sh build)
(cd EmissionsAgent && ./stack.sh build)
(cd JurongIslandInputAgent && ./stack.sh build)
(cd PythonService && ./stack.sh build)
(cd ShipInputAgent && ./stack.sh build)
(cd VirtualSensorAgent && ./stack.sh build)