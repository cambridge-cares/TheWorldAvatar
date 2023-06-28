#!/bin/bash
(cd AermodAgent && ./stack.sh build)
(cd DispersionInteractor && ./stack.sh build)
(cd EmissionsAgent && ./stack.sh build)