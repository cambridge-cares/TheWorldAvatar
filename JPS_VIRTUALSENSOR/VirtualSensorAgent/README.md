# Overview
Updates time series of pollutant concentrations in the knowledge graph for a single virtual sensor.

# Requirements
At least one AERMOD simulation should have been run following the instructions in the JPS_VIRTUALSENSOR/README.md document.


# API
URL: http://localhost:3838/dispersion-interactor/UpdateVirtualSensors
Send a POST request to this URL. A single parameter called derivation whose value is the derivation IRI returned by the call to InitialiseSimulation should be specified as the single parameter.

This class queries all virtual sensor derivations derived from dispersion outputs that belong to the derivation IRI. It then makes them out of date by updating the dispersion output timestamps. Finally, all virtual sensor derivations are updated. 