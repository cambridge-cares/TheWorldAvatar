## AERMOD agent
This agent extends the DerivationAgent class and requires the following inputs
- Weather station (type: https://www.theworldavatar.com/kg/ontoems/ReportingStation)
- nx (type: http://www.theworldavatar.com/kg/dispersion/nx)
- ny (type: http://www.theworldavatar.com/kg/dispersion/ny)
- Scope (type: http://www.theworldavatar.com/kg/dispersion/Scope)
- Simulation time (type: http://www.theworldavatar.com/kg/dispersion/SimulationTime)

Key steps taken by this agent:
1) Query values from the given IRIs
2) Get ships located within the scope at the given simulation time
3) Update derivations of the given ships (emission values)
4) Create input file for AERMET (weather preprocessor), execute AERMET
5) Create other input files for AERMOD and execute AERMOD
6) Upload AERMOD output file to the FileServer
7) Call PythonService to postprocess AERMOD output file in order to obtain the GeoJSON contour for visualisation
8) Upload GeoJSON to PostGIS as a vector and create a GeoServer layer