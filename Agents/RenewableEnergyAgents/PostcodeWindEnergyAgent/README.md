# An agent to represent and query solar data reported by the Newcastle Urban Observatory.
### Authors
* [Toby Latcham](tjl47@cam.ac.uk)
* [Sophie Hall](sh2000@cam.ac.uk)
* [Feroz Farazi](msff2@cam.ac.uk)

The agent processes wind energy data consumed across the UK to represent it in the World Avatar Knowledge Graph (KG) to make it accessible and queryable. The data includes annual electricity consumption statistics and daily electricity consumption patterns for each month at a postcode.

Python modules developed for data instantiation and query are briefly described below.

* wind_energy_instantiation.py: This script instantiates electric energy consumption time series data attached to geospatial reference within the KG using the [TimeSeriesClient].

* wind_energy_query.py: This script queries the KG and visualises the electric energy consmuption data using the Digital Twin Visualisation Framework ([DTVF]).

To interact with the [TheWorldAvatar] (TWA) code base (mainly) written in Java, the [py4jps] Python wrapper is used.
