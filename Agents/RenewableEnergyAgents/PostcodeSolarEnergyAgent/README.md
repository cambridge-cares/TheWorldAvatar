# An agent to represent and query solar data reported by the Newcastle Urban Observatory.
### Authors
* [Toby Latcham](tjl47@cam.ac.uk)
* [Sophie Hall](sh2000@cam.ac.uk)
* [Feroz Farazi](msff2@cam.ac.uk)

The agent processes solar energy data consumed across the UK to represent it in the World Avatar Knowledge Graph (KG) to make it accessible and queryable. The data includes annual electricity consumption statistics and daily electricity consumption patterns for each month at a postcode.

Python modules developed for data instantiation and query are briefly described below.

* solar_energy_instantiation.py: This script instantiates electric energy consumption time series data attached to geospatial reference within the KG using the [TimeSeriesClient].

* solar_energy_query.py: This script queries the KG and visualises the electric energy consmuption data using the Digital Twin Visualisation Framework ([DTVF]).

To interact with the [TheWorldAvatar] (TWA) code base (mainly) written in Java, the [py4jps] Python wrapper is used.

Note I: Run MetOfficeSolarSensorAgent.solar_sensor_instantiation.py before running PostcodeSolarEnergyAgent.solar_energy_instantiation.py.

Note II: Running any python file available here will require the change of directory to the <root> folder named RenewableEnergyAgents and the use of the agent folder called PostcodeSolarEnergyAgent. For example, to run solar_energy_instantiation.py, use the following command:
         python -m PostcodeSolarEnergyAgent.solar_energy_instantiation

         Follow the instructions provided in README.md available under MetOfficeSolarSensorAgent to know how to run MetOfficeSolarSensorAgent.solar_energy_instantiation.py.