# Agent purpose
The CEAVisualisationAgent is intended to create the necessary PostgreSQL table and GeoServer layer, as well as relevant set up, for the visualisation of CEA agent results within TWA-VF.
In the resulting TWA-VF visualisation, it is intended that the buildings are visualised with a color scale representing their energy demand and solar potential results from the CEA agent.

# Agent logic
The agent is designed to receive a POST request with the CEA agent outputs as annual values for energy demands and solar potentials and the solar suitable areas, together with the building IRI. 
After receiving the request, the agent will create (if the table does not exist before) or update the table that with the building IRI and its corresponding annual values and solar suitable areas.
The agent will also create a GeoServer layer based on the aforementioned PostgreSQL table.
A `data.json` has been included in `./stack-manager-input/data/webspace` which uses the GeoServer layer that the agent created to visualise CEA results in TWA-VF.

# What has been implemented
- The main classes to receive the request, and to parse the data into PostgreSQL table and GeoServer layer.
- Stack manager configurations
- `data.json` for TWA-VF

# TODO
- Automatic creation of legends for TWA-VF
- Integration with the CEA agent
- Unit tests
- Testing that everything works together

