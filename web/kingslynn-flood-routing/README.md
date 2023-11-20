# Kings Lynn Flood Routing Digital Twin Visualisation Framework (TWA-VF)
This visualization serves as a proof of concept, leveraging knowledge graph technology for: 
1) Flood routing for vehicles with various water wading depth capability. 
2) Isochrone mapping from points of interest, highlighting unreachable area and population.
3) Travelling Salesman Problem for fastest route to restore flooded power stations. 
4) Road network sensitivity analysis - before and after flood. 

The instantiated data is visualised using the TWA Visualisation Framework [TWA-VF](../twa-vis-framework) version `4.1.2`. 

<img src="TSP.png" alt="Mapbox visualisation" width="100%"/>

## Uploading the data
Upload data using stack-data-uploader in [here](./stack-data-uploader-inputs/).

## Deployment
### Spinning up stack-manager
Copy all relevant config and data files to their relevant directories with in the Stacks folder, spin up the stack-manager with `kingslynn` as the `<STACK-NAME>` by running `./stack.sh start kingslynn`. Ensure that 'visualisation' is included as a service by using the `kingslynn` manager config file. 

### Running agent
This visualisation require running 3 agents:
1) IsochroneAgent
2) TravellingSalesmanAgent
3) NetworkAnalysisAgent

Send curl requests following instructions of each agents using .sh (Will furnish with more details here)

## Accessing the visualisation
Visualization can be seen at [http://localhost:3838/visualisation](http://localhost:3838/visualisation)

<!-- Links -->
[FeatureInfoAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent
[index.html]: webspace/index.html
[data.json]: /DTVF/data.json