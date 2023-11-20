# Kings Lynn Flood Routing Digital Twin Visualisation Framework (TWA-VF)

This visualization serves as a proof of concept, leveraging knowledge graph technology for: 
1) Flood routing for vehicles with various water wading depth capability. 
2) Isochrone mapping from points of interest, highlighting unreachable area and population.
3) Travelling Salesman Problem for fastest route to restore flooded power stations. 
4) Road network sensitivity analysis - before and after flood. 

The instantiated data is visualised using the TWA Visualisation Framework [TWA-VF](../twa-vis-framework) version `4.1.2`. 

<img src="floodrouter.JPG" alt="Mapbox visualisation" width="100%"/>

## Creating the Visualisation
After copying all relevant config and data files to their relevant directories with in the Stacks folder, spin up the stack-manager with `kingslynn` as the `<STACK-NAME>` by running `./stack.sh start kingslynn`. Ensure that 'visualisation' is included as a service by using the `kingslynn` manager config file. Understanding of the stack-manager and stack-data-uploader are assumed here.


Visualization can be seen at [http://localhost:3838/visualisation](http://localhost:3838/visualisation)


<!-- Links -->
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[example Mapbox visualisation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/example-mapbox-vis
[FeatureInfoAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent

<!-- repositories -->
[FeatureInfoAgent subdirectory]: /DTVF/FeatureInfoAgent
[FeatureInfoAgent queries]: FeatureInfoAgent/queries
[icons]: /DTVF/data/icons
[index.html]: webspace/index.html
[data.json]: /DTVF/data.json