# Kings Lynn Flood Routing Digital Twin Visualisation Framework (DTVF)

This visualization serves as a proof of concept for routing under flood, isochrone from points of interest, unreachable area.

The instantiated data is visualised using the Digital Twin Visualisation Framework ([DTVF]) version `3.3.4`. The configuration file structure (i.e. `data.json`) is based on the [example Mapbox visualisation].

<img src="floodrouter.JPG" alt="Mapbox visualisation" width="100%"/>

## Creating the Visualisation
This document marks down the steps taken to create flood router, isochrone under flooding and transport network criticality analysis. 

After copying all relevant config and data files to their relevant directories with in the Stacks folder, spin up the stack-manager with `kingslynn` as the `<STACK-NAME>` by running `./stack.sh start kingslynn`. Ensure that 'visualisation' is included as a service by using the `kingslynn` manager config file. Understanding of the stack-manager and stack-data-uploader are assumed here.

NB this viz employs a somewhat hacky method of table post processing which requires having an `empty` data subdirectory. This can be seen in the inputs tree.
### DTVF Prerequisite
A valid Mapbox API token must be provided in your [index.html] file.



Visualization can be seen at [http://localhost:3838/visualisation](http://localhost:3838/visualisation)


<!-- Links -->
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[example Mapbox visualisation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/example-mapbox-vis
[FeatureInfoAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent

<!-- repositories -->
[FeatureInfoAgent subdirectory]: /DTVF/FeatureInfoAgent
[FeatureInfoAgent queries]: FeatureInfoAgent/queries
[DTVF subdirectory]: /DTVF
[icons]: /DTVF/data/icons
[index.html]: webspace/index.html
[data.json]: /DTVF/data.json