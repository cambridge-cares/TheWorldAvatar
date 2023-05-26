# Advanced Mapbox features

This pages details some of the more advanced Mapbox-specific features that can be enabled and configured through the DTVF.

<br/>

## Clustering

When using a GeoJSON source of point location, users can enable the cluster function to group nearby points into a single group. An example of clustering can be seen in [this Mapbox example](https://docs.mapbox.com/mapbox-gl-js/example/cluster/).

To enable this, simply add the relevant cluster parameters (matching the Mapbox API) to the desired `source` object within the visualisation's `data.json` file.

```json
 {
    "id": "my-source",
    "type": "geojson",
    "data": "data/my-data-file.geojson",
    "cluster": true,
    "clusterMaxZoom": 15,
    "clusterRadius": 30
}
```

<br/>
<p align="center">
 <img src="./img/cluster-example.JPG" alt="Example of point clustering." width="50%"/>
</p>
<p align="center">
 <em>Example of point clustering.</em><br/><br/><br/>
</p>

<br/>

## Filters

In the Mapbox style API, a filter is a property at the layer level that determines which features should be rendered in a style layer. Filters are written as expressions, which give you fine-grained control over which features to include: the style layer only displays the features that match the filter condition that you define.

In the DTVF's `data.json` file, the `filter` parameter can be added to any Mapbox layer. You can read more about Mapbox's expression syntax on [their help page](https://docs.mapbox.com/mapbox-gl-js/style-spec/expressions).

```json
{
    "id": "my-layer",
    "name": "My Layer",
    "source": "my-source",
    "type": "symbol",
    "filter": ["in", "Hall", ["get", "name"]],
    ...
},
```

<br/>
<p align="center">
 <img src="./img/filtering-example.png" alt="Example of location filtering." width="80%"/>
</p>
<p align="center">
 <em>Example of location filtering.</em><br/><br/><br/>
</p>


<br/>

## Searching

When using the Mapbox implementation, a rudimentary ability to search for individual locations has been added. Accessed via the CTRL+F keyboard shortcut, this feature allows users to hide all locations that do not match some input search term. Properties from the geospatial data (i.e. not any dynamically loaded metadata) can be used as targets for the search; the example Mapbox visualisation contains an example of this (primarily focussed on the Cambridge Colleges data set).

It is worth noting that, at the time of writing, the filter will apply to all locations across all layers currently available on the map. Additionally, any clustering of locations (normally enabled by configuring the source object within the data configuration file) will be temporarily disabled whilst the search controls are active; this is to avoid the situation in which matching locations are hidden because they were clustered (and clustered locations will almost always fail the filter match as they contain very little information on what individual features they contain).

To enable this functionality within your visualisation, a special `search` node needs to be added to your `settings.json` file. This node should be a JSON array of objects, each object defining: `description`, the user facing name of the parameter that can be filtered on/searched for; `property`, the name of the actual property within the geospatial data; and `type`, which must be `string|number|boolean` and represents the type of property.

An example snippet of the `settings.json` file defining search parameters is shown below.

```json
"search": [
    {
        "description": "Name",
        "property": "name",
        "type": "string"
    },
    {
        "description": "Year Founded",
        "property": "founded",
        "type": "number"
    },
    {
        "description": "Undergraduates Admitted?",
        "property": "undergraduates",
        "type": "boolean"
    }  
]
```

<br/>
<p align="center">
 <img src="./img/search-example.JPG" alt="Example of feature searching." width="65%"/>
</p>
<p align="center">
 <em>Example of feature searching</em><br/><br/><br/>
</p>