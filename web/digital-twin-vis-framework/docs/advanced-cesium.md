# Advanced CesiumJS features

## Terrain elevation

Unfortunately, the terrain elevation provided by Cesium ion, is not permitted for commercial or funded-education use. As such, we need to identify third-party terrain data.

Terrain elevation data can also be provided via use of a `terrain` variable in the `settings.json` file. The value this variable is passed directly to a new [CesiumTerrainProvider](https://cesium.com/learn/cesiumjs/ref-doc/CesiumTerrainProvider.html) instance, as such users need to ensure that their settings conform with the CesiumJS API.


An example specification of terrain elevation is shown below. Note that in this case, the data is pulled from quantized mesh tiles provided by [MapTiler](https://cloud.maptiler.com/), a service that is **not permitted for commercial use** without a paid-for licence.

```json
"terrain": {
    "url": "https://api.maptiler.com/tiles/terrain-quantized-mesh-v2/?key=API_KEY",
    "requestVertexNormals": true
}
```

<br/>

## Clipping planes

For 3D tileset sources, clipping planes can also be added that allow the user to effectively slice the model, revealing its interior at a specific height; CesiumJS has an online example of this [here](https://sandcastle.cesium.com/?src=3D%20Tiles%20Clipping%20Planes.html); a demonstration of this feature has also been added to this example visualisation.

To enable clipping planes, within the specification of a 3D tileset layer in the `data.json` file, add a `clipping` object to specify the height range (above sea level, in metres), and an optional array of labelled increments; this can be done for as many tilesets as the developer requires. An example of the specification format is shown below:

```json
  "clipping": {
        "min": 0,
        "max": 10,
        "start": 10,
        "labels": {
            "0": "Ground level",
            "2.9": "Bottom floor",
            "5.78": "Top floor",
            "9.14": "Roof"
        }
    }    
```

There are a few caveats to mention however:
- This feature is only supported on 3D tileset sources.
- Cesium only seems to support clipping planes on the _entire_ tileset.
- The size of the clipping plane is based on the bounds of the tileset itself.
- The feature can currently only be active on one tileset at any given time.
- If adding planes to an existing visualisation, ensure the version of DTVF JS and CSS files used in the `index.html` file are _at least_ 3.4.0.
- If adding planes to an existing visualisation, ensure the version of Cesium JS and CSS files used in the `index.html` file are _at least_ 1.105.

<br/>
<p align="center">
    <img src="./img/sample-nyc-3.JPG" alt="Example of a clipping plane on a 3D tileset" width="75%"/>
</p>

<br/>

## Data driven styling