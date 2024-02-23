# Configuration: Map Settings

The `config/map-settings.json` file, which should be present within the `uploads` directory, provides map settings that are not specific to the data sets being loaded. This includes settings such as the map's starting position, available camera positions, imagery options etc.

It is intended that this file (along with other configuration files) are provided by the deploying developer via Docker volumes created with the standard TWA Stack. As such, there may be no off-the-shelf example file committed to this repository. 

## Format

Below is an example of the contents for a valid `map-settings.json` file with additional comments explaining each entry.

The format of the file is mostly consistent when deploying mapbox or cesium maps, there will be some differences. This is detailed in the [Provider Specific Format](#provider-specific-format) section.

> [!NOTE]  
> The comments seen below are for explanation purposes only, they are not valid JSON. If wishing to use this content in production, remove the comments first.

```json
{
    "type": "mapbox", // Type of map, "mapbox" or "cesium"
    "camera": {			
        "default": "Paris",  // Name of starting camera position
        "positions": [       // Selectable positions for the camera
            {
                "name": "London",
                "center": [-0.12794, 51.50774],
                "zoom": 18,
                "bearing": 0,
                "pitch": 45
            },
            {
                "name": "Paris",
                "center": [2.29448, 48.85826],
                "zoom": 16,
                "bearing": 0,
                "pitch": 45
            },
            {
                "name": "New York",
                "center": [-73.98568, 40.74845],
                "zoom": 16,
                "bearing": 0,
                "pitch": 45
            }
        ]
    },
    "imagery": {
        "default": "Auto", // Name of default map imagery, "auto" will inherit from browser theme
        "options": [       // Mapbox base layer options, using their Style URL system
            {
                "name": "Light",
                "url": "mapbox://styles/mapbox/light-v11?optimize=true"
            },
            {
                "name": "Dark",
                "url": "mapbox://styles/mapbox/dark-v11?optimize=true"
            },
            {
                "name": "Outdoors",
                "url": "mapbox://styles/mapbox/outdoors-v12?optimize=true"
            },
            {
                "name": "Satellite",
                "url": "mapbox://styles/mapbox/satellite-streets-v12?optimize=true"
            },
            {
                "name": "3D (Day)",
                "url": "mapbox://styles/mapbox/standard",
                "time": "dawn"
            },
            {
                "name": "3D (Night)",
                "url": "mapbox://styles/mapbox/standard",
                "time": "dusk"
            }
        ]
    }
}
```

## Provider Specific Format

TBD (but I can already say that the camera position formats will be different for Cesium maps).