/*
*
* THIS SCRIPT IS IN DEVELOPMENT, DO NOT USE.
*
*/

/**
 * Given a selected map feature (point only), this function will...
 * 
 * @param {MapBox Map} map current map instance
 * @param {DataRegistry} registry current data registry
 * @param {JSONObject} feature selected feature
 */
function showConnectionsForFeature(map, registry, feature) {
    // Get the location of the feature
    let featureLocation = feature["geometry"]["coordinates"];
    console.log(featureLocation);

    // Find all line layers (assuming all line layers are connections);
    let connectionLayers = findConnectionLayers(map);

    for(var i = 0; i < connectionLayers.length; i++) {
        let layer = connectionLayers[i];
        if(layer["id"].endsWith("_clickable")) continue;
        let layerSource = registry.cachedGeoJSON[layer["source"]]

        // This will contain the IDs of connections within this layer we will allow
        // to be shown on the map (i.e. ones that connect to the feature);
        let allowedIDs = [];

        for(var j = 0; j < layerSource["features"].length; j++) {
            let connection = layerSource["features"][j];
            let coords = connection["geometry"]["coordinates"];
            console.log(coords);
            
            if(coords.includes(featureLocation)) {
                allowedIDs.push(connection["id"]);
                console.log("Match");
            }
        }


        console.log(layer.id + ": " + allowedIDs);
        // map.setFilter(layer.id,
        //     [
        //         "any", 
        //         ["in", "fromID"].concat(permittedIDs), 
        //         ["in", "toID"].concat(permittedIDs)
        //     ]
        // );
    }
}

/**
 * Finds and returns an array of all "line" type layers that have
 * been added to the map as part of this framework.
 * 
 * @param {MapBox Map} map current map instance 
 * @returns array of found layers
 */
function findConnectionLayers(map) {
    let allLayers = map.getStyle().layers;
    let lineLayers = [];

    for(var i = 0; i < allLayers.length; i++) {
        let layer = allLayers[i];
        if(layer["type"] === "line" && layer["metadata"] && layer["metadata"]["provider"] === "cmcl") {
            lineLayers.push(layer);
        }
    }
    return lineLayers;
}

/**
 * This function finds and adds a filter to all "line" type layers
 * so that all lines are filtered out (i.e. hidden).
 * 
 * @param {MapBox Map} map current map instance
 */
function hideAllConnections(map) {
    // Find all line layers (assuming all line layers are connections);
    let connectionLayers = findConnectionLayers(map);

    for(var i = 0; i < connectionLayers.length; i++) {
        let layer = connectionLayers[i];
        map.setFilter(layer.id, ["boolean", false]);
    }
    console.log("--- CONNECTIONS HAVE BEEN HIDDEN? ---");
}

/**
 * This function obfuscates the locations on the map by removing a significant amount of layers
 * and restricting the choice of terrain.
 * 
 * Jethro has asked that this be added so that it can be used when generating video footage
 * of the visualisation for marketing (potentially with dummy data).
 * 
 * @param {MapBox Map} map current map instance
 * @param {Boolean} obfuscate obfuscation state (true, false). 
 */
function obfuscateLocation(map, obfuscate) {
    // Allowed MapBox layers
    let allowed = ["land", "landcover", "landuse", "hillshade", "waterway", "admin-1-boundary-bg", "admin-0-boundary-bg"];

    // Set layer visibility state
    let layers = map.getStyle().layers;
    for(var i = 0; i < layers.length; i++) {
        if(!allowed.includes(layers[i].id)) {
            map.setLayoutProperty(layers[i].id,	"visibility", (obfuscate) ? "none" : "visible");
        }
    }

    // Show/hide the controls for certain terrain types
    let terrainContainer = document.getElementById("terrainContainer");
    let targetElements = [];

    targetElements = targetElements.concat(
        Array.prototype.slice.call(terrainContainer.querySelectorAll("*[id='blueprint'], *[for='blueprint']"))
    );
    targetElements = targetElements.concat(
        Array.prototype.slice.call(terrainContainer.querySelectorAll("*[id='satellite'], *[for='satellite']"))
    );
    targetElements = targetElements.concat(
        Array.prototype.slice.call(terrainContainer.querySelectorAll("*[id='satellite-streets'], *[for='satellite-streets']"))
    );

    // Udpate visibility for controls
    targetElements.forEach(targetElement => {
        targetElement.style.display = (obfuscate) ? "none" : "block";
    });
}