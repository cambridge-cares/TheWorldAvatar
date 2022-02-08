/*
*
* THIS SCRIPT IS IN DEVELOPMENT, DO NOT USE.
*
*/

var allowedConnections = {};
var allowedSites = {};

/**
 * Given a selected map feature (point only), this function will...
 * 
 * @param {MapBox Map} map current map instance
 * @param {DataRegistry} registry current data registry
 * @param {JSONObject} feature selected feature
 */
function showConnectionsForFeature(map, registry, feature, depth = 1) {


    // TEST 
    if(DT.currentFeature != null) {
        map.setFeatureState(DT.currentFeature, { active: false });
    }
    map.setFeatureState(feature, { active: true });
    // TEST

    // Hide previously shown connections
    hideAllConnections(map);
    allowedConnections = {};
    allowedSites = {};

    // Recurse to find connections to show
    recurseConnection(registry, feature, 0, depth);

    // Update filters to show those connections
    updateFilters(map);

    let focusView = new FocusView(map);
    focusView.createOverlay();

    focusView.duplicateLayers(allowedConnections);
    focusView.duplicateLayers(allowedSites);
}

function recurseConnection(registry, currentFeature, currentDepth, depthLimit) {
    // Gather details of connections from/to the current feature
    let connectionDetails = getConnectionDetails(currentFeature);
    currentDepth = currentDepth + 1;

    // Add discovered connections to global pool
    for (const [connLayer, connIds] of Object.entries(connectionDetails)) {
        if(!allowedConnections[connLayer]) allowedConnections[connLayer] = [];

        for(var i = 0; i < connIds.length; i++) {
            if(!allowedConnections[connLayer].includes(connIds[i])) allowedConnections[connLayer].push(connIds[i]);

            // Get the GeoJSON feature for the connection
            let connectionFeature = getConnection(registry, connLayer, parseInt(connIds[i]));

            // Gather details of sites attached to this connection
            let siteDetails = getSiteDetails(connectionFeature);

            // Add discovered sites to global pool
            for (const [siteLayer, siteIds] of Object.entries(siteDetails)) {
                if(!allowedSites[siteLayer]) allowedSites[siteLayer] = [];

                for(var j = 0; j < siteIds.length; j++) {
                    if(!allowedSites[siteLayer].includes(siteIds[j])) allowedSites[siteLayer].push(siteIds[j]);

                    let siteFeature = getSite(registry, siteLayer, siteIds[j]);

                    // Recurse if not at limit
                    if(currentDepth < depthLimit) {
                        recurseConnection(registry, siteFeature, currentDepth, depthLimit);
                    }
                }
            }
        }
    }
}

/**
 * Update filters of MapBox layers to only show the allowed Connections.
 */
function updateFilters(map) {
    // Connections
    for (const [connLayer, connIds] of Object.entries(allowedConnections)) {
        let literal = ["literal", connIds];
        let filter = ["in", ["id"], literal];
        map.setFilter(connLayer, filter);
    }
}

/**
 * 
 * @param {*} feature 
 * @returns 
 */
function getConnectionDetails(feature) {
    let allowedConnections = {};

    // Get connections as listed in feature properties.
    // Note: a MapBox bug here means that what's set as a JSON array in the original GeoJSON
    // gets returned as a single string, hence the need to re-parse it as JSON.
    // https://github.com/mapbox/mapbox-gl-js/issues/2434
    let connections = feature.properties["connections"];
    if(connections == null) return allowedConnections;

    if(typeof connections === "string") {
        connections = JSON.parse(connections);
    }

    for(var i = 0; i < connections.length; i++) {
        let connection = connections[i];
        let layerName = connection.split("/")[0];
        let id = connection.split("/")[1];

        if(!allowedConnections[layerName]) {
            allowedConnections[layerName] = [];
        }
        if(!allowedConnections[layerName].includes(parseInt(id))) {
            allowedConnections[layerName].push(parseInt(id));
        }
    }
    return allowedConnections;
}

/**
 * 
 * @param {*} feature 
 * @returns 
 */
 function getSiteDetails(connection) {
    let allowedSites = {};

    // Source of connection
    let sourceSite = connection["properties"]["from"];
    if(sourceSite != null) {
        let sourceSource = sourceSite.split("/")[0];
        let sourceID = parseInt(sourceSite.split("/")[1]);
        if(!allowedSites[sourceSource]) allowedSites[sourceSource] = [];
        if(!allowedSites[sourceSource].includes(sourceID)) allowedSites[sourceSource].push(parseInt(sourceID));
    }

    // Target of connection
    let targetSite = connection["properties"]["to"];
    if(targetSite != null) {
        let targetSource = targetSite.split("/")[0];
        let targetID = parseInt(targetSite.split("/")[1]);
        if(!allowedSites[targetSource]) allowedSites[targetSource] = [];
        if(!allowedSites[targetSource].includes(targetID)) allowedSites[targetSource].push(parseInt(targetID));
    }
    return allowedSites;
}

/**
 * Attempts to find the GeoJSON feature representing the input connection
 * ID within the input connection target.
 * 
 * @param {DataRegistry} registry registry containing cached GeoJSON
 * @param {String} connectionSource name of connection source
 * @param {Integer} connectionID id of connection feature
 * 
 * @returns matching connection feature, or null
 */
function getConnection(registry, connectionSource, connectionID) {
    let sourceData = registry.cachedGeoJSON[connectionSource];
    return sourceData["features"].find(feature => {
        return feature["id"] === connectionID;
    });
}

/**
 * 
 * @param {*} registry 
 * @param {*} connectionFeature 
 * @returns 
 */
function getSite(registry, siteSource, siteID) {
    let sourceData = registry.cachedGeoJSON[siteSource];
    return sourceData["features"].find(feature => {
        return feature["id"] === siteID;
    });
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

