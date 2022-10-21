/**
 * Get the geographical center of the input feature.
 * 
 * @param feature
 * 
 * @return coords [x, y]
 */
function getCenter(feature: Object) {
    // Convert from Mapbox feature to GeoJSON
    let geojsonFeat = turf.feature(feature["geometry"]);

    // Despite documentation that center() accepts a single Feature
    // or a FeatureCollection, using the former causes Typescript errors
    let featCollec = turf.featureCollection([geojsonFeat]);
    let center = turf.center(featCollec);

    return center["geometry"]["coordinates"];
}

/**
 * Load HTML content from an external file.
 */
async function loadHTML(htmlFile: string) {
    return await fetch(htmlFile).then(response => {
        return response.text();
    });
}

/**
 * Determines and returns the URL for the default WMS imagery.
 * 
 * @returns 
 */
function getDefaultImagery() {
    let imagerySettings = Manager.SETTINGS.getSetting("imagery");
    let defaultSetting = imagerySettings["default"];
    return imagerySettings[defaultSetting];
}

/**
 * Update the given URL replacing "localhost" or "127.0.0.1" with the base
 * URL that the visualisation is currently being accessed from.
 * 
 * @param originalURL 
 * @returns 
 */
function updateURL(originalURL: string): string {
    if(!originalURL.includes("localhost") && !originalURL.includes("127.0.0.1")) {
        return originalURL;
    }

    // Get the URL that the visualisation is currently accessed from
    let winURL = window.location;
	let baseURL = winURL.protocol + "//" + winURL.host + winURL.pathname;

    originalURL = originalURL.replace("http://", "");
    originalURL = originalURL.replace("https://", "");
    originalURL = originalURL.replace("localhost", "");
    originalURL = originalURL.replace("127.0.0.1", "");

    if(originalURL.startsWith("/")) {
        return baseURL + originalURL;
    } else {
        return baseURL + "/" + originalURL;
    }
}

/**
 * Returns a new dictionary without any properties
 * that had null values.
 * 
 * @param dictionary 
 * @returns 
 */
function filterNulls(dictionary: Object) {
    let result = {};

    for(var key in dictionary) {
        let value = dictionary[key];
        if(value !== null) result[key] = value;
    }
    return result;
}

/**
 * Using the custom search terms, this returns the correct name
 * for a feature given it's properties object.
 * 
 * @param properties feature's property dictionary
 * @returns name (or null) 
 */
function getName(properties: Object): string {
    let fieldSettings = Manager.SETTINGS.getSetting("fields");
    if(fieldSettings == null) return properties["name"];

    let nameField = fieldSettings["name"];
    if(nameField == null) return properties["name"];
    
    return properties[nameField];
}

/**
 * Using the custom search terms, this returns the correct description
 * for a feature given it's properties object.
 * 
 * @param properties feature's property dictionary
 * @returns description (or null) 
 */
function getDescription(properties: Object): string {
    let fieldSettings = Manager.SETTINGS.getSetting("fields");
    if(fieldSettings == null) return properties["description"];

    let nameField = fieldSettings["description"];
    if(nameField == null) return properties["description"];
    
    return properties[nameField];
}