/**
 * Get the geographical center of the input feature.
 * 
 * @param feature
 * 
 * @return coords [x, y]
 */
function getCenter(feature: Object) {
    // Convert from MapBox feature to GeoJSON
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
 * 
 * @returns 
 */
function getDefaultImagery() {
    let imagerySettings = Manager.SETTINGS.getSetting("imagery");
    let defaultSetting = imagerySettings["default"];
    return imagerySettings[defaultSetting];
}
