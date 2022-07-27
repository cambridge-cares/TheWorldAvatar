var Cesium = require("cesium/Cesium");
/**
 * Add the tileset to the viewer and set its model matrix for geolocation
 * @param cesiumviewer    Cesium viewer object
 * @param cesiumtileset   name of tileset.json file
 * @param x               latitude or x coordinate of a geolocation
 * @param y               longitude or y coordinate of a geolocation
 * @param z               elevation or z coordinate of a geolocation
 * @return                Cesium viewer primitive object
 */
export function addTileset(cesiumviewer, cesiumtileset, x, y, z = 0) {
    let coordinates = Cesium.Cartesian3.fromDegrees(x, y, z);

    return cesiumviewer.scene.primitives.add(
        new Cesium.Cesium3DTileset({
            url: cesiumtileset,
            modelMatrix: Cesium.Transforms.eastNorthUpToFixedFrame(coordinates),
            debugShowBoundingVolume: false,
        })
    )
}

/**
 * Generates a formatted HTML string containing the picked feature's available metadata
 * @param metadata  the object referencing the picked feature's metadata stored in tileset
 * @return          HTML string
 */
 export function createMetadataHtml(metadata) {
    const propertyKeys = metadata.getPropertyIds();
    if (!Cesium.defined(propertyKeys)) {
        return `(No properties for ${title})<br>`;
    }
    // Formatting the information box into a table
    let html = "<div class='biminfobox-title'>"
    let htmlTable = "<table id='bim-infobox-table' class='biminfobox-content' style='border-collapse:collapse;'>";

    for (let i = 0; i < propertyKeys.length; i++) {
        const propertyKey = propertyKeys[i];
        const propertyValue = metadata.getProperty(propertyKey);
        if (propertyKey == "Asset Name") {
            html += `<h3>${propertyValue}</h3>`;
        } else {
            htmlTable += `<tr><td>${propertyKey} :</td> <td> ${propertyValue}</td></tr>`;
        }
    }
    html += "</div>" + htmlTable + "</table>";
    return html;
}

