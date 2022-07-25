/**
 * Add new HTML <div> element
 * @param cssclass      class styled in css
 */
 export function addDiv(cssclass) {
    let divElement = document.createElement("div");
    divElement.className = cssclass;
    return divElement; 
}

/**
 * Add a close button as HTML element
 * @param cssid      id styled in css
 */
 export function addCloseButton(cssid) {
    let divElement = document.createElement("INPUT");
    divElement.id = cssid;
    divElement.setAttribute("type", "button");
    divElement.value = "X";
    divElement.onclick = closeMetadata;
    return divElement; 
}

// Close the metadata infobox on click
export function closeMetadata() {
    // Set transition time and effects for metadata infobox
    window.setTimeout(function () {
        // Create a custom infobox element to present meta data when clicked on an asset
        let metadataBox = document.getElementsByClassName("biminfobox")[0];
        metadataBox.style.transform = 'translate(100%, 0)';
        metadataBox.style.opacity = 0;
        metadataBox.style.visibility = 'hidden';
        metadataBox.style.transition = "visibility 0s 0.2s, opacity 0.2s ease-in, transform 0.2s ease-in";

        // Remove close button at same time
        let closeElement = document.getElementById("close");
        closeElement.style.display = 'none';
    }, 0.2);
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