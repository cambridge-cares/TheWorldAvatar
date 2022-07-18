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