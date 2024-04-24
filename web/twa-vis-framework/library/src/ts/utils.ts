
/**
 * Opens the user help document associated with this version of the TWA-VF.
 */
function openHelpURL() {
    window.open("./lib/help/", "_blank");
}

/**
 * Returns the current version of the TWA-V.
 */
async function getVisVersion() {
    return await fetch("./lib/VERSION").then(response => {
        return response.text();
    });
}

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
    if(imagerySettings == null) {

        if(Manager.PROVIDER === MapProvider.MAPBOX) {
            MapboxUtils.generateDefaultImagery();
        } else if(Manager.PROVIDER === MapProvider.CESIUM) {
            CesiumUtils.generateDefaultImagery();
        }

        imagerySettings = Manager.SETTINGS.getSetting("imagery");
    }

    let defaultSetting = imagerySettings["default"];

    let url = imagerySettings[defaultSetting];
    if(url.endsWith("_token=")) url += MapHandler.MAP_API;
    return url;
}

/**
 * Update the given URL replacing "localhost" or "127.0.0.1" with the base
 * URL that the visualisation is currently being accessed from.
 * 
 * @param originalURL 
 * @returns 
 */
function updateURL(originalURL: string): string {
    return originalURL;
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
    if(properties === null || properties === undefined) return null;

    let fieldSettings = Manager.SETTINGS.getSetting("fields");
    if(fieldSettings === null || fieldSettings === undefined) {
        return properties["name"];
    }

    let nameField = fieldSettings["name"];
    if(nameField === null || nameField === undefined) {
        return properties["name"];
    }
    
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

/**
 * Is attribution content has been set, show a popup box in
 * the lower left to display it.
 */
function showAttributions() {
    let container = document.getElementById("attributionContainer");
    if(container == null) {
        container = document.createElement("div");
        container.id = "attributionContainer";
        container.className = "collapsed";

        container.innerHTML = "<div id='attributionTitle'><span>Attributions</span><i class='fas fa-chevron-up'></i></div>";
        document.body.appendChild(container);
    }

    let content = Manager.SETTINGS.getSetting("attribution");
    container.style.display = (content == null) ? "none" : "block";

    if(content != null) {
        container.innerHTML += content;

        let title = container.querySelector("#attributionTitle");
        title.addEventListener("click", function(event) {
            let classList = container.classList;
            
            if(classList.contains("collapsed")) {
                // Expand it
                classList.remove("collapsed");
                classList.add("expanded");
                title.innerHTML = "<span>Attributions</span><i class='fas fa-chevron-down'></i>";
            } else {
                // Collapse it
                classList.remove("expanded");
                classList.add("collapsed");
                title.innerHTML = "<span>Attributions</span><i class='fas fa-chevron-up'></i>";
            }
        });
    }
}

/**
 * Shows the relevant help page.
 */
function showHelpPage() {
    switch(Manager.PROVIDER) {
        case MapProvider.CESIUM:
            // TODO
        break;

        case MapProvider.MAPBOX:
            // TODO
        break;
    }
}


/**
 * Builds and adds a button to open the dashboard in a new tab.
 * 
 * Note: The 'dashboard' parameter must have been set within the
 * visualisation's settings.json file for this to appear.
 */
function buildDashboardButton() {
    if(Manager.SETTINGS.getSetting("dashboard") != null) {

        let html = `
            <div id="dashButtonContainer" class="controlBlock expanded" onclick="openDashboard()" style="cursor: pointer;">
                <p>Open dashboard</p>
            </div>
        `;

        // Parse into a HTML element
        var parser = new DOMParser();
        var doc = parser.parseFromString(html, 'text/html');

        let container = document.getElementById("controlContainer");
        let searchButton = document.getElementById("helpandsearch");
        container.insertBefore(doc.body.firstChild, searchButton);
    }
}

/**
 * Open the dashboard in a new link.
 */
function openDashboard() {
    let url = Manager.SETTINGS.getSetting("dashboard");
    if(url != null) {
        window.open(url, '_blank').focus();
    }
}
