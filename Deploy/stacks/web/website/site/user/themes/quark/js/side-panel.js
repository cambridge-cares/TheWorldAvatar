/**
* This JS file handles the the creation, configuration, and setup of a
* common side-panel to be used in all UK Digital Twin visualisations.
*
* Note that this script needs to be run in an environment/on a page that also
* has the JQuery library, and side-panel.css stylesheet loaded.
* 
* Author: Michael Hillman (mdhillman<@>cmclinnovations.com)
*/

// List of required div names
var requiredDivs = [
	"titleContainer",
	"metadataContainer",
	"textContainer",
	"chartContainer",
	"tableContainer",
	"legendContainer",
	"dateContainer"
];

// Side panel component
var sidePanel = null;

/**
 * Initialise the side-panel components.
 * 
 * @param mapContainer - div containing MapBox map 
 */
function initialiseSidePanel(mapContainer) {
	// Update style of map
	mapContainer.style.position = "absolute";
	mapContainer.style.top = "0";
	mapContainer.style.bottom = "0";
	mapContainer.style.width = "calc(100% - 400px)";
	mapContainer.style.height = "100%";

	// Find the side panel element
	sidePanel = document.getElementById("side-panel");
	if(sidePanel == null) {
		sidePanel = document.createElement("div");
		sidePanel.setAttribute("id", "side-panel");

		// Add to the body
		document.body.appendChild(sidePanel);
	}

	// Check for each of the sub-components expected to be in the sidePanel
	requiredDivs.forEach(requiredDiv => {
		var div = document.getElementById(requiredDiv);

		if(div == null) {
			console.log("INFO: Could not find required side-panel component '" + requiredDiv + "', will create dynamically.");
			div = document.createElement("div");
			div.setAttribute("id", requiredDiv);
			
			// Hide by default
			div.style.display = "none";

			sidePanel.appendChild(div);
		}
	});
}

/**
 * Update the title within the side-panel.
 * @param titleHTML - HTML string
 */
function setSidePanelTitle(titleHTML) {
	document.getElementById("titleContainer").innerHTML = titleHTML;
	document.getElementById("titleContainer").style.display = "block";
}

/**
 * Update the text within the side-panel.
 * 
 * @param textHTML - HTML string
 */
function setSidePanelText(textHTML) {
	document.getElementById("textContainer").innerHTML = textHTML;
	document.getElementById("textContainer").style.display = "block";
}

/**
 * Appends to the text within the side-panel
 * 
 * @param textHTML - HTML string
 */
function appendSidePanelText(textHTML) {
	document.getElementById("textContainer").innerHTML += textHTML;
	document.getElementById("textContainer").style.display = "block";
}

/**
 * Sets the content of the legend panel.
 * 
 * @param legendHTML - HTML string
 */
function setSidePanelLegend(legendHTML) {
	document.getElementById("legendContainer").innerHTML = legendHTML;
	document.getElementById("legendContainer").style.display = "block";
}

/**
 * Sets the content of the meta-data panel.
 * 
 * @param metaHTML - HTML string
 */
function setSidePanelMeta(metaHTML) {
	document.getElementById("metadataContainer").innerHTML = metaHTML;
	document.getElementById("metadataContainer").style.display = "block";
}

/**
 * Sets the content of the date container panel.
 * 
 * @param dateHTML - HTML string
 */
function setSidePanelDate(dateHTML) {
	document.getElementById("dateContainer").innerHTML = dateHTML;
	document.getElementById("dateContainer").style.display = "block";
}

/**
 * Sets the content of the table container panel.
 * 
 * @param tableHTML - HTML string
 */
 function setSidePanelTable(tableHTML) {
	document.getElementById("tableContainer").innerHTML = tableHTML;
	document.getElementById("tableContainer").style.display = "block";
}