/**
*	This JS file handles the creation of a line chart for the Land Use visualisation.
*	Note that this scripts needs to be loaded into an environment that also contains the
*	following JS libraries:
*	
*		- JQuery
*/


// Last selected location
var lastLocation = null;

// Setup initial state of side panel components
function resetSidePanel() {
	document.getElementById('chartContainer').style.display = "none";
	document.getElementById('metadataContainer').style.display = "none";
	document.getElementById('tableContainer').style.display = "none";

	var titleHTML = `
		<h2>Cambridgeshire<br>Land Use</h2>
	`;
	setSidePanelTitle(titleHTML);

	var textHTML = `
		<p>The map to the left shows a sample of the land use data from Cambridgeshire, UK.</p>
		<p>Crop designations can be seen by hovering over the map, or using the legend below. Use the mouse wheel to zoom the map and provide a higher resolution of crop areas.</p>
	`;
	setSidePanelText(textHTML);

	var legendHTML = `
		<div id="legend">
			<b>Legend:</b><br>
			<div id="padding" style="height: 6px;"></div>
			<img width="24px" src="legend-terminal.svg"/>Terminals<br>
		</div>
	`;
	setSidePanelLegend(legendHTML);
}

/**
 * Setup layer controls for each crop type.
 */
function setupLayerControl() {
	// Sort the dictionary based on the second element
	var items = Object.keys(crops).map(function(key) {
		return [key, crops[key]];
	});

	items.sort(function(first, second) {
		return first[1].localeCompare(second[1]);
	});

	// Register layers
	for(var i = 0; i < items.length; i++) {
		var iri = items[i][0];
		var crop = items[i][1];
		crop = crop.replace(/\w\S*/g, (w) => (w.replace(/^\w/, (c) => c.toUpperCase())));

		registerLayer(crop, [iri], "Crops", true);
	}
}


// Runs when a crop location is selected
function showCrop(cropName, nodePosition) {
	if(cropName == null) {
		// Do nothing
		return;
	}

	lastLocation = nodePosition;
	resetSidePanel(); 
	
	// Set title to offtake name
	setSidePanelTitle(`
		<h2>` + cropName + `</h2>
	`);

	// Pretty-print location
	var prettyLocation = "lat: " + roundN(nodePosition[1], 5) + ", long: " + roundN(nodePosition[0], 5);
	prettyLocation = "<a href='javascript:void(0)' onclick='panToLast()'>" + prettyLocation + "</a>"

	// Show meta data
	var metaHTML = `
		<table width="100%">
			<tr>
				<td width="25%">Location:</td>
				<td width="75%" style="text-align: right;">` + prettyLocation + `</td>
			</tr>
		</table>
	`;
	setSidePanelMeta(metaHTML);

	// Update text container 
	setSidePanelText(`
		<p style='font-style: italic; color: grey;'>Select another data point to see the crop type and coordinates.</p>
	`);
}

// Round digit to N decimal places
function roundN(value, digits) {
	var tenToN = 10 ** digits;
	return (Math.round(value * tenToN)) / tenToN;
}

// Pans back to the last selected location
function panToLast() {
	if(lastLocation != null) {
		map.flyTo({
			center: lastLocation,
			curve: 1.9,
			speed: 1.6,
			pitch: 45,
			zoom: 16
		});
	}
}