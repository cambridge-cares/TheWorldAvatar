/**
*	This JS file handles the creation of a line chart for the Land Use visualisation.
*	Note that this scripts needs to be loaded into an environment that also contains the
*	following JS libraries:
*	
*		- JQuery
*/

const svgTemplate = `
	<tr id="legend-item">
		<td width="24px">
			<svg width="18" height="18">
				<circle cx="9" cy="9" r="9" fill="COLOR" />
			</svg>
		</td>
		<td width="100%">NAME</td>
	</tr>
`;

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
		<p>The map to the left shows a sample of the land use data from the East Anglia region, in the UK.</p>
		<p>Crop designations for the current county can be seen by hovering over the map, or using the legend below. Use the mouse wheel to zoom the map and provide a higher resolution of crop areas.</p>
	`;
	setSidePanelText(textHTML);

	var legendHTML = `
		<div id="legend">
			<b>Legend:</b><br>
			<div id="padding" style="height: 6px;"></div>
			<table>
	`;

	var legendItems = [];

	for(var i = 1; i < colors.length; i++) {
		var current = colors[i];
		if(Array.isArray(current)) {
			current = current[0];
		}


		if(current.startsWith("#") && current !== "#000000") {
			var crop = crops[colors[i - 1]];

			var legendItem = svgTemplate.replace("COLOR", current);
			legendItem = legendItem.replace("NAME", crop);
			legendItems.push([crop, legendItem]);
		}
	}

	// Sort legend items by crop name
	legendItems.sort(sortFunction);

	for(var i = 0; i < legendItems.length; i++) {
		legendHTML += legendItems[i][1];
	}
	legendHTML += "</table></div>";
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

		if(inColors(iri)) {
			registerLayer(crop, [iri], "Crops", true);
		}
	}
}

// Returns true if the input IRI appears in the colors array
function inColors(iri) {
	for(var i = 0; i < colors.length; i++) {
		var item = colors[i];
		if(Array.isArray(item)) {
			item = colors[i][0];
		}

		if(item === iri) {
			return true;
		}
	}

	return false;
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

// Sort 2D array by first item
function sortFunction(a, b) {
    if (a[0] === b[0]) {
        return 0;
    }
    else {
        return (a[0] < b[0]) ? -1 : 1;
    }
}