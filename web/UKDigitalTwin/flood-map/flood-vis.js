/**
*	This JS file handles functionality for the Flood Visualisation.
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

function addMouseEffects(map, layerName) {
	// On mouse enter
	map.on('mouseenter', layerName, function (e) {
		map.getCanvas().style.cursor = 'pointer';

		if(map.getZoom() < 12.5) {
			var coordinates = e.features[0].geometry.coordinates.slice();
			var name = e.features[0].properties["name"];
			
			if(name == null || name === "") {
				var name = e.features[0].properties["Offtake Point (License Name)"];
			} else if(name.startsWith("http")) {
				name = crops[name];
			}
			name = name.replaceAll("_", " ");

			var html = `
				<b> ` + name + `</b><br>
				<p>Click for more details.</p>
			`;
			popup.setLngLat(coordinates).setHTML(html).addTo(map);
		}
	});

	//On mouse exit
	map.on('mouseleave', layerName, function (e) {
		map.getCanvas().style.cursor = '';
		popup.remove();
	});

	// On selection
	map.on('click', layerName, function (e) {
		var coordinates = e.features[0].geometry.coordinates.slice();
		selectItem(coordinates, e.features[0].properties);

		map.flyTo({
			center: coordinates,
			curve: 1.9,
			speed: 1.6,
			pitch: 75,
			zoom: 14,
			bearing: Math.random() * 90
		});
	});
}

/**
 * Adds a new layer to the map to add labels to locations within the input data source.
 * 
 * @param {*} map MapBox object.
 * @param {*} layerName desired layer name.
 * @param {*} sourceName name of data source.
 * @param {*} innerColor text inner color.
 * @param {*} outerColor text outer color.
 * @param {*} name name field in geojson
 */
function addLabels(map, layerName, sourceName, innerColor, outerColor, nameField, visibility) {
	map.addLayer({
		id: layerName,
		type: 'symbol',
		source: sourceName,
		layout: {
			'visibility': visibility,
			'text-field': ['concat',
				['get', nameField],
				' \n | \n | \n | \n | ']
			,
			'text-font': ['DIN Offc Pro Medium', 'Arial Unicode MS Bold'],
			'text-size': 14,
			'text-offset': [0, -4],
		},
		"paint": {
			"text-color": innerColor,
			"text-halo-color": outerColor,
			"text-halo-width": 0.8,
			"text-opacity": ['interpolate', ['exponential', 2], ['zoom'], 8.5, 0, 12.5, 1]
		}
	});
}

// Setup initial state of side panel components
function resetSidePanel() {
	document.getElementById('chartContainer').style.display = "none";
	document.getElementById('metadataContainer').style.display = "none";
	document.getElementById('tableContainer').style.display = "none";
	document.getElementById('dateContainer').style.display = "none";

	var titleHTML = `
		<h2>UK Flood Map</h2>
	`;
	setSidePanelTitle(titleHTML);

	var textHTML = `
		<p>The map to the left shows a sample of the flood data within the UK Digital Twin.​</p>
		<p>Here is some text that explains what's shown within the visualisation as well as a few lines on
		how to use the controls to change what you're looking at. Blah Blah Blah.​</p>
	`;
	setSidePanelText(textHTML);
}

/**
 * Fired when an item is selected.
 * 
 * @param location - coordinates
 * @param properties - GEOJSON properties
 */
function selectItem(location, properties) {
	if(properties == null) {
		// Do nothing
		return;
	}

	var name = properties["name"];
	if(name == null || name === "") {
		var name = properties["Offtake Point (License Name)"];
	} else if(name.startsWith("http")) {
		name = crops[name];
	}
	name = name.replaceAll("_", " ");

	// Set title to item name
	setSidePanelTitle(`
		<h2>` + name + `</h2>
	`);

	// Pretty-print location
	var prettyLocation = "lat: " + roundN(location[1], 5) + ", long: " + roundN(location[0], 5);
	prettyLocation = "<a href='javascript:void(0)' onclick='panToLast()'>" + prettyLocation + "</a>"

	// Show meta data
	var metaHTML = "<table width='100%'>";

	for (const [key, value] of Object.entries(properties)) {
		if(key === "name") continue;
		if(key.startsWith("marker")) continue;
		if(key.startsWith("sdg")) continue;

		// Value will be too long, skip it
		if(key.startsWith("Connected to")) continue;

		metaHTML += `
			<tr>
				<td width="50%">` + capitalize(key) + `:</td>
				<td width="50%" style="text-align: right;">` + value + `</td>
			</tr>
		`;
	}

	metaHTML += "</table>";
	setSidePanelMeta(metaHTML);

	// Update text container 
	setSidePanelText(``);
}

// Capitalize a string
function capitalize(word) {
	return word[0].toUpperCase() + word.slice(1).toLowerCase();
}

function sortFunction(a, b) {
    if (a[0] === b[0]) {
        return 0;
    }
    else {
        return (a[0] < b[0]) ? -1 : 1;
    }
}

/**
 * Update the legend depending on the current layer.
 * 
 * @param type 
 */
function updateLegend(activeLayers) {
	var html = `<div id="legend" class="w3-sidebar w3-bar-block w3-light-grey w3-card">`;
	html += `<div class="w3-bar-item legend-title">Legends:</div>`;
	html += `<button id="power-button" class="w3-bar-item w3-button tablink" onclick="openLegend(event, 'power-legend')">Power</button>`;
	html += `<button id="gas-button" class="w3-bar-item w3-button tablink" onclick="openLegend(event, 'gas-legend')">Gas</button>`;
	html += `<button id="crop-button" class="w3-bar-item w3-button tablink" onclick="openLegend(event, 'crop-legend')">Crops</button>`;
	html += `</div>`;

	console.log(html);

	var powerDiv = `
		<div id="power-legend" class="w3-container legend-right" style="display:none">
			<div id="padding" style="height: 6px;"></div>
			<img width="18px" src="legend/power/legend-coal.svg"/>Coal<br>
			<img width="18px" src="legend/power/legend-coalbiomass.svg"/>Coal & Biomass<br>
			<img width="18px" src="legend/power/legend-hydro.svg"/>Hydro<br>
			<img width="18px" src="legend/power/legend-naturalgas.svg"/>Natural Gas<br>
			<img width="18px" src="legend/power/legend-nuclear.svg"/>Nuclear<br>
			<img width="18px" src="legend/power/legend-oil.svg"/>Oil<br>
			<img width="18px" src="legend/power/legend-pumphydro.svg"/>Pump Hydro<br>
			<img width="18px" src="legend/power/legend-solar.svg"/>Solar<br>
			<img width="18px" src="legend/power/legend-sourgas.svg"/>Sour Gas<br>
			<img width="18px" src="legend/power/legend-waste.svg"/>Waste<br>
			<img width="18px" src="legend/power/legend-wastead.svg"/>Waste (Anaerobic Digestion)<br>
			<img width="18px" src="legend/power/legend-wastemsw.svg"/>Waste (Municipal Solid Waste)<br>
			<img width="18px" src="legend/power/legend-wind.svg"/>Wind<br>
		</div>
	`;
	html += powerDiv;

	var gasDiv = `
		<div id="gas-legend" class="w3-container legend-right" style="display:none">
		<div id="padding" style="height: 6px;"></div>
			<div style="padding-bottom: 2px;"><img width="24px" src="legend/gas/triangle-down-black.png"/>Terminals</div>
			<div style="padding-bottom: 2px;"><img width="24px" src="legend/gas/triangle-up-black.png"/>Offtakes</div>
			<div style="padding-bottom: 2px;"><img width="24px" src="legend/gas/legend-pipe.svg"/>Pipelines</div>
		</div>
	`;
	html += gasDiv;

	
	var legendItems = [];

	for(var i = 1; i < crop_colors.length; i++) {
		var current = crop_colors[i];
		if(Array.isArray(current)) {
			current = current[0];
		}

		if(current.startsWith("#") && current !== "#000000") {
			var crop = crops[crop_colors[i - 1]];

			var legendItem = svgTemplate.replace("COLOR", current);
			legendItem = legendItem.replace("NAME", crop);
			legendItems.push([crop, legendItem]);
		}
	}
	legendItems.sort(sortFunction);

	var cropDiv = `
		<div id="crop-legend" class="w3-container legend-right" style="display:none">
			<div id="padding" style="height: 6px;"></div>
			<table>
	`;
	for(var i = 0; i < legendItems.length; i++) {
		cropDiv += legendItems[i][1];
	}
	cropDiv += "</table></div>";
	html += cropDiv;

	setSidePanelLegend(html);
}

function openLegend(evt, dataSet) {
	var i, x, tablinks;
	x = document.getElementsByClassName("legend-right");
	for (i = 0; i < x.length; i++) {
	  x[i].style.display = "none";
	}
	tablinks = document.getElementsByClassName("tablink");
	for (i = 0; i < x.length; i++) {
	  tablinks[i].className = tablinks[i].className.replace(" w3-red", ""); 
	}
	document.getElementById(dataSet).style.display = "block";
	evt.currentTarget.className += " w3-red";
}


// Pretty print date
function prettyPrintDate(date) {
	var day = "" + date.getDate();
	var month = months[date.getMonth()];
	
	var hour = "" + date.getHours();
	var minute = "" + date.getMinutes();
	
	if (day.length < 2) day = "0" + day;
	if (month.length < 2) month = "0" + month;
	if (hour.length < 2) hour = "0" + hour;
	if (minute.length < 2) minute = "0" + minute;
	
	return addOrd(day) + " " + month + ", " + hour + ":" + minute;
}


// Get number with ordinal
function addOrd(n) {
  var ords = [, 'st', 'nd', 'rd'];
  var ord, m = n % 100;
  return n + ((m > 10 && m < 14) ? 'th' : ords[m % 10] || 'th');
}


// Round digit to N decimal places
function roundN(value, digits) {
   var tenToN = 10 ** digits;
   return (Math.round(value * tenToN)) / tenToN;
}