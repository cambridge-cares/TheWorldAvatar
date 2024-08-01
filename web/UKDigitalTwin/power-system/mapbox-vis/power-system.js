/**
*	This JS file handles functionality for the Power System visualisation.
*/


// Setup initial state of side panel components
function resetSidePanel() {
	document.getElementById('chartContainer').style.display = "none";
	document.getElementById('metadataContainer').style.display = "none";
	document.getElementById('tableContainer').style.display = "none";
	document.getElementById('legendContainer').style.display = "none";
	document.getElementById('dateContainer').style.display = "none";

	var titleHTML = `
		<h2>UK Power System</h2>
	`;
	setSidePanelTitle(titleHTML);

	var textHTML = `
		<p>The map to the left shows a sample of the power system data within the UK Digital Twin.​</p>
		<p>Each node on the map represents a generator with the size of the node corresponding to the capacity of the 
		generator. The colour of the nodes either shows the type of fuel used by the generators (on the Power Generation 
		layer) or an estimate of the contribution of the generators to Indicator 9.4.1 of the UN Sustainable Development 
		Goals (on the SDG Indicator layer).​</p>
	`;
	setSidePanelText(textHTML);

/**
 * Fired when a plant is selected.
 * 
 * @param name - plant name
 * @param fuel - fuel
 * @param capacity - capacity
 * @param indicator - SDG indicator
 * @param location - coordinates
 */
function selectPlant(name, fuel, capacity, indicator, location) {
	if(name == null) {
		// Do nothing
		return;
	}
	
	// Set title to offtake name
	setSidePanelTitle(`
		<h2>` + name + `</h2>
	`);

	// Pretty-print location
	var prettyLocation = "lat: " + roundN(location[1], 5) + ", long: " + roundN(location[0], 5);
	prettyLocation = "<a href='javascript:void(0)' onclick='panToLast()'>" + prettyLocation + "</a>"

	// Show meta data
	var metaHTML = `
		<table width="100%">
			<tr>
				<td width="25%">Fuel:</td>
				<td width="75%" style="text-align: right;">` + fuel + `</td>
			</tr>
			<tr>
				<td width="25%">Location:</td>
				<td width="75%" style="text-align: right;">` + prettyLocation + `</td>
			</tr>
			<tr>
				<td width="35%">Capacity:</td>
				<td width="65%" style="text-align: right;">` + capacity + ` MW</td>
			</tr>
	`;

	// Only add if present
	if(indicator != null) {
		metaHTML += `
			<tr>
				<td width="40%">Indicator 9.4.1:</td>
				<td width="60%" style="text-align: right;">` + indicator + ` kg/£</td>
			</tr>
		`;
	}

	metaHTML += "</table>";
	setSidePanelMeta(metaHTML);

	// Update text container 
	setSidePanelText(``);
}

/**
 * Update the legend depending on the current layer.
 * 
 * @param type 
 */
function updateLegend(type) {
	var html = `<div id="legend">`;

	if(type == "power") {
		html += `
			<b>Legend:</b><br>
			<div id="padding" style="height: 6px;"></div>
			<img width="18px" src="legend-coal.svg"/>Coal<br>
			<img width="18px" src="legend-coalbiomass.svg"/>Biomass<br>
			<img width="18px" src="legend-hydro.svg"/>Hydro<br>
			<img width="18px" src="legend-naturalgas.svg"/>Natural Gas<br>
			<img width="18px" src="legend-nuclear.svg"/>Nuclear<br>
			<img width="18px" src="legend-oil.svg"/>Oil<br>
			<img width="18px" src="legend-pumphydro.svg"/>Pump Hydro<br>
			<img width="18px" src="legend-solar.svg"/>Solar<br>
			<img width="18px" src="legend-sourgas.svg"/>Sour Gas<br>
			<img width="18px" src="legend-waste.svg"/>Waste<br>
			<img width="18px" src="legend-wastead.svg"/>Waste (Anaerobic Digestion)<br>
			<img width="18px" src="legend-wastemsw.svg"/>Waste (Municipal Solid Waste)<br>
			<img width="18px" src="legend-wind.svg"/>Wind<br>
		`;
	} else if(type == "indicator") {
		html += `
			<img src="legend-sdg.png" class="legend-sdg" width="325px"/>
		`;
	}
	html += `</div>`;

	setSidePanelLegend(html);
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