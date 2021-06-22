/**
*	This JS file handles the creation of a line chart for the Gas Grid visualisation.
*	Note that this scripts needs to be loaded into an environment that also contains the
*	following JS libraries:
*	
*		- JQuery
*		- D3
*/

const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

// Hardcoded sample data
const sampleHeadings = ["Time", "Instantaneous Flow<br><span style='font-size: 75%;'>[m<sup>3</sup>/s]</span>"];

// Table HTML when no data is available
const noDataTable = "<div id=\"noData\"><p>No available data.</p></div>";

// Last selected location
var lastLocation = null;

// Cached flow data
var flowData = null;

// Setup initial state of side panel components
function resetSidePanel() {
	document.getElementById('chartContainer').style.display = "none";
	document.getElementById('metadataContainer').style.display = "none";
	document.getElementById('tableContainer').style.display = "none";

	var titleHTML = `
		<h2>UK Gas Grid</h2>
	`;
	setSidePanelTitle(titleHTML);

	var textHTML = `
		<p>The map to the left shows a sample of gas grid data within the UK Digital Twin.
		Intake Terminals (<span style="color:#108dcc;">blue</span>), Offtakes (<span style="color:#B42222;">red</span>), and Pipes from the gas transmission system are shown for the mainland UK.</p>
		<p>Select an Intake Terminal (<span style="color:#108dcc;">blue</span>) to see its recent Instantaneous Flow data. This data is pulled from the UK Digital Twin
		and is updated on a daily basis.</p>
	`;
	setSidePanelText(textHTML);

	var legendHTML = `
		<div id="legend">
			<b>Legend:</b><br>
			<div id="padding" style="height: 6px;"></div>
			<img width="24px" src="legend-terminal.svg"/>Terminals<br>
			<img width="24px" src="legend-offtake.svg"/>Offtakes<br>
			<img width="24px" src="legend-pipe.svg"/>Pipelines<br>
		</div>
	`;
	setSidePanelLegend(legendHTML);
}

// Runs when an offtake is selected, shows meta-data
function showOfftake(nodeName, nodeType, nodePosition, LinepackZoneStr, NTSExitArea, NTSExitZone, Pipeline) {
	if(nodeName == null) {
		// Do nothing
		return;
	}
	
	lastLocation = nodePosition;
	console.log("Offtake selected, will only show metadata...");
	resetSidePanel(); 
	
	// Set title to offtake name
	setSidePanelTitle(`
		<h2 style="color: #B42222 !important;">` + nodeName + `</h2>
	`);

	// Pretty-print location
	var prettyLocation = "lat: " + roundN(nodePosition[1], 5) + ", long: " + roundN(nodePosition[0], 5);
	prettyLocation = "<a href='javascript:void(0)' onclick='panToLast()'>" + prettyLocation + "</a>"

	// Show meta data
	var metaHTML = `
		<table width="100%">
			<tr>
				<td width="25%">Type:</td>
				<td width="75%" style="text-align: right;">` + nodeType + `, Offtake</td>
			</tr>
			<tr>
				<td width="25%">Location:</td>
				<td width="75%" style="text-align: right;">` + prettyLocation + `</td>
			</tr>
	`;

	// Only add if present
	if(LinepackZoneStr != null) {
		metaHTML += `
			<tr>
				<td width="35%">Linepack Zone:</td>
				<td width="65%" style="text-align: right;">` + LinepackZoneStr + `</td>
			</tr>
		`;
	}

	// Only add if present
	if(NTSExitArea != null) {
		metaHTML += `
			<tr>
				<td width="35%">NTS Exit Area:</td>
				<td width="65%" style="text-align: right;">` + NTSExitArea + `</td>
			</tr>
		`;
	}

	// Only add if present
	if(NTSExitZone != null) {
		metaHTML += `
			<tr>
				<td width="35%">NTS Exit Zone:</td>
				<td width="65%" style="text-align: right;">` + NTSExitZone + `</td>
			</tr>
		`;
	}

	// Only add if present
	if(Pipeline != null) {
		var parts = Pipeline.split("/");
		Pipeline = parts[parts.length - 1].replace("Connection", "");

		metaHTML += `
			<tr>
				<td width="35%">Connection:</td>
				<td width="65%" style="text-align: right;">` + Pipeline + `</td>
			</tr>
		`;
	}
		
	metaHTML += "</table>";
	setSidePanelMeta(metaHTML);

	// Update text container 
	setSidePanelText(`
		<p style='font-style: italic; color: grey;'>Select an Intake Terminal (<span style="color:#108dcc;">blue</span>) to view Instantaneous Flow data.</p>
	`);
}

// Runs when a terminal is selected, shows recent live data.
function showTerminal(nodeName, nodeType, nodePosition) {
	if(nodeName == null) {
		// Do nothing
		return;
	}
	
	lastLocation = nodePosition;
	console.log("Terminal selected, will plot data...");
	resetSidePanel(); 
	
	// Set title to terminal name
	setSidePanelTitle(`
		<h2 style="color: #108dcc !important;">` + nodeName + `</h2>
	`);

	// Pretty-print location
	var prettyLocation = "lat: " + roundN(nodePosition[1], 5) + ", long: " + roundN(nodePosition[0], 5);
	prettyLocation = "<a href='javascript:void(0)' onclick='panToLast()'>" + prettyLocation + "</a>"

	// Show meta data
	setSidePanelMeta(`
		<table width="100%">
			<tr>
				<td width="30%">Type:</td>
				<td width="70%" style="text-align: right;">Intake</td>
			</tr>
			<tr>
				<td width="30%">Location:</td>
				<td width="70%" style="text-align: right;">` + prettyLocation + `</td>
			</tr>
		</table>
	`);

	// Hide unused side-panel components
	document.getElementById('textContainer').style.display = "none";
	document.getElementById('legendContainer').style.display = "none";

	// Show elements to display flow data
	document.getElementById('chartContainer').style.display = "block";
	document.getElementById('tableContainer').style.display = "block";

	// Find the relevant flow data entries
	var flows = findFlowData(nodeName);
	console.log(flows.length + " flow data points found.");
	
	// Build the line graph and table
	document.getElementById('chartContainer').innerHTML = "";

	buildChart(flows);
	buildTable(flows);
}

/**
 * Loads the flow-data.json file containing recent flow data for
 * all input terminals.
 */
function loadFlowData() {
	if(flowData == null) {
		$.ajax({
			url: "geoJSON_assets/flow-data.json",
			type: "get",
			dataType: "json",
			cache: true,
			success: flowSuccess,
			failure: flowFailure,
			async: true
		});
	}
}

/**
 * Fired when flow data is downloaded successfully.
 * 
 * @param data - flow data
 * @param status - return status
 * @param xhr - response header
 */
function flowSuccess(data, status, xhr) {
	flowData = data;
	dateString = xhr.getResponseHeader("Last-Modified");
	setSidePanelDate("Data updated on " + dateString);
	console.log("Flow data has been loaded.");
}

/**
 * Fired when flow data could not be downloaded.
 */
function flowFailure() {
	console.log("Could not get modified date of flow-data.json");
	console.log(status);
}


/**
 * Searches the cached flow data for all entries relating to the input terminal name.
 * 
 * @param nodeName - name of Input Terminal to get data for.
 * 
 * @returns resulting flow data array.
 */
function findFlowData(nodeName) {
	var flows = [];
	
	for(var i = 0; i < flowData.length; i++) {
		entry = flowData[i];
		label = entry["label"];
		
		if (label.toLowerCase() === nodeName.toLowerCase()) {
			datetime = entry["UTC"];
			value = entry["num_val"];
			flows.push([datetime, value]);
		}
	}
	
	return flows;
}


/**
 * Sorts 2D arrays by their first entry.
 * 
 * @param a - 2D array
 * @param b - 2D array
 */
function sortFunction(a, b) {
    if (a[0] === b[0]) {
        return 0;
    }
    else {
        return (a[0] < b[0]) ? -1 : 1;
    }
}


/**
 * Generates the line chart.
 * 
 * @param dataPoints - flow data.
 */
function buildChart(dataPoints) {
	// Determine the size of the chart
	var width = 330;
	var height = 300;

    // Parse datetimes within the sample data
	var timeParse = d3.timeParse("%Y-%m-%dT%H:%M:%S");
	dataPoints.forEach(function(point) {
		var originalDate = point[0].replace(".000Z", "");
		var newDate = timeParse(originalDate);
		if (newDate != null) point[0] = newDate;
		
		point[1] = parseFloat(point[1]);
	});
	
	// Sort the data based on datetime
	var currentChartData = dataPoints.sort(sortFunction);

	// Setup axis ranges/mapping
	var xRange = d3.extent(currentChartData, function(d) { 
		return d[0];
	});
	var yRange = d3.max(currentChartData, function(d) { 
		return d[1];
	});
		
	// Set up axis scales
	var x = d3.scaleTime()
		.range([0, (width - 60)])
		.domain(d3.extent(currentChartData, function(d){return d[0]}));
	var y = d3.scaleLinear()
		.range([height - 60, 0])
		.domain([
			d3.min(currentChartData, function(d){return d[1]}) - 1.0, 
			d3.max(currentChartData, function(d){return d[1]}) + 1.0
		]);

	// Setup axes themselves
	var xAxis = d3.axisBottom(x)
		.scale(x)
		.tickFormat(d3.timeFormat("%H:%M"));
	var yAxis = d3.axisLeft(y);
	
	// Setup the line generator
	var line = d3.line()
		.x(function(d) { return x(d[0]); })
		.y(function(d) { return y(d[1]); });
		
	// Create the chart	
	var vis = d3.select("#chartContainer").append("svg")
		.attr("width", width)
		.attr("height", height); 
		
	// Add the X axis
	vis.append("g")
		.attr("class", "axis")
		.attr("transform", "translate(50, " + (height - 50) + ")")
		.call(xAxis)
	.selectAll("text")
		.attr("y", -3)
		.attr("x", 8)
		.attr("transform", "rotate(90)")
		.style("text-anchor", "start");
			
	// Label for the X axis
	vis.append("text")             
		.attr("transform", "translate(" + ((width / 2) + 15) + " , " + (height) + ")")
		.style("text-anchor", "middle")
		.attr("class", "axisLabel")
		.text("Time");
	
	// Add the Y axis
	vis.append("g")
		.attr("class", "axis")
		.attr("transform", "translate(50, 10)")
		.call(yAxis);

	// Label for the Y axis
	vis.append("text")        
		.attr("transform", "rotate(-90)translate(" + (-1 * (height/2) + 25) + ", 10)")
		.style("text-anchor", "middle")
		.attr("class", "axisLabel")
		.text("Instantaneous Flow [m^3/s]");
		
	// Add the line
	vis.append("path")
		.attr("transform", "translate(50, 10)")
		.datum(dataPoints) 
		.attr("class", "line")
		.attr("d", line);
}


// Generates the raw data table	
function buildTable(dataPoints) {
	
	// Build the HTML table of raw data
	var htmlTable = "<table id=\"dataTable\">";
	htmlTable += "<tr><th>" + sampleHeadings[0] + "</th>";
	htmlTable += "<th>" + sampleHeadings[1] + "</th></tr>";
	
	// Add the rows
	for (var i = 0; i < dataPoints.length; i++) {
		var rowData = dataPoints[i];
		
		// Get data (add some noise for variability)
		var dateTime = prettyPrintDate(rowData[0]);
		var flow = rowData[1];
		
		// Build HTML row
		htmlTable += "<tr>";
		htmlTable += "<td>" + dateTime + "</td>";
		htmlTable += "<td>" + roundN(flow, 3) + "</td>";
		htmlTable += "</tr>";
	}
	htmlTable += "</table>"
	
	// Add the HTML table
	setSidePanelTable(htmlTable);
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