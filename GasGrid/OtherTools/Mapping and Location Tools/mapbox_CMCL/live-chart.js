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
const sampleHeadings = ["Datetime", "Instantaneous Flow"];
const sampleData = [
	{datetime:"2021-04-22T23:55:00", value:5.00},
	{datetime:"2021-04-22T23:56:00", value:5.00},
	{datetime:"2021-04-22T23:57:00", value:5.00},
	{datetime:"2021-04-22T23:58:00", value:5.00},
	{datetime:"2021-04-22T23:59:00", value:5.00},
	{datetime:"2021-04-23T00:00:00", value:5.00},
	{datetime:"2021-04-23T00:01:00", value:5.00},
	{datetime:"2021-04-23T00:02:00", value:5.00},
	{datetime:"2021-04-23T00:03:00", value:5.00},
	{datetime:"2021-04-23T00:04:00", value:5.00},
	{datetime:"2021-04-23T00:05:00", value:5.00}
];

// Table HTML when no data is available
const noDataTable = "<div id=\"noData\"><p>No available data.</p></div>";

// Line Chart object
var lineChart = null;

// Cached flow data
var flowData = null;


// Reset
function reset() {
	var titleContainer = document.getElementById('titleContainer');
	titleContainer.innerHTML = "";
	
	var subtitleContainer = document.getElementById('subtitleContainer');
	subtitleContainer.innerHTML = "";
	
	var chartContainer = document.getElementById('chartContainer');
	chartContainer.innerHTML = "";
	
	var tableContainer = document.getElementById('tableContainer');
	tableContainer.innerHTML = noDataTable;
}

// Runs when an offtake is selected, shows meta-data
function showOfftake(nodeName, nodeType, nodePosition) {
	console.log("Offtake selected, will only show metadata...");
	reset(); 
	
	
	
	
	
	
}


// Runs when a terminal is selected, shows recent live data.
function showTerminal(nodeName, nodeType, nodePosition) {
	console.log("Terminal selected, will plot data...");
	reset(); 
	
	// Set the chart title
	var titleContainer = document.getElementById('titleContainer');
	titleContainer.innerHTML = nodeName;
	
	// Set the chart subtitle
	var subtitleContainer = document.getElementById('subtitleContainer');
	var lat = roundN(nodePosition[0], 3);
	var lon = roundN(nodePosition[1], 3);
	subtitleContainer.innerHTML = nodeType + " (" + lat + ", " + lon + ")";
	
	// Show the side panel
	var sidePanel = document.getElementById('side-panel');
	sidePanel.style.display = "block";
	
	// Resize the map
	var mapPanel = document.getElementById('map');
	mapPanel.style.width = "calc(100% - 405px)";
	
	// Build the line graph and table
	// Find the relevant flow data entries
	var flows = findFlowData(nodeName);
	console.log(flows.length + " flow data points found.");
	
	lineChart = null;
	buildChart(flows);
}


// Loads the flow data json file
function loadFlowData() {
	if(flowData == null) {
		$.getJSON( "geoJSON_assets/flow-data.json", function(data) {
			flowData = data;
					
			var dateContainer = document.getElementById('dateContainer');
			var dateString = "Unknown";
			
			try {
				var req = new XMLHttpRequest();
				req.open("HEAD", "geoJSON_assets/flow-data.json", false);
				req.send(null);
				if(req.status== 200){
					dateString = req.getResponseHeader("Last-Modified");
				} 
			} catch(er) {
				console.log("Could not get modified date of flow-data.json");
				console.log(er.message);
			}
			
			dateContainer.innerHTML = "Data refreshed on: " + dateString;
			console.log("Flow data has been loaded.");
		});
	}
}


// Searches the cached flow data for all entries relating to the input terminal name
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


// Sorts a 2D array by the first entry
function sortFunction(a, b) {
    if (a[0] === b[0]) {
        return 0;
    }
    else {
        return (a[0] < b[0]) ? -1 : 1;
    }
}


// Generates the line chart
function buildChart(dataPoints) {
	// Determine the size of the chart
	var width = document.getElementById('chartContainer').offsetWidth;
	var height = document.getElementById('chartContainer').offsetHeight;
	
    // Parse datetimes within the sample data
	var timeParse = d3.timeParse("%Y-%m-%dT%H:%M:%S");
	dataPoints.forEach(function(point) {
		var originalDate = point[0].replace(".000Z", "");
		var newDate = timeParse(originalDate);
		if (newDate != null) point[0] = newDate;
		
		point[1] = parseFloat(point[1]);
	});
	
	// Sort the data based on datetime
	sortedData = dataPoints.sort(sortFunction);
	
	// Setup axis ranges/mapping
	var xRange = d3.extent(dataPoints, function(d) { 
		return d[0];
	});
	var yRange = d3.max(dataPoints, function(d) { 
		return d[1];
	});
		
	// Set up axis scales
	var x = d3.scaleTime()
		.range([0, (width - 50)])
		.domain(d3.extent(dataPoints, function(d){return d[0]}));
	var y = d3.scaleLinear()
		.range([height - 90, 0])
		.domain([
			d3.min(dataPoints, function(d){return d[1]}) - 1.0, 
			d3.max(dataPoints, function(d){return d[1]}) + 1.0
		]);

	// Setup axes themselves
	var xAxis = d3.axisBottom(x)
		.scale(x);
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
			.attr("class", "x axis")
			.attr("transform", "translate(30," + (height - 60) + ")")
			.call(xAxis)
		.selectAll("text")
			.attr("y", 0)
			.attr("x", 8)
			.attr("dy", ".30em")
			.attr("transform", "rotate(90)")
			.style("text-anchor", "start");
			
	// Label for the X axis
	vis.append("text")             
		.attr("transform", "translate(" + (width/2) + " , " + (height) + ")")
		.style("text-anchor", "middle")
		.attr("class", "xlabel")
		.text("Datetime");
	
	vis.append("g")
		.attr("class", "y axis")
		.attr("transform", "translate(30, 30)")
		.call(yAxis);
		
	// Add the line
	vis.append("path")
		.attr("transform", "translate(30, 30)")
		.datum(dataPoints) 
		.attr("class", "line")
		.attr("d", line);
	
		
	// Add the markers	
	vis.selectAll(".circle")
        .data(dataPoints)
		.enter()
		.append("circle")
		.attr("transform", "translate(30, 30)")
		.attr("class", "points")
		.attr("r", 2)
		.attr("cx", function(d) { return x(d[0]) })
		.attr("cy", function(d) { return y(d[1]) })
		.attr("fill", "#FF5733")
		
	// Build the table
	buildTable(dataPoints);
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
	var tableContainer = document.getElementById('tableContainer');
	tableContainer.innerHTML = htmlTable;
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