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

// Creates a line-graph with the input chart name
function plotLiveData(nodeName, nodeType, nodePosition) {
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
	lineChart = null;
	buildChart();
}


// Generates the line chart
function buildChart() {
	// Determine the size of the chart
	var width = document.getElementById('chartContainer').offsetWidth;
	var height = document.getElementById('chartContainer').offsetHeight;
	
	// Copy the data as we're going to edit it
	var data = JSON.parse(JSON.stringify(sampleData));
	
	// Parse datetimes within the sample data
	var timeParse = d3.timeParse("%Y-%m-%dT%H:%M:%S");
	data.forEach(function(d) {
		var newDate = timeParse(d.datetime);
		if (newDate != null) d.datetime = newDate;
	});

	
	// Add some noise to each value in the sample data
	data.forEach(function(d) {
		var noise = (((Math.floor(Math.random() * 40)) - 20) / 100);
		d.value *= (1.0 + noise);
	});
	
	// Setup axis ranges/mapping
	var xRange = d3.extent(data, function(d) { 
		return d.datetime; 
	});
	var yRange = d3.max(data, function(d) { 
		return d.value; 
	});
		
	// Set up axis scales
	var x = d3.scaleTime()
		.range([0, (width - 50)])
		.domain(d3.extent(data, function(d){return d.datetime}));
	var y = d3.scaleLinear()
		.range([height - 90, 0])
		.domain([
			d3.min(data, function(d){return d.value}) - 1.0, 
			d3.max(data, function(d){return d.value}) + 1.0
		]);

	// Setup axes themselves
	var xAxis = d3.axisBottom(x)
		.scale(x);
	var yAxis = d3.axisLeft(y);
	
	// Setup the line generator
	var line = d3.line()
		.x(function(d) { return x(d.datetime); })
		.y(function(d) { return y(d.value); });
		
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
		.datum(data) 
		.attr("class", "line")
		.attr("d", line);
		
	// Add the markers	
	vis.selectAll(".circle")
        .data(data)
		.enter()
		.append("circle")
		.attr("transform", "translate(30, 30)")
		.attr("class", "points")
		.attr("r", 2)
		.attr("cx", function(d) { return x(d.datetime) })
		.attr("cy", function(d) { return y(d.value) })
		.attr("fill", "#FF5733")
		
	// Build the table
	buildTable(data);
}


// Generates the raw data table	
function buildTable(data) {
	
	// Build the HTML table of raw data
	var htmlTable = "<table id=\"dataTable\">";
	htmlTable += "<tr><th>" + sampleHeadings[0] + "</th>";
	htmlTable += "<th>" + sampleHeadings[1] + "</th></tr>";
	
	// Add the rows
	for (var i = 0; i < data.length; i++) {
		var rowData = data[i];
		
		// Get data (add some noise for variability)
		var dateTime = prettyPrintDate(rowData.datetime);
		var flow = rowData.value;
		
		// Build HTML row
		htmlTable += "<tr>";
		htmlTable += "<td>" + dateTime + "</td>";
		htmlTable += "<td>" + roundN(flow, 2) + "</td>";
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