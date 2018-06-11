/**
 * 
 */
var listExchangeRates;

var processInputs = function() {
	console.log("TEST FROM JAVA SERVLET");
}

$(document).ready(function() {
	console.log("START downloading and saving market data in the knowledge base");
	$.getJSON('/JPS_Arbitrage/downloadingAndSavingMarketDataInTheKnowledgeBase',
			function(data){
		console.log(data);
		console.log("DONE downloading and saving market data in the knowledge base");
		// -- Process HTTP response in the form of a string -- //
//		var positionSecondAmpersand = data.indexOf('&', 1);
//		var stringHeaderAndXVal = data.slice(1, positionSecondAmpersand);
//		var stringDatetimeAndYVal = data.slice(positionSecondAmpersand + 1);
//		
//		// -- Splits individual strings which are comma-separated into array of strings -- //
//		var arrayHeaderAndXVal = stringHeaderAndXVal.split(',');
//		var arrayDatetimeAndYVal = stringDatetimeAndYVal.split(',');
//		
//		// -- Separates arrays into its constituents -- //
//		var arrayHeader = arrayHeaderAndXVal.slice(0,4);
//		var arrayXVal = arrayHeaderAndXVal.slice(4);
//		
//		var arrayDatetime = arrayDatetimeAndYVal.slice(0,4);
//		var arrayYVal = arrayDatetimeAndYVal.slice(4);
//		
//		// -- arrayDatetime and arrayXVal must have equal length i.e. each datetime must have a corresponding xval -- //
//		if (arrayYVal.length > 0 && arrayXVal.length > 0) {
//			var parsedData = parseData(arrayYVal, arrayXVal);
//        	drawChart(parsedData);
//		} else {
//			console.log("NO DATA RETRIEVED");
//		}
	}).done(function(){	
		console.log("START downloading and saving exchange rates in the knowledge base");
		$.getJSON('/JPS_Arbitrage/downloadingAndSavingExchangeRatesInTheKnowledgeBase',
			function(data){
				let arrayExchangeRates = JSON.parse(data);
				let arraySize = arrayExchangeRates[0].length;
				for(let i = 0; i < arraySize; i++) {
					console.log(arrayExchangeRates[0][i], arrayExchangeRates[1][i]);
				}
				console.log("DONE downloading and saving exchange rates in the knowledge base");
		});
	}).done(function(){
		console.log("START running arbitrage analysis using MoDS with market data provided by DataDownloadAgent");
		$.getJSON('/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent',
			{
				MoDS_input: JSON.stringify([24220.0656])
			},
			function(data){
				console.log(data);
				console.log("DONE running arbitrage analysis using MoDS with market data provided by DataDownloadAgent");
			});
	});
});

//API to fetch historical data of Bitcoin Price Index
const api = 'https://api.coindesk.com/v1/bpi/historical/close.json?start=2017-12-31&end=2018-04-01';

/**
 * Loading data from API when DOM Content has been loaded'.
 */
//document.addEventListener("DOMContentLoaded", function(event) {
//fetch(api)
//    .then(function(response) { return response.json(); })
//    .then(function(data) {
//        var parsedData = parseData(data);
//        drawChart(parsedData);
//    })
//    .catch(function(err) { console.log(err); })
//});

/**
 * Parse data into key-value pairs
 * @param {object} data Object containing historical data of BPI
 */
function parseData(arrayYVal, arrayXVal) {
    var arr = [];
    for (var i in arrayXVal) {
    	
//    	console.log(arrayYVal[i]);
//    	console.log(typeof(arrayYVal[i]));
//    	
//    	console.log(new Date(arrayXVal[i]));
//    	console.log(typeof(new Date(arrayXVal[i])));
//    	
    	var yVal = +arrayYVal[i];
    	
    	console.log(yVal);
    	console.log(typeof(yVal));
    	
//    	if(isNaN(yVal)) {
//    		yVal = +0;
//    	}
//    	
//    	console.log(yVal);
    	console.log("END");
        
    	arr.push({
            date: new Date(arrayXVal[i]), //date
            value: yVal //convert string to number
        });
    }
    return arr;
}

/**
 * Creates a chart using D3
 * @param {object} data Object containing historical data of BPI
 */
function drawChart(data) {
var svgWidth = 750, svgHeight = 500; // initially 600, 400
var margin = { top: 20, right: 20, bottom: 30, left: 50 };
var width = svgWidth - margin.left - margin.right;
var height = svgHeight - margin.top - margin.bottom;

var svg = d3.select('svg')
    .attr("width", svgWidth)
    .attr("height", svgHeight);
    
var g = svg.append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var x = d3.scaleTime()
    .rangeRound([0, width]);

var y = d3.scaleLinear()
    .rangeRound([height, 0]);

var line = d3.line()
    .x(function(d) { return x(d.date)})
    .y(function(d) { return y(d.value)})
    x.domain(d3.extent(data, function(d) { return d.date }));
    y.domain(d3.extent(data, function(d) { return d.value }));

g.append("g")
    .attr("transform", "translate(0," + height + ")")
    .call(d3.axisBottom(x))
    .select(".domain")
    .remove();

g.append("g")
    .call(d3.axisLeft(y))
    .append("text")
    .attr("fill", "#000")
    .attr("transform", "rotate(-90)")
    .attr("y", 6)
    .attr("dy", "0.71em")
    .attr("text-anchor", "end")
    .text("Price ($)");

g.append("path")
    .datum(data)
    .attr("fill", "none")
    .attr("stroke", "steelblue")
    .attr("stroke-linejoin", "round")
    .attr("stroke-linecap", "round")
    .attr("stroke-width", 1.5)
    .attr("d", line);
}

