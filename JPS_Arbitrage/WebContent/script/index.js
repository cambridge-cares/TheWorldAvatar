/**
 * 
 */

const downloadAndSaveMarketData = () => {
	return $.getJSON('/JPS_Arbitrage/downloadingAndSavingMarketDataInTheKnowledgeBase');
}

const processMarketData = (marketData) => {
	console.log("In processMarketData function");
    // -- Process HTTP response in the form of a string -- //
	let splitStrings = marketData.split('&');
	let stringSize = splitStrings.length;

	let positionSecondAmpersand = marketData.indexOf('&', 1);
	let stringHeaderAndXVal = marketData.slice(1, positionSecondAmpersand);
	let stringDatetimeAndYVal = marketData.slice(positionSecondAmpersand + 1);

	// -- Splits individual strings which are comma-separated into array of strings -- //
	let arrayHeaderAndXVal = stringHeaderAndXVal.split(',');
	let arrayDatetimeAndYVal = stringDatetimeAndYVal.split(',');

	// -- Separates arrays into its constituents -- //
	let arrayHeader = arrayHeaderAndXVal.slice(0,4);
	let arrayXVal = arrayHeaderAndXVal.slice(4);

	let arrayDatetime = arrayDatetimeAndYVal.slice(0,4);
	let arrayYVal = arrayDatetimeAndYVal.slice(4);

	// -- arrayDatetime and arrayXVal must have equal length i.e. each datetime must have a corresponding xval -- //
	if (arrayYVal.length > 0 && arrayXVal.length > 0) {
		let parsedData = parseData(arrayYVal, arrayXVal);
		drawChart(parsedData);
	} else {
		console.log("NO MARKET DATA FOR CPO RETRIEVED");
	}

	return;
}

const downloadAndSaveExchangeRates = () => {
	return $.getJSON('/JPS_Arbitrage/downloadingAndSavingExchangeRatesInTheKnowledgeBase');
}

/**
 * param exchangeRates is a JSON-serialized 2d-array
 * arrayExchangeRates[0] is an array of strings stating the currencies which are being converted
 * arrayExchangeRates[1] is an array of strings stating the conversion rate of the corresponding currency-pairs
 */

const processExchangeRates = (exchangeRates) => {
    let arrayExchangeRates = JSON.parse(exchangeRates);
    let arraySize = arrayExchangeRates[0].length;
    for(let i = 0; i < arraySize; i++) {
        console.log(arrayExchangeRates[0][i], arrayExchangeRates[1][i]);
    }
}

const processInputs = () => {
	console.log("Begin processing input");
	$.when(downloadAndSaveMarketData(), downloadAndSaveExchangeRates()).done(function(responseOne, responseTwo){
		let marketData = responseOne[0];
		processMarketData(marketData);

		let exchangeRates = responseTwo[0];
		processExchangeRates(exchangeRates);

		let inputA = parseFloat($('#textInputA').val());

        $.getJSON('/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent',
            {
				MoDS_input: JSON.stringify([inputA])
            },
            function(data){
                $('#MoDSOutput').text(data);
            });
	})
}

// $(document).ready(function(){
//     processInputs();
// });

//API to fetch historical data of Bitcoin Price Index
//const api = 'https://api.coindesk.com/v1/bpi/historical/close.json?start=2017-12-31&end=2018-04-01';

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
    for (let i in arrayXVal) {
    	let yVal = +arrayYVal[i];
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

