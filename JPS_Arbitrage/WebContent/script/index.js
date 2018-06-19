/**
 * 
 */
const storeUtilityPricesInKnowledgeBase = arrayHeaderPrices => {
	return $.getJSON('/JPS_Arbitrage/savingDataInTheKnowledgeBase', {
		arrayHeaderPrices: JSON.stringify(arrayHeaderPrices)
	});
};

const downloadAndSaveMarketData = () => {
	return $.getJSON('/JPS_Arbitrage/downloadingAndSavingMarketDataInTheKnowledgeBase');
};

//const processMarketData = (marketData) => {
//	console.log("In processMarketData function");
//	console.log(marketData);
//    // -- Process HTTP response in the form of a string -- //
//	let splitStrings = marketData.split('&');
//	splitStrings.shift(); // remove first element which is an empty string
//	let stringSize = splitStrings.length;
//	let i = 0;
//	let parsedDataArray = [];
//
//	console.log(splitStrings);
//
//	while(i < stringSize) {
//
//		let stringHeaderAndXVal = splitStrings[i];
//		let stringDatetimeAndYVal = splitStrings[i+1];
//
//        // -- Splits individual strings which are comma-separated into array of strings -- //
//        let arrayHeaderAndXVal = stringHeaderAndXVal.split(',');
//        let arrayDatetimeAndYVal = stringDatetimeAndYVal.split(',');
//
//        // -- Separates arrays into its constituents -- //
//        let arrayHeader = arrayHeaderAndXVal.slice(0, 4);
//        let arrayXVal = arrayHeaderAndXVal.slice(4);
//
//        let arrayDatetime = arrayDatetimeAndYVal.slice(0, 4);
//        let arrayYVal = arrayDatetimeAndYVal.slice(4);
//
//
//		// -- arrayDatetime and arrayXVal must have equal length i.e. each datetime must have a corresponding xval -- //
//		if (arrayYVal.length > 0 && arrayXVal.length > 0) {
//			let parsedData = parseData(arrayYVal, arrayXVal);
//			parsedDataArray.push(parsedData);
//		} else {
//			console.log("NO MARKET DATA FOR CPO RETRIEVED");
//		}
//
//        i = i + 2;
//    }
//
//    drawChart(parsedDataArray);
//};

const downloadAndSaveExchangeRates = () => {
	return $.getJSON('/JPS_Arbitrage/downloadingAndSavingExchangeRatesInTheKnowledgeBase');
};

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
};

const retrieveSelectedPlantParams = () => {
	let plantSelectionElement = $("#plantSelection");
	let choicePlant = plantSelectionElement.find("option:selected").text();

	// Alternative to the two lines above
	// let choicePlant = $("#plantSelection option:selected").text();

	let cpoElement = document.getElementById("cpo");

	if (choicePlant == "Biodiesel") {
		if (cpoElement.style.display === "none") {
			cpoElement.style.display = "block";
		}
	} else {
		if (cpoElement.style.display === "block") {
			cpoElement.style.display = "none";
		}
	}
};

const processInputs = () => {
	console.log("Begin processing input");
	
	let inputFlowRateCPO = $('input#flowrateCPO').val();	
	let inputPriceCoolingWater = $('input#priceCoolingWater').val();
	let inputPriceFuelGas = $('input#priceFuelGas').val();
	let inputPriceElectricity = $('input#priceElectricity').val();
	
	let pattern = /^\d+(\.\d+)?$/;

	if (pattern.test(inputFlowRateCPO) && 
		pattern.test(inputPriceCoolingWater) && 
		pattern.test(inputPriceFuelGas) && 
		pattern.test(inputPriceElectricity)) {

		let header = ["V_Price_CoolingWater_001", "V_Price_FuelGas_001", "V_Price_Electricity_001"];
		let prices = [inputPriceCoolingWater, inputPriceFuelGas, inputPriceElectricity];
		arrayHeaderPrices = [header, prices];
		
//		$.when(downloadAndSaveMarketData()).done(function(response){
//			console.log(JSON.parse(response));
////			// have to loop and then json parse again
//			let arrayObj = JSON.parse(response);
//			for (let i = 0; i < arrayObj.length; i++) {
//				console.log(JSON.parse(arrayObj[i]));
//			}
//		})
//		
//		$.when(downloadAndSaveExchangeRates()).done(function(response){
//			console.log(response);
//			console.log(JSON.parse(response));
//		})
		
		$.when(downloadAndSaveMarketData(), downloadAndSaveExchangeRates(), storeUtilityPricesInKnowledgeBase(arrayHeaderPrices)).done(function(responseOne, responseTwo, responseThree){
			let marketData = responseOne[0];
//			processMarketData(marketData);
			console.log(marketData);
		
			let exchangeRates = responseTwo[0];
//			processExchangeRates(exchangeRates);
			console.log(exchangeRates);
			
			let storeUtilityPricesInKnowledgeBaseResults = responseThree[0];
			console.log(storeUtilityPricesInKnowledgeBaseResults);
		
			let inputA = parseFloat($('#flowrateCPO').val());
			let choiceAnalysis = $("#analysisSelection option:selected").text();
			
			if(choiceAnalysis === "Market Data from DataDownload Agent") {
				console.log("Running Arbitrage Analysis");
		        $.getJSON('/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent',
		            {
		                MoDS_input: JSON.stringify([inputA])
		            },
		            function (data) {
		            	let modsAnalysisResults = JSON.parse(data);
		            	let textModsAnalysisResults = "The highest marginal profit per tonne of biodiesel FAME is " + 
		            		modsAnalysisResults['marginal profit per tonne of biodiesel FAME (in USD)'] + " USD. The futures contracts " +
		            				"need to be accepted at the following ratio of reagent to product: " +
		            		modsAnalysisResults['ratio of reagent to product'] + ". Buy crude palm oil futures contracts with delivery in " +
		            		modsAnalysisResults['month to buy crude palm oil futures contracts'] + " and sell biodiesel FAME futures contracts with delivery in " +
		            		modsAnalysisResults['month to sell biodiesel FAME futures contract'] + ". " + modsAnalysisResults['note'];
		            		
		                $('#MoDSOutput').text(textModsAnalysisResults);
//		            	$('#MoDSOutput').text(data);
		            });
		    } else {
		        $.getJSON('/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles',
		            {
		                MoDS_input: JSON.stringify([inputA])
		            },
		            function (data) {
		                $('#MoDSOutput').text(data);
		            });
			}
		})
	} else {
		console.log("Please fill in ALL parameters with positive real numbers.")
	}
};

$(document).ready(function(){
    let cpoElement = document.getElementById("cpo");
	cpoElement.style.display = "block";

	$.getJSON('/JPS_Arbitrage/retrieveUtilityPrices',
			{
				individuals: "V_Price_CoolingWater_001,V_Price_FuelGas_001,V_Price_Electricity_001"
			},
			function(data){
				let dataObj = JSON.parse(data);
				$('input#priceCoolingWater').val(dataObj['V_Price_CoolingWater_001']);
				$('input#priceFuelGas').val(dataObj['V_Price_FuelGas_001']);
				$('input#priceElectricity').val(dataObj['V_Price_Electricity_001']);
			});
//	$.getJSON('/JPS_Arbitrage/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase',
//			{
//				individuals: "V_Price_CoolingWater_001,V_Price_Storage_Biodiesel_001,V_Price_Storage_CrudePalmOil_001,V_Price_Transport_Malaysia-SG_CrudePalmOil_001,V_Price_Electricity_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_HighPressureSteam_001,V_Price_MediumPressureSteam_001,V_Price_Transport_SEA-SC_Biodiesel_001,V_Price_FuelGas_001"
//			},
//			function(data){
//				console.log(data);
//			});
});

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
function drawChart(parsedDataArray) {
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
		.y(function(d) { return y(d.value)});

	// x.domain(d3.extent(parsedDataArray[0], function(d) { return d.date }));
	// y.domain(d3.extent(parsedDataArray[0], function(d) { return d.value }));
	x.domain(d3.extent(parsedDataArray[0], function(d) { return d.date }));
	y.domain([0,3000]);

	console.log("Parsed Data Array:");
	console.log(parsedDataArray);

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

	// g.append("path")
	// 	.datum(parsedDataArray[0])
	// 	.attr("fill", "none")
	// 	.attr("stroke", "steelblue")
	// 	.attr("stroke-linejoin", "round")
	// 	.attr("stroke-linecap", "round")
	// 	.attr("stroke-width", 1.5)
	// 	.attr("d", line);

	g.selectAll('.graph1')
		.data(parsedDataArray[0])
		.enter()
		.append('path')
		.attr('class', 'graph1')
		.attr('d', line(parsedDataArray[0]));
}