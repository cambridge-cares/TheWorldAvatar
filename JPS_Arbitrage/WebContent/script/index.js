import { storeUtilityPricesInKnowledgeBase } from "./utilities-handler.js";
import { downloadAndSaveMarketData, consoleLogDownloadAndSaveMarketData, processMarketData} from "./marketdata-handler.js";
import { downloadAndSaveExchangeRates, consoleLogDownloadAndSaveExchangeRates, processExchangeRates } from "./exchangerates-handler.js";


const retrieveSelectedPlantParams = () => {
	let plantSelectionElement = $("#plantSelection");
	let choicePlant = plantSelectionElement.find("option:selected").text();
	// Alternative to the two lines above
	// let choicePlant = $("#plantSelection option:selected").text();

	if (choicePlant === "Biodiesel") {
		$("#labelPlantSpecificParam").find("span").text("Flow-rate of Crude Palm Oil (kg/hour):");
		$("#plantSpecificParam").val("24220.0656");
	} else {
        $("#labelPlantSpecificParam").find("span").text("Param of Natural Gas:");
        $("#plantSpecificParam").val("0.5527777778");
	}
};

const processInputs = (evt) => {
	console.log("Begin processing input");
	
	let inputPlantSpecificParam = $('input#plantSpecificParam').val();
	let inputPriceCoolingWater = $('input#priceCoolingWater').val();
	let inputPriceFuelGas = $('input#priceFuelGas').val();
	let inputPriceElectricity = $('input#priceElectricity').val();

	let inputA = parseFloat($('#plantSpecificParam').val());

	let choiceAnalysis = $("#analysisSelection option:selected").text();
	let choicePlant = $("#plantSelection option:selected").text();

	let pattern = /^\d+(\.\d+)?$/;

	if (pattern.test(inputPlantSpecificParam) &&
		pattern.test(inputPriceCoolingWater) &&
		pattern.test(inputPriceFuelGas) &&
		pattern.test(inputPriceElectricity) &&
		pattern.test(inputA)) {

		let header = ["V_Price_CoolingWater_001", "V_Price_FuelGas_001", "V_Price_Electricity_001"];
		let prices = [inputPriceCoolingWater, inputPriceFuelGas, inputPriceElectricity];
		let arrayHeaderPrices = [header, prices];

		$.when(downloadAndSaveMarketData(choicePlant), downloadAndSaveExchangeRates(), storeUtilityPricesInKnowledgeBase(arrayHeaderPrices)).done(function(responseOne, responseTwo, responseThree){
			let marketData = responseOne[0];
//			consoleLogDownloadAndSaveMarketData(marketData);
			processMarketData(marketData);

			let exchangeRates = responseTwo[0];
			consoleLogDownloadAndSaveExchangeRates(exchangeRates);
//			processExchangeRates(exchangeRates);

			let storeUtilityPricesInKnowledgeBaseResults = responseThree[0];
			console.log(storeUtilityPricesInKnowledgeBaseResults);


			if(choiceAnalysis === "MoDS") {
				if(choicePlant === "Biodiesel") {
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
                        });
                } else if (choicePlant === "Methanol") {
                    $.getJSON('/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent2',
                        {
                            MoDS_input: JSON.stringify([inputA])
                        },
                        function (data) {
                            let modsAnalysisResults = JSON.parse(data);
                            let textModsAnalysisResults = "The highest marginal profit per tonne of methanol is " +
                                modsAnalysisResults['marginal profit per tonne of methanol (in USD)'] +
                                " USD. The futures contracts need to be accepted at the following ratio" +
                                " of reagent to product: " +
                                modsAnalysisResults['ratio of reagent to product'] +
                                ". Buy natural gas futures contracts with delivery in " +
                                modsAnalysisResults['month to buy natural gas futures contracts'] +
                                " and sell methanol futures contracts with delivery in " +
                                modsAnalysisResults['month to sell methanol futures contract'] +
                                ". " +
                                modsAnalysisResults['note'];

                            $('#MoDSOutput').text(textModsAnalysisResults);
                        });
				}
		    } else {
				console.log("not done yet");
		        // $.getJSON('/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles',
		        //     {
		        //         MoDS_input: JSON.stringify([inputA])
		        //     },
		        //     function (data) {
		        //         $('#MoDSOutput').text(data);
		        //     });
			}
		})
	} else {
		console.log("Please fill in ALL parameters with positive real numbers.")
	}
	evt.preventDefault();
};

$(document).ready(function(){

	document.getElementById("plantSelection").addEventListener("change", retrieveSelectedPlantParams);
	document.getElementById("startSimButton").addEventListener("click", processInputs, false);

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