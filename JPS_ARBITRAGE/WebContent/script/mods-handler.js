import { storePricesInKnowledgeBase } from "./utilities-handler.js";
import { downloadAndSaveMarketData, processMarketData} from "./marketdata-handler.js";
import { downloadAndSaveExchangeRates } from "./exchangerates-handler.js";

const processInputs = (evt) => {
    console.log("Begin processing input");
	
	$("#analysis").append('<img id="myProgressBar" style="width:100px;height:100px;" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/>')

    let inputPlantSpecificParam = $('input#plantSpecificParam').val();
    let inputPriceCoolingWater = $('input#priceCoolingWater').val();
    let inputPriceFuelGas = $('input#priceFuelGas').val();
    let inputPriceElectricity = $('input#priceElectricity').val();

    let inputA = parseFloat($('#plantSpecificParam').val());

    let choicePlant = $("#plantSelection option:selected").text();

    let pattern = /^\d+(\.\d+)?$/;

    if (pattern.test(inputPlantSpecificParam) &&
        pattern.test(inputPriceCoolingWater) &&
        pattern.test(inputPriceFuelGas) &&
        pattern.test(inputPriceElectricity) &&
        pattern.test(inputA)) {

        let headerUtilParam = ["V_Price_CoolingWater_001", "V_Price_FuelGas_001", "V_Price_Electricity_001"];
        let pricesUtilParam = [inputPriceCoolingWater, inputPriceFuelGas, inputPriceElectricity, inputPlantSpecificParam];

        if(choicePlant === "Biodiesel") {
            headerUtilParam.push("V_massF_CrudePalmOilInput_001");
        } else if (choicePlant === "Methanol") {
            headerUtilParam.push("V_moleF_NaturalGasInput_001");
        }
        let arrayHeaderPricesUtilParam = [headerUtilParam, pricesUtilParam];

        $.when(downloadAndSaveMarketData(choicePlant),
            downloadAndSaveExchangeRates(),
            storePricesInKnowledgeBase(arrayHeaderPricesUtilParam))
            .done(function(responseOne){

                let marketData = responseOne[0];
                processMarketData(marketData);

			
				
				console.log('Mission accomplished')
				
				$('#myProgressBar').remove()
				
				
				
                if(choicePlant === "Biodiesel") {
                    $.getJSON('/JPS_ARBITRAGE/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent',
                    {
                        MoDS_input: JSON.stringify([inputA]),
                        choicePlant
                    },
                    function (data) {
                        let modsAnalysisResults = JSON.parse(data);
                        let textModsAnalysisResults = "The highest marginal profit per tonne of biodiesel FAME is " +
                            parseFloat(modsAnalysisResults['marginal profit per tonne of biodiesel FAME (in USD)']).toFixed(2) + 
                            " USD. The futures contracts need to be accepted at the following ratio of reagent to product: " +
                            parseFloat(modsAnalysisResults['ratio of reagent to product']).toFixed(2) + 
                            ". Buy crude palm oil futures contracts with delivery in " +
                            modsAnalysisResults['month to buy crude palm oil futures contracts'] + 
                            " and sell biodiesel FAME futures contracts with delivery in " +
                            modsAnalysisResults['month to sell biodiesel FAME futures contract'] + 
                            ". " +
                            modsAnalysisResults['note'];

                        $('#MoDSOutput').text(textModsAnalysisResults);
                    });
                } else if (choicePlant === "Methanol") {
                    $.getJSON('/JPS_ARBITRAGE/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent',
                    {
                        MoDS_input: JSON.stringify([inputA]),
                        choicePlant
                    },
                    function (data) {
                        let modsAnalysisResults = JSON.parse(data);
                        let textModsAnalysisResults = "The highest marginal profit per tonne of methanol is " +
                            parseFloat(modsAnalysisResults['marginal profit per tonne of methanol (in USD)']).toFixed(2) +
                            " USD. The futures contracts need to be accepted at the following ratio of reagent to product: " +
                            parseFloat(modsAnalysisResults['ratio of reagent to product']).toFixed(2) +
                            ". Buy natural gas futures contracts with delivery in " +
                            modsAnalysisResults['month to buy natural gas futures contracts'] +
                            " and sell methanol futures contracts with delivery in " +
                            modsAnalysisResults['month to sell methanol futures contract'] +
                            ". " +
                            modsAnalysisResults['note'];

                        $('#MoDSOutput').text(textModsAnalysisResults);
                    });
                }
            })
    } else {
        console.log("Please fill in ALL parameters with positive real numbers.")
    }
    evt.preventDefault();
};

export { processInputs };