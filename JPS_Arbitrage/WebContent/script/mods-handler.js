import { storeUtilityPricesInKnowledgeBase } from "./utilities-handler.js";
import { downloadAndSaveMarketData, consoleLogDownloadAndSaveMarketData, processMarketData} from "./marketdata-handler.js";
import { downloadAndSaveExchangeRates, consoleLogDownloadAndSaveExchangeRates, processExchangeRates } from "./exchangerates-handler.js";

const processInputs = (evt) => {
    console.log("Begin processing input");

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

        let header = ["V_Price_CoolingWater_001", "V_Price_FuelGas_001", "V_Price_Electricity_001"];
        let prices = [inputPriceCoolingWater, inputPriceFuelGas, inputPriceElectricity];
        let arrayHeaderPrices = [header, prices];

        $.when(downloadAndSaveMarketData(choicePlant), downloadAndSaveExchangeRates(), storeUtilityPricesInKnowledgeBase(arrayHeaderPrices)).done(function(responseOne, responseTwo, responseThree){
            let marketData = responseOne[0];
//			consoleLogDownloadAndSaveMarketData(marketData);
            processMarketData(marketData);

            let exchangeRates = responseTwo[0];
            // consoleLogDownloadAndSaveExchangeRates(exchangeRates);
            // processExchangeRates(exchangeRates);

            let storeUtilityPricesInKnowledgeBaseResults = responseThree[0];

            if(choicePlant === "Biodiesel") {
                $.getJSON('/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent',
                {
                    MoDS_input: JSON.stringify([inputA]),
                    choicePlant
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
                $.getJSON('/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent',
                {
                    MoDS_input: JSON.stringify([inputA]),
                    choicePlant
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
        })
    } else {
        console.log("Please fill in ALL parameters with positive real numbers.")
    }
    evt.preventDefault();
};

export { processInputs };