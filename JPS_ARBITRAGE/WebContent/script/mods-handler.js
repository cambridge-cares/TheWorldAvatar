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
            });
    } else {
        console.log("Please fill in ALL parameters with positive real numbers.")
    }
    evt.preventDefault();
};

const replaceResults = (evt) => {
	 let choicePlant = $("#plantSelection option:selected").text();
	let marketDataObj,textModsAnalysisResults  = null ;
	if (choicePlant === "Biodiesel") {
		
		$('#myProgressBar').remove()
		marketDataObj = "[\"{\\\"arrayHeader\\\": [\\\"CPO\\\", \\\"Date\\\", \\\"Price type\\\", \\\"Size (tonne)\\\"], \\\"arrayMonths\\\": [\\\"JAN 2021\\\", \\\"FEB 2021\\\", \\\"MAR 2021\\\", \\\"APR 2021\\\", \\\"MAY 2021\\\", \\\"JUN 2021\\\", \\\"JUL 2021\\\", \\\"AUG 2021\\\", \\\"SEP 2021\\\", \\\"OCT 2021\\\", \\\"NOV 2021\\\", \\\"DEC 2021\\\", \\\"JAN 2022\\\", \\\"FEB 2022\\\", \\\"MAR 2022\\\", \\\"APR 2022\\\", \\\"MAY 2022\\\", \\\"JUN 2022\\\", \\\"JUL 2022\\\", \\\"AUG 2022\\\", \\\"SEP 2022\\\", \\\"OCT 2022\\\", \\\"NOV 2022\\\", \\\"DEC 2022\\\", \\\"JAN 2023\\\", \\\"FEB 2023\\\", \\\"MAR 2023\\\", \\\"APR 2023\\\", \\\"MAY 2023\\\", \\\"JUN 2023\\\", \\\"JUL 2023\\\", \\\"AUG 2023\\\", \\\"SEP 2023\\\", \\\"OCT 2023\\\", \\\"NOV 2023\\\", \\\"DEC 2023\\\", \\\"JAN 2024\\\", \\\"FEB 2024\\\", \\\"MAR 2024\\\", \\\"APR 2024\\\", \\\"MAY 2024\\\", \\\"JUN 2024\\\", \\\"JUL 2024\\\", \\\"AUG 2024\\\", \\\"SEP 2024\\\", \\\"OCT 2024\\\", \\\"NOV 2024\\\", \\\"DEC 2024\\\", \\\"JAN 2025\\\", \\\"FEB 2025\\\", \\\"MAR 2025\\\", \\\"APR 2025\\\", \\\"MAY 2025\\\", \\\"JUN 2025\\\", \\\"JUL 2025\\\", \\\"AUG 2025\\\", \\\"SEP 2025\\\", \\\"OCT 2025\\\", \\\"NOV 2025\\\", \\\"DEC 2025\\\", \\\"JAN 2026\\\"], \\\"arrayDatetime\\\": [\\\"Thu, 21 Jan 2021 08:43:42 GMT\\\", \\\"Prior Settlement (USD per tonne)\\\", \\\"25.0\\\"], \\\"arrayPrices\\\": [\\\"868.25\\\", \\\"798.25\\\", \\\"782.75\\\", \\\"769.00\\\", \\\"755.50\\\", \\\"740.75\\\", \\\"726.25\\\", \\\"717.00\\\", \\\"712.25\\\", \\\"712.25\\\", \\\"709.75\\\", \\\"702.50\\\", \\\"692.25\\\", \\\"686.75\\\", \\\"687.25\\\", \\\"678.50\\\", \\\"673.50\\\", \\\"672.50\\\", \\\"671.75\\\", \\\"670.50\\\", \\\"669.50\\\", \\\"669.00\\\", \\\"667.75\\\", \\\"667.00\\\", \\\"666.50\\\", \\\"664.75\\\", \\\"664.25\\\", \\\"662.75\\\", \\\"662.25\\\", \\\"660.75\\\", \\\"660.00\\\", \\\"659.50\\\", \\\"539.25\\\", \\\"547.00\\\", \\\"508.25\\\", \\\"508.75\\\", \\\"541.25\\\", \\\"546.50\\\", \\\"573.75\\\", \\\"562.50\\\", \\\"560.00\\\", \\\"554.50\\\", \\\"575.50\\\", \\\"552.50\\\", \\\"553.50\\\", \\\"572.75\\\", \\\"613.75\\\", \\\"621.50\\\", \\\"605.75\\\", \\\"593.75\\\", \\\"525.50\\\", \\\"514.25\\\", \\\"508.25\\\", \\\"543.75\\\", \\\"573.00\\\", \\\"573.75\\\", \\\"598.75\\\", \\\"596.75\\\", \\\"696.00\\\", \\\"678.00\\\", \\\"0.00\\\"]}\",\"{\\\"arrayHeader\\\": [\\\"FAME\\\", \\\"Date\\\", \\\"Price type\\\", \\\"Size (tonne)\\\"], \\\"arrayMonths\\\": [\\\"JAN 2021\\\", \\\"FEB 2021\\\", \\\"MAR 2021\\\", \\\"APR 2021\\\", \\\"MAY 2021\\\", \\\"JUN 2021\\\", \\\"JUL 2021\\\", \\\"AUG 2021\\\", \\\"SEP 2021\\\", \\\"OCT 2021\\\", \\\"NOV 2021\\\", \\\"DEC 2021\\\", \\\"JAN 2022\\\", \\\"FEB 2022\\\", \\\"MAR 2022\\\", \\\"APR 2022\\\", \\\"MAY 2022\\\", \\\"JUN 2022\\\", \\\"JUL 2022\\\", \\\"AUG 2022\\\", \\\"SEP 2022\\\", \\\"OCT 2022\\\", \\\"NOV 2022\\\", \\\"DEC 2022\\\", \\\"JAN 2023\\\"], \\\"arrayDatetime\\\": [\\\"Thu, 21 Jan 2021 08:43:58 GMT\\\", \\\"Prior Settlement (USD per tonne)\\\", \\\"100.0\\\"], \\\"arrayPrices\\\": [\\\"1097.950\\\", \\\"1060.850\\\", \\\"1055.391\\\", \\\"1056.357\\\", \\\"1051.667\\\", \\\"1052.432\\\", \\\"1008.602\\\", \\\"1009.477\\\", \\\"1010.432\\\", \\\"935.417\\\", \\\"934.295\\\", \\\"933.924\\\", \\\"934.167\\\", \\\"934.250\\\", \\\"934.250\\\", \\\"934.088\\\", \\\"933.682\\\", \\\"934.182\\\", \\\"935.167\\\", \\\"935.989\\\", \\\"936.932\\\", \\\"936.250\\\", \\\"934.557\\\", \\\"934.667\\\", \\\"0.000\\\"]}\"]";
		textModsAnalysisResults = "The highest marginal profit per tonne of biodiesel FAME is 242.19 USD. The futures contracts need to be accepted at the following ratio of reagent to product: 3.98. Buy crude palm oil futures contracts with delivery in JUL 2021 and sell biodiesel FAME futures contracts with delivery in AUG 2021. Note that all crude palm oil was stored and instantaneously converted on delivery date."
		
		}else if (choicePlant === "Methanol") {
		marketDataObj = "[\"{\\\"arrayHeader\\\": [\\\"MeOH\\\", \\\"Date\\\", \\\"Price type\\\", \\\"Size (tonne)\\\"], \\\"arrayMonths\\\": [\\\"FEB 2011\\\", \\\"MAR 2011\\\", \\\"APR 2011\\\", \\\"MAY 2011\\\", \\\"JUN 2011\\\", \\\"JUL 2011\\\", \\\"AUG 2011\\\", \\\"SEP 2011\\\", \\\"OCT 2011\\\", \\\"NOV 2011\\\", \\\"DEC 2011\\\", \\\"JAN 2012\\\"], \\\"arrayDatetime\\\": [\\\"Thu, 21 Jan 2021 08:59:33 GMT\\\", \\\"Prior Settlement (CNY per tonne)\\\", \\\"1.0\\\"], \\\"arrayPrices\\\": [\\\"2387.0\\\", \\\"2378.0\\\", \\\"2328.0\\\", \\\"2293.0\\\", \\\"2281.0\\\", \\\"2272.0\\\", \\\"2278.0\\\", \\\"2272.0\\\", \\\"2326.0\\\", \\\"2313.0\\\", \\\"2361.0\\\", \\\"2354.0\\\"]}\",\"{\\\"arrayHeader\\\": [\\\"NG\\\", \\\"Date\\\", \\\"Price type\\\", \\\"Size (mmBTU)\\\"], \\\"arrayMonths\\\": [\\\"FEB 2021\\\", \\\"MAR 2021\\\", \\\"APR 2021\\\", \\\"MAY 2021\\\", \\\"JUN 2021\\\", \\\"JUL 2021\\\", \\\"AUG 2021\\\", \\\"SEP 2021\\\", \\\"OCT 2021\\\", \\\"NOV 2021\\\", \\\"DEC 2021\\\", \\\"JAN 2022\\\", \\\"FEB 2022\\\", \\\"MAR 2022\\\", \\\"APR 2022\\\", \\\"MAY 2022\\\", \\\"JUN 2022\\\", \\\"JUL 2022\\\", \\\"AUG 2022\\\", \\\"SEP 2022\\\", \\\"OCT 2022\\\", \\\"NOV 2022\\\", \\\"DEC 2022\\\", \\\"JAN 2023\\\", \\\"FEB 2023\\\", \\\"MAR 2023\\\", \\\"APR 2023\\\", \\\"MAY 2023\\\", \\\"JUN 2023\\\", \\\"JUL 2023\\\", \\\"AUG 2023\\\", \\\"SEP 2023\\\", \\\"OCT 2023\\\", \\\"NOV 2023\\\", \\\"DEC 2023\\\", \\\"JAN 2024\\\", \\\"FEB 2024\\\", \\\"MAR 2024\\\", \\\"APR 2024\\\", \\\"MAY 2024\\\", \\\"JUN 2024\\\", \\\"JUL 2024\\\", \\\"AUG 2024\\\", \\\"SEP 2024\\\", \\\"OCT 2024\\\", \\\"NOV 2024\\\", \\\"DEC 2024\\\", \\\"JAN 2025\\\", \\\"FEB 2025\\\", \\\"MAR 2025\\\", \\\"APR 2025\\\", \\\"MAY 2025\\\", \\\"JUN 2025\\\", \\\"JUL 2025\\\", \\\"AUG 2025\\\", \\\"SEP 2025\\\", \\\"OCT 2025\\\", \\\"NOV 2025\\\", \\\"DEC 2025\\\", \\\"JAN 2026\\\", \\\"FEB 2026\\\", \\\"MAR 2026\\\", \\\"APR 2026\\\", \\\"MAY 2026\\\", \\\"JUN 2026\\\", \\\"JUL 2026\\\", \\\"AUG 2026\\\", \\\"SEP 2026\\\", \\\"OCT 2026\\\", \\\"NOV 2026\\\", \\\"DEC 2026\\\", \\\"JAN 2027\\\", \\\"FEB 2027\\\", \\\"MAR 2027\\\", \\\"APR 2027\\\", \\\"MAY 2027\\\", \\\"JUN 2027\\\", \\\"JUL 2027\\\", \\\"AUG 2027\\\", \\\"SEP 2027\\\", \\\"OCT 2027\\\", \\\"NOV 2027\\\", \\\"DEC 2027\\\", \\\"JAN 2028\\\", \\\"FEB 2028\\\", \\\"MAR 2028\\\", \\\"APR 2028\\\", \\\"MAY 2028\\\", \\\"JUN 2028\\\", \\\"JUL 2028\\\", \\\"AUG 2028\\\", \\\"SEP 2028\\\", \\\"OCT 2028\\\", \\\"NOV 2028\\\", \\\"DEC 2028\\\", \\\"JAN 2029\\\", \\\"FEB 2029\\\", \\\"MAR 2029\\\", \\\"APR 2029\\\", \\\"MAY 2029\\\", \\\"JUN 2029\\\", \\\"JUL 2029\\\", \\\"AUG 2029\\\", \\\"SEP 2029\\\", \\\"OCT 2029\\\", \\\"NOV 2029\\\", \\\"DEC 2029\\\", \\\"JAN 2030\\\", \\\"FEB 2030\\\", \\\"MAR 2030\\\", \\\"APR 2030\\\", \\\"MAY 2030\\\", \\\"JUN 2030\\\", \\\"JUL 2030\\\", \\\"AUG 2030\\\", \\\"SEP 2030\\\", \\\"OCT 2030\\\", \\\"NOV 2030\\\", \\\"DEC 2030\\\", \\\"JAN 2031\\\", \\\"FEB 2031\\\", \\\"MAR 2031\\\", \\\"APR 2031\\\", \\\"MAY 2031\\\", \\\"JUN 2031\\\", \\\"JUL 2031\\\", \\\"AUG 2031\\\", \\\"SEP 2031\\\", \\\"OCT 2031\\\", \\\"NOV 2031\\\", \\\"DEC 2031\\\", \\\"JAN 2032\\\", \\\"FEB 2032\\\", \\\"MAR 2032\\\", \\\"APR 2032\\\", \\\"MAY 2032\\\", \\\"JUN 2032\\\", \\\"JUL 2032\\\", \\\"AUG 2032\\\", \\\"SEP 2032\\\", \\\"OCT 2032\\\", \\\"NOV 2032\\\", \\\"DEC 2032\\\", \\\"JAN 2033\\\", \\\"FEB 2033\\\", \\\"MAR 2033\\\", \\\"APR 2033\\\", \\\"MAY 2033\\\", \\\"JUN 2033\\\", \\\"JUL 2033\\\", \\\"AUG 2033\\\", \\\"SEP 2033\\\", \\\"OCT 2033\\\", \\\"NOV 2033\\\", \\\"DEC 2033\\\"], \\\"arrayDatetime\\\": [\\\"Thu, 21 Jan 2021 09:00:08 GMT\\\", \\\"Prior Settlement (USD per mmBTU)\\\", \\\"10.0\\\"], \\\"arrayPrices\\\": [\\\"2.539\\\", \\\"2.533\\\", \\\"2.562\\\", \\\"2.607\\\", \\\"2.672\\\", \\\"2.741\\\", \\\"2.765\\\", \\\"2.755\\\", \\\"2.781\\\", \\\"2.848\\\", \\\"2.980\\\", \\\"3.069\\\", \\\"3.008\\\", \\\"2.851\\\", \\\"2.506\\\", \\\"2.460\\\", \\\"2.484\\\", \\\"2.516\\\", \\\"2.528\\\", \\\"2.515\\\", \\\"2.542\\\", \\\"2.604\\\", \\\"2.750\\\", \\\"2.850\\\", \\\"2.806\\\", \\\"2.667\\\", \\\"2.371\\\", \\\"2.343\\\", \\\"2.377\\\", \\\"2.423\\\", \\\"2.429\\\", \\\"2.413\\\", \\\"2.443\\\", \\\"2.522\\\", \\\"2.714\\\", \\\"2.808\\\", \\\"2.764\\\", \\\"2.633\\\", \\\"2.343\\\", \\\"2.317\\\", \\\"2.352\\\", \\\"2.396\\\", \\\"2.403\\\", \\\"2.400\\\", \\\"2.433\\\", \\\"2.550\\\", \\\"2.766\\\", \\\"2.869\\\", \\\"2.820\\\", \\\"2.680\\\", \\\"2.390\\\", \\\"2.365\\\", \\\"2.399\\\"]}\"]";
		textModsAnalysisResults = "The highest marginal profit per tonne of methanol is -74650.53 USD. The futures contracts need to be accepted at the following ratio of reagent to product: 2.85. Buy natural gas futures contracts with delivery in MAR 2021 and sell methanol futures contracts with delivery in NOV 2021. Note that all natural gas was instantaneously converted on arrival and methanol was stored until delivery date.";
		}
		$('#MoDSOutput').text(textModsAnalysisResults);
		processMarketData(marketDataObj);
    evt.preventDefault();
};
export { processInputs,replaceResults };