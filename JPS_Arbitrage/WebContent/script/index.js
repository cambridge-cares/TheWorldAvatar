import { retrieveSelectedPlantParams } from "./plantparams-handler.js";
import { processInputs } from "./mods-handler.js";

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