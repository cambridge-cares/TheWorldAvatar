import { retrieveSelectedPlantParams } from "./plantparams-handler.js";
import { processInputs } from "./mods-handler.js";

$(document).ready(function(){

	document.getElementById("plantSelection").addEventListener("change", retrieveSelectedPlantParams);
	document.getElementById("startSimButton").addEventListener("click", processInputs, false);

	$.getJSON('/JPS_ARBITRAGE/retrieveUtilityPrices',
		{
			individuals: "V_Price_CoolingWater_001,V_Price_FuelGas_001,V_Price_Electricity_001"
		},
		function(data){
			let dataObj = JSON.parse(data);
			$('input#priceCoolingWater').val(dataObj['V_Price_CoolingWater_001']);
			$('input#priceFuelGas').val(dataObj['V_Price_FuelGas_001']);
			$('input#priceElectricity').val(dataObj['V_Price_Electricity_001']);
		});

    $.getJSON('/JPS_ARBITRAGE/retrievePlantSpecificParam',
        {
            individuals: "V_massF_CrudePalmOilInput_001"
        }, data => {
            let dataObj = JSON.parse(data);
            $("#plantSpecificParam").val(dataObj['V_massF_CrudePalmOilInput_001']);
        });
});