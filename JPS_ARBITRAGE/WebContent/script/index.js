import { retrieveSelectedPlantParams } from "./plantparams-handler.js";
import { processInputs } from "./mods-handler.js";

$(document).ready(function(){
	
	//-----------------------------------------------------------------------------------//
	
	const toggleDisplay = elemId => {
        let x = document.getElementById(elemId);
        if (x.style.display !== 'block') {
            x.style.display = 'block';
        } else {
            x.style.display = 'none';
        }
    };

    $("#readme-button").click(function() {
        toggleDisplay("readme-text");
    });

    document.addEventListener("click", function(evt) {
        var readmeButtonElement = document.getElementById('readme-button'),
            readmeTextElement = document.getElementById('readme-text'),
            targetElement = evt.target;  // clicked element

        if (targetElement == readmeButtonElement || targetElement == readmeTextElement) {
            return; //readme-button or readme-text is clicked. do nothing.
        }

        if(readmeTextElement.style.display === 'block') {
            readmeTextElement.style.display = 'none';
        }
    });
    
    //-----------------------------------------------------------------------------------//

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