const retrieveSelectedPlantParams = () => {
    let plantSelectionElement = $("#plantSelection");
    let choicePlant = plantSelectionElement.find("option:selected").text();
    // Alternative to the two lines above
    // let choicePlant = $("#plantSelection option:selected").text();

    if (choicePlant === "Biodiesel") {
        $("#labelPlantSpecificParam").find("span").text("Flow-rate of Crude Palm Oil (kg/hr):");
        $("#plantSpecificParam").val("");
        $.getJSON('/JPS_ARBITRAGE/retrievePlantSpecificParam',
            {
                individuals: "V_massF_CrudePalmOilInput_001"
            }, data => {
                let dataObj = JSON.parse(data);
                $("#plantSpecificParam").val(dataObj['V_massF_CrudePalmOilInput_001']);
            }).fail(function(){
				$("#plantSpecificParam").val("24220.0656");
			});
    } else {
        $("#labelPlantSpecificParam").find("span").text("Flow-rate of Natural Gas (kmol/s):");
        $("#plantSpecificParam").val("");
        $.getJSON('/JPS_ARBITRAGE/retrievePlantSpecificParam',
            {
                individuals: "V_moleF_NaturalGasInput_001"
            }, data => {
                let dataObj = JSON.parse(data);
                $("#plantSpecificParam").val(dataObj['V_moleF_NaturalGasInput_001']);
            }).fail(function(){
				$("#plantSpecificParam").val("0.5527777778");
	
			});
    }
};

export { retrieveSelectedPlantParams };