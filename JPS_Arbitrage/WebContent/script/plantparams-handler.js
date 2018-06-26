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

export { retrieveSelectedPlantParams };