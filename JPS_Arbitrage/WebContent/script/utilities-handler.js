const storeUtilityPricesInKnowledgeBase = arrayHeaderPrices => {
    return $.getJSON('/JPS_Arbitrage/savingDataInTheKnowledgeBase', {
        arrayHeaderPrices: JSON.stringify(arrayHeaderPrices)
    });
};

export { storeUtilityPricesInKnowledgeBase };