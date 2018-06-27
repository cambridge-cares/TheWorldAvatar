const storeUtilityPricesInKnowledgeBase = arrayHeaderPrices => {
    return $.getJSON('/JPS_ARBITRAGE/savingDataInTheKnowledgeBase', {
        arrayHeaderPrices: JSON.stringify(arrayHeaderPrices)
    });
};

export { storeUtilityPricesInKnowledgeBase };