const storePricesInKnowledgeBase = arrayHeaderPrices => {
    return $.getJSON('/JPS_ARBITRAGE/savingDataInTheKnowledgeBase', {
        arrayHeaderPrices: JSON.stringify(arrayHeaderPrices)
    });
};

export { storePricesInKnowledgeBase };