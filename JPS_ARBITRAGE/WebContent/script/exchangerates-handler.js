const downloadAndSaveExchangeRates = () => {
    return $.getJSON('/JPS_ARBITRAGE/downloadingAndSavingExchangeRatesInTheKnowledgeBase');
};

const consoleLogDownloadAndSaveExchangeRates = response => {
    console.log(JSON.parse(response));
};

/**
 * param exchangeRates is a JSON-serialized 2d-array
 * arrayExchangeRates[0] is an array of strings stating the currencies which are being converted
 * arrayExchangeRates[1] is an array of strings stating the conversion rate of the corresponding currency-pairs
 */

const processExchangeRates = (exchangeRates) => {
    let arrayExchangeRates = JSON.parse(exchangeRates);
    let arraySize = arrayExchangeRates[0].length;
    for(let i = 0; i < arraySize; i++) {
        console.log(arrayExchangeRates[0][i], arrayExchangeRates[1][i]);
    }
};

export { downloadAndSaveExchangeRates, consoleLogDownloadAndSaveExchangeRates, processExchangeRates };