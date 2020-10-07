const downloadAndSaveMarketData = (choicePlant) => {
    return $.getJSON('/JPS_ARBITRAGE/downloadingAndSavingMarketDataInTheKnowledgeBase', {
        choicePlant
    });
};

const consoleLogDownloadAndSaveMarketData = response => {
    let arrayObj = JSON.parse(response);
    for (let i = 0; i < arrayObj.length; i++) {
        console.log(JSON.parse(arrayObj[i]));
    }
};

const processMarketData = (marketData) => {
    let arrayParsedMarketDataObj = []
	let marketDataObj = JSON.parse(marketData);

	for (let i = 0; i < marketDataObj.length; i++) {
	    marketDataObj[i] = JSON.parse(marketDataObj[i]);

	    let label = marketDataObj[i]['arrayHeader'][0];

	    let template = marketDataObj[i]['arrayDatetime'][1];
	    let price = "Price " + template.substring(template.indexOf("("));

	    let marketData = parseData(marketDataObj[i]['arrayPrices'], marketDataObj[i]['arrayMonths']);

	    arrayParsedMarketDataObj.push({
            label,
            price,
            marketData
        })
    }
	let svgNodeList = document.getElementsByClassName("line-chart");
    for (let i = 0; i < svgNodeList.length; i++) {
        while(svgNodeList[i].firstChild) {
            svgNodeList[i].removeChild(svgNodeList[i].firstChild);
        }
    }

    drawChart(arrayParsedMarketDataObj);
};

/**
* Parse data into key-value pairs
* @param {object} data Object containing historical data of BPI
*/
const parseData = (arrayYVal, arrayXVal) => {
  var arr = [];
  for (let i in arrayXVal) {
  	// let yVal = +arrayYVal[i];
    let yVal;

    if (arrayYVal[i] === '-'){
        yVal = 0;
    } else {
        // yVal = parseFloat(y);
        yVal = +arrayYVal[i];
    }

    arr.push({
          date: new Date(arrayXVal[i]), //date
          value: yVal //convert string to number
      });
  }
  return arr;
};

/**
 * Creates a chart using D3
 * @param {object} data Object containing historical data of BPI
 */
const drawChart = data => {

	d3.selectAll("svg")
      .each(function(d, i){
        let svgWidth = 600, svgHeight = 350; // initially 600, 400
        let margin = {top: 20, right: 20, bottom: 30, left: 50};
        let width = svgWidth - margin.left - margin.right;
        let height = svgHeight - margin.top - margin.bottom;

        let marketData = data[i]['marketData'];
        let price = data[i]['price'];
        let label = data[i]['label'];

		let svg = d3.select(this)
			.attr("width", svgWidth)
			.attr("height", svgHeight);

        let g = svg.append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        let x = d3.scaleTime()
            .rangeRound([0, width]);

        let y = d3.scaleLinear()
            .rangeRound([height, 0]);

        let line = d3.line()
            .x(function (d) {
                return x(d.date)
            })
            .y(function (d) {
                return y(d.value)
            });

        y.domain(d3.extent(marketData, function (d) {
            return d.value
        }));
        x.domain(d3.extent(marketData, function (d) {
            return d.date
        }));
        // y.domain([0,3000]);

        // gridlines in x axis function
        function make_x_gridlines() {
            return d3.axisBottom(x)
                .ticks(5)
        }

        // gridlines in y axis function
        function make_y_gridlines() {
            return d3.axisLeft(y)
                .ticks(5)
        }

        // add the X gridlines
        g.append("g")
         .attr("class", "grid")
         .attr("transform", "translate(0," + height + ")")
         .call(make_x_gridlines()
            .tickSize(-height)
            .tickFormat("")
         );

        // add the Y gridlines
        g.append("g")
         .attr("class", "grid")
         .call(make_y_gridlines()
            .tickSize(-width)
            .tickFormat("")
         );

        g.append("g")
            .attr("transform", "translate(0," + height + ")")
            .call(d3.axisBottom(x))
            .select(".domain")
            .remove();

        g.append("g")
            .call(d3.axisLeft(y))
            .append("text")
            .attr("fill", "#000")
            .attr("transform", "rotate(-90)")
            .attr("y", 6)
            .attr("dy", "0.71em")
            .attr("text-anchor", "end")
            .text(price);

        g.append("path")
            .datum(marketData)
            .attr("fill", "none")
            .attr("stroke", "steelblue")
            .attr("stroke-linejoin", "round")
            .attr("stroke-linecap", "round")
            .attr("stroke-width", 1.5)
            .attr("d", line);

        g.append("text")
            .attr("transform", "translate(" + (width+3) + "," + y(marketData[marketData.length - 1].value) + ")")
            .attr("dy", ".35em")
            .attr("text-anchor", "start")
            .style("fill", "steelblue")
            .text(label);
    });
};

export { downloadAndSaveMarketData, consoleLogDownloadAndSaveMarketData, processMarketData };