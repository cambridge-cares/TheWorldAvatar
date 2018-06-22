const downloadAndSaveMarketData = (choicePlant) => {
    return $.getJSON('/JPS_Arbitrage/downloadingAndSavingMarketDataInTheKnowledgeBase', {
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
	let marketDataObj = JSON.parse(marketData);
	for (let i = 0; i < marketDataObj.length; i++) {
	    marketDataObj[i] = JSON.parse(marketDataObj[i]);
    }

    // drawChart(marketDataObj);

    let firstArray = parseData(marketDataObj[0]['arrayPrices'], marketDataObj[0]['arrayMonths']);
	let secondArray = parseData(marketDataObj[1]['arrayPrices'], marketDataObj[1]['arrayMonths']);

	let svgNodeList = document.getElementsByClassName("line-chart");
    for (let i = 0; i < svgNodeList.length; i++) {
        while(svgNodeList[i].firstChild) {
            svgNodeList[i].removeChild(svgNodeList[i].firstChild);
        }
    }

    drawChart([firstArray, secondArray]);
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

// const drawChart = (marketDataObj) => {
//
// 	var svgToClear = document.getElementById("line-chart-output");
// 	while(svgToClear.firstChild) {
// 		svgToClear.removeChild(svgToClear.firstChild);
// 	}
//
//     var data = [
//         {
//             label: marketDataObj[0]['arrayHeader'][0],
//             x: marketDataObj[0]['arrayMonths'].map(x => new Date(x)),
//             y: marketDataObj[0]['arrayPrices'].map(y => {
//             	if (y === '-'){
//             		return 0;
//             	} else {
//             		return parseFloat(y);
//             	}
//             })
//         },
//         {
//             label: marketDataObj[1]['arrayHeader'][0],
//             x: marketDataObj[1]['arrayMonths'].map(x => new Date(x)),
//             y: marketDataObj[1]['arrayPrices'].map(y => {
//             	if (y === '-'){
//             		return 0;
//             	} else {
//             		return parseFloat(y);
//             	}
//             })
//         }
//     ];
//
//     var xy_chart = d3_xy_chart()
//         .width(960)
//         .height(500)
//         .xlabel("Time (in Months)")
//         .ylabel("Prices (in USD)") ;
//
//     var svg = d3.select("svg")
//         .datum(data)
//         .call(xy_chart) ;
//
//     function d3_xy_chart() {
//         var width = 640,
//             height = 480,
//             xlabel = "X Axis Label",
//             ylabel = "Y Axis Label" ;
//
//         function chart(selection) {
//             selection.each(function(datasets) {
//                 //
//                 // Create the plot.
//                 //
//                 var margin = {top: 20, right: 80, bottom: 30, left: 50},
//                     innerwidth = width - margin.left - margin.right,
//                     innerheight = height - margin.top - margin.bottom ;
//
//                 var x_scale = d3.time.scale()
//                     .range([0, innerwidth])
//                     .domain([ d3.min(datasets, function(d) { return d3.min(d.x); }),
//                         d3.max(datasets, function(d) { return d3.max(d.x); }) ]) ;
//
//                 var y_scale = d3.scale.linear()
//                     .range([innerheight, 0])
//                     .domain([ d3.min(datasets, function(d) { return d3.min(d.y); }),
//                         d3.max(datasets, function(d) { return d3.max(d.y); }) ]) ;
//
//                 var color_scale = d3.scale.category10()
//                     .domain(d3.range(datasets.length)) ;
//
//                 var x_axis = d3.svg.axis()
//                     .scale(x_scale)
//                     .orient("bottom") ;
//
//                 var y_axis = d3.svg.axis()
//                     .scale(y_scale)
//                     .orient("left") ;
//
//                 var x_grid = d3.svg.axis()
//                     .scale(x_scale)
//                     .orient("bottom")
//                     .tickSize(-innerheight)
//                     .tickFormat("") ;
//
//                 var y_grid = d3.svg.axis()
//                     .scale(y_scale)
//                     .orient("left")
//                     .tickSize(-innerwidth)
//                     .tickFormat("") ;
//
//                 var draw_line = d3.svg.line()
//                     .interpolate("basis")
//                     .x(function(d) { return x_scale(d[0]); })
//                     .y(function(d) { return y_scale(d[1]); }) ;
//
//                 var svg = d3.select(this)
//                     .attr("width", width)
//                     .attr("height", height)
//                     .append("g")
//                     .attr("transform", "translate(" + margin.left + "," + margin.top + ")") ;
//
//                 svg.append("g")
//                     .attr("class", "x grid")
//                     .attr("transform", "translate(0," + innerheight + ")")
//                     .call(x_grid) ;
//
//                 svg.append("g")
//                     .attr("class", "y grid")
//                     .call(y_grid) ;
//
//                 svg.append("g")
//                     .attr("class", "x axis")
//                     .attr("transform", "translate(0," + innerheight + ")")
//                     .call(x_axis)
//                     .append("text")
//                     .attr("dy", "-.71em")
//                     .attr("x", innerwidth)
//                     .style("text-anchor", "end")
//                     .text(xlabel) ;
//
//                 svg.append("g")
//                     .attr("class", "y axis")
//                     .call(y_axis)
//                     .append("text")
//                     .attr("transform", "rotate(-90)")
//                     .attr("y", 6)
//                     .attr("dy", "0.71em")
//                     .style("text-anchor", "end")
//                     .text(ylabel) ;
//
//                 var data_lines = svg.selectAll(".d3_xy_chart_line")
//                     .data(datasets.map(function(d) {return d3.zip(d.x, d.y);}))
//                     .enter().append("g")
//                     .attr("class", "d3_xy_chart_line") ;
//
//                 data_lines.append("path")
//                     .attr("class", "line")
//                     .attr("d", function(d) {return draw_line(d); })
//                     .attr("stroke", function(_, i) {return color_scale(i);}) ;
//
//                 data_lines.append("text")
//                     .datum(function(d, i) { return {name: datasets[i].label, final: d[d.length-1]}; })
//                     .attr("transform", function(d) {
//                         return ( "translate(" + x_scale(d.final[0]) + "," +
//                             y_scale(d.final[1]) + ")" ) ; })
//                     .attr("x", 3)
//                     .attr("dy", ".35em")
//                     .attr("fill", function(_, i) { return color_scale(i); })
//                     .text(function(d) { return d.name; }) ;
//
//             }) ;
//         }
//
//         chart.width = function(value) {
//             if (!arguments.length) return width;
//             width = value;
//             return chart;
//         };
//
//         chart.height = function(value) {
//             if (!arguments.length) return height;
//             height = value;
//             return chart;
//         };
//
//         chart.xlabel = function(value) {
//             if(!arguments.length) return xlabel ;
//             xlabel = value ;
//             return chart ;
//         } ;
//
//         chart.ylabel = function(value) {
//             if(!arguments.length) return ylabel ;
//             ylabel = value ;
//             return chart ;
//         } ;
//
//         return chart;
//     }
// };

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

		let svg = d3.select(this)
			.attr("width", svgWidth)
			.attr("height", svgHeight);

        var g = svg.append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        var x = d3.scaleTime()
            .rangeRound([0, width]);

        var y = d3.scaleLinear()
            .rangeRound([height, 0]);

        var line = d3.line()
            .x(function (d) {
                return x(d.date)
            })
            .y(function (d) {
                return y(d.value)
            });

        // x.domain(d3.extent(parsedDataArray[0], function(d) { return d.date }));
        y.domain(d3.extent(data[i], function (d) {
            return d.value
        }));
        x.domain(d3.extent(data[i], function (d) {
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
         )

        // add the Y gridlines
        g.append("g")
         .attr("class", "grid")
         .call(make_y_gridlines()
            .tickSize(-width)
            .tickFormat("")
         )

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
            .text("Price ($)");

        g.append("path")
            .datum(data[i])
            .attr("fill", "none")
            .attr("stroke", "steelblue")
            .attr("stroke-linejoin", "round")
            .attr("stroke-linecap", "round")
            .attr("stroke-width", 1.5)
            .attr("d", line);
    });
};

export { downloadAndSaveMarketData, consoleLogDownloadAndSaveMarketData, processMarketData };