$('document').ready(function(){
    // =================== search button and enter in input field =======
    $('#ask-button').click(function (e){
        askQuestion(e);
    });

    $('#input-field').keypress(function(e){
        if(e.which === 13){//Enter key pressed
            askQuestion(e);
        }
    });
    google.charts.load('current', {packages: ['corechart', 'line']});
});
function askQuestion(){
    let question = $('#input-field').val();
    let candidate_answers = [a1];
    candidate_answers.forEach(e => process_matrix_data(e));
    return a1;
}

function makeTable(matrix_set){
    // let test_valueSet = [{'x': '1', 'y': '2'},{'x': '1', 'y': '2'},{'x': '1', 'y': '2'},{'x': '1', 'y': '2'}]
    matrix_set.forEach(function (matrix) {
        let x_data = matrix['value'];
        let array = []
        let x_data_list = []
        if (typeof x_data === 'string'){
            x_data_list.push([x_data]);
        }
        else{
            x_data_list = x_data
        }
        x_data_list.forEach(function (x,index){
            let valueSet = {};
            let x_data_title = matrix['attribute'];
            let x_data_unit = matrix['unit'];
            valueSet[x_data_title] = x
            valueSet['unit'] = x_data_unit;

        Object.keys(matrix).forEach(function (k) {
            if(k!== 'value' && k!== 'attribute' && k!== 'unit'){
                let content = '';
                let unit = matrix[k]['unit'];
                let value = matrix[k]['value'];
                if (typeof value === 'string'){
                    content = value + ' ' + unit;
                    valueSet[k] = content
                }else{
                    valueSet[k] = value[index] + ' ' + unit;
                }
            }
        });
        array.push(valueSet);
    });
        processChatbotResults(array);
    });

}
function drawLineChart(rows, attribute, y_unit, x_data_title, x_data_unit, fixed_data_title, fixed_data, fixed_data_unit) {
      // create a new id for this element
      let element_id = '#' + hash(rows.toString()).toString();
      console.log('element id', element_id);

        $('<div>', {
            id: element_id,
            class: 'chart',
        }).appendTo('#chart_div');


      $(element_id).ready(function() {

          var data = new google.visualization.DataTable();
          data.addColumn('number', 'X');
          data.addColumn('number', attribute);
          data.addRows(rows);
          var options = {

            title: attribute + ' (' + y_unit + ') at ' + fixed_data_title + ' of ' + fixed_data + ' ' + fixed_data_unit,
            hAxis: {
              title: x_data_title + ' (' + x_data_unit + ')',
            },
            backgroundColor: '#f1f8e9',
            legend: {position: 'none'}
          };
          var chart = new google.visualization.LineChart(document.getElementById(element_id));
          chart.draw(data, options);
      });


    }
function prepare_chart_data(elements){
    // find the value, find the other array
    // y, x
    elements.forEach(function (element){
        let y_data = element['value']
        let y_unit = element['unit']
        let x_data = []
        let x_data_title = ''
        let x_data_unit = ''
        let attribute = element['attribute']
        let fixed_data_title = ''
        let fixed_data = ''
        let fixed_data_unit = ''
        console.log(attribute)
        // find the fixed value and the serial value
        // find the key that is neither value nor attribute
        Object.keys(element).forEach(function (k) {
            if(k!== 'value' && k!== 'attribute'){
                // see whether this is single value or serial
                if(typeof element[k]['value'] === "object"){
                    // this is serial, x axis
                    let x_data_object = element[k]; // containing values and a unit
                    x_data_unit = x_data_object['unit'];
                    x_data = x_data_object['value'];
                    x_data_title = k; // store the title of x axis too
                }else{
                    // this is the fixed value
                    fixed_data_title = k;
                    let fixed_data_object = element[k];
                    fixed_data_unit = fixed_data_object['unit'];
                    fixed_data = fixed_data_object['value'];
                }
            }
        });
        let rows = make_rows(x_data, y_data);
        drawLineChart(rows, attribute, y_unit, x_data_title, x_data_unit, fixed_data_title, fixed_data, fixed_data_unit);
    });
}
function make_rows(x_data, y_data){
    let rows = [];
    for (let i = 0; i < y_data.length; i++) {
        rows.push([parseFloat(x_data[i]), parseFloat(y_data[i])]);
    }
    return rows
}
function process_matrix_data(matrix){
    let elements = [];
    if ("multiple_results" in matrix){
        elements = matrix['multiple_results'];
        prepare_chart_data(elements);
        makeTable(elements);
    }
    else{
        if(typeof matrix['value'] === "string"){
            // single value, table only
            elements = [matrix]
            makeTable(elements)
        }else{
            // serial value, use chart to visualise
            elements = [matrix];
            prepare_chart_data(elements);
            makeTable(elements)
        }
    }
}



a1 =  {
    "value": [
        "-205.69",
        "-206.12",
        "-230.14",
        "-255.35",
        "-281.58",
        "-308.70",
        "-336.64",
        "-365.32",
        "-394.68",
        "-455.23",
        "-550.13",
        "-615.76",
        "-717.35",
        "-893.90",
        "-1078.17",
        "-1269.01",
        "-1465.55",
        "-1667.13",
        "-1873.21"
    ],
    "unit": "kJ/mol",
    "temperature": {
        "value": [
            "298.15",
            "300.00",
            "400.00",
            "500.00",
            "600.00",
            "700.00",
            "800.00",
            "900.00",
            "1000.00",
            "1200.00",
            "1500.00",
            "1700.00",
            "2000.00",
            "2500.00",
            "3000.00",
            "3500.00",
            "4000.00",
            "4500.00",
            "5000.00"
        ],
        "unit": "K"
    },
    "pressure": {
        "value": "101325.00",
        "unit": "Pa"
    },
    "attribute": "gibbs energy"
};
a2 = {
    "multiple_results": [
        {
            "value": [
                "28.73",
                "28.81",
                "32.85",
                "36.09",
                "38.73",
                "40.92",
                "42.75",
                "44.28",
                "45.56",
                "47.53",
                "49.47",
                "50.35",
                "51.27",
                "52.20",
                "52.74",
                "53.07",
                "53.29",
                "53.45",
                "53.56"
            ],
            "unit": "J/mol/K",
            "temperature": {
                "value": [
                    "298.15",
                    "300.00",
                    "400.00",
                    "500.00",
                    "600.00",
                    "700.00",
                    "800.00",
                    "900.00",
                    "1000.00",
                    "1200.00",
                    "1500.00",
                    "1700.00",
                    "2000.00",
                    "2500.00",
                    "3000.00",
                    "3500.00",
                    "4000.00",
                    "4500.00",
                    "5000.00"
                ],
                "unit": "K"
            },
            "pressure": {
                "value": "101325.00",
                "unit": "Pa"
            },
            "attribute": "heat capacity at constant volume"
        },
        {
            "value": [
                "37.04",
                "37.13",
                "41.17",
                "44.40",
                "47.05",
                "49.24",
                "51.07",
                "52.59",
                "53.87",
                "55.84",
                "57.79",
                "58.66",
                "59.58",
                "60.51",
                "61.05",
                "61.38",
                "61.61",
                "61.76",
                "61.87"
            ],
            "unit": "J/mol/K",
            "temperature": {
                "value": [
                    "298.15",
                    "300.00",
                    "400.00",
                    "500.00",
                    "600.00",
                    "700.00",
                    "800.00",
                    "900.00",
                    "1000.00",
                    "1200.00",
                    "1500.00",
                    "1700.00",
                    "2000.00",
                    "2500.00",
                    "3000.00",
                    "3500.00",
                    "4000.00",
                    "4500.00",
                    "5000.00"
                ],
                "unit": "K"
            },
            "pressure": {
                "value": "101325.00",
                "unit": "Pa"
            },
            "attribute": "heat capacity at constant pressure"
        }
    ]
};
a3 = {
    "value": "37.13",
    "unit": "J/mol/K",
    "temperature": {
        "value": "300.00",
        "unit": "K"
    },
    "pressure": {
        "value": "101325.00",
        "unit": "Pa"
    },
    "attribute": "heat capacity at constant pressure"
};

const hash = function(str, seed = 0) {
    let h1 = 0xdeadbeef ^ seed, h2 = 0x41c6ce57 ^ seed;
    for (let i = 0, ch; i < str.length; i++) {
        ch = str.charCodeAt(i);
        h1 = Math.imul(h1 ^ ch, 2654435761);
        h2 = Math.imul(h2 ^ ch, 1597334677);
    }
    h1 = Math.imul(h1 ^ (h1>>>16), 2246822507) ^ Math.imul(h2 ^ (h2>>>13), 3266489909);
    h2 = Math.imul(h2 ^ (h2>>>16), 2246822507) ^ Math.imul(h1 ^ (h1>>>13), 3266489909);
    return 4294967296 * (2097151 & h2) + (h1>>>0);
};
function parseJSONObject(jsonResults) {
	let valueSet = {};
	valueSet["Result"] = [];

	// Get the variable headers
	let headObject = jsonResults["head"];

	if (headObject == null) {
		// May not be a JSON object?
		return parseJSONArray(jsonResults);
	}

	let headVars = headObject["vars"];
	headVars.forEach((key) => {
		let values = valueSet[key];
		if (values == null) {
			values = [];
			valueSet[key] = values;
		}
	});

	// Store the values from each array entry
	let resultsObject = jsonResults["results"];
	let bindings = resultsObject["bindings"];

	let index = 1;
	bindings.forEach((item) => {
		// Store result index
		valueSet["Result"].push(index);
		index++;

		Object.keys(item).forEach((key) => {
			// Store value
			let subitem = item[key];
			valueSet[key].push(subitem["value"]);
		})
	});

	return toTable(valueSet);
}
function toTable(valueSet) {
	// Build into HTML table
	let html = "<table class=\"chatbot-table\"><tr>";

	// Headers
	Object.keys(valueSet).forEach((header) => {
		html += "<th>" + header + "</th>";
	});
	html += "</tr>";

	// Values
	let rows = valueSet["Result"].length;

	for (var r = 0; r < rows; r++) {
		html += "<tr>";

		for (var key in valueSet) {
			let values = valueSet[key];
			let value = values[r];

			html += "<td>";

			if(isValidURL(String(value))) {

				if(isImageURL(String(value))) {
					// Image with link to source
					html += "<a href=\"" + value + "\">";
					html += "<img width=\"200px\" src=\"" + value + "\"/>";
					html += "</a>";
				} else {
					// Text link
					html += "<a href=\"" + value + "\">" + value + "</a>";
				}
			} else {
				// Try removing spaces from the value, if it then matches
				// an entry in the species list, keep it without the spaces
				let tempValue = String(value).replace(/\s/g, '');

				if(species.includes(tempValue)) {
					html += tempValue;
				} else {
					html += value;
				}
			}
			html += "</td>"
		}
		html += "</tr>";
	}

	html += "</table>";
	return html;
}
function parseJSONArray(jsonResults) {
	// JSON values (map of column names to value array)

	let valueSet = {};
	valueSet["Result"] = [];
	let index = 1;
	jsonResults.forEach((item) => {
		// Store result index
		valueSet["Result"].push(index);
		index++;

		Object.keys(item).forEach((key) => {
			// Store value
			let values = valueSet[key];
			if (values == null) {
				values = [];
				valueSet[key] = values;
			}
			values.push(item[key]);
		})
	});

	return toTable(valueSet);
}
function processChatbotResults(rawResults) {
	// Parse the results
	var chatbotResults = null;

	if (rawResults == null || rawResults == "") {
		chatbotResults = "<span style=\"color: red; padding-left: 15px;\">The World Avatar failed to provide and answer.</span>";
	} else {
		// Get the data into JSON form (if not already);
		let jsonData = null;

		try {
			jsonData = JSON.parse(rawResults);
		} catch (err1) {
			try {
				jsonData = JSON.parse(JSON.stringify(rawResults));
			} catch (err2) {
				jsonData = rawResults;
			}
		}

		if (Array.isArray(jsonData)) {
			// JSON array
			chatbotResults = parseJSONArray(jsonData);
		} else {
			// JSON object
                        console.log(jsonData)

			chatbotResults = parseJSONObject(jsonData);
		}
	}

	// Find the div container to add results to
	let resultsContainer = document.getElementById("chatbot-results");
	resultsContainer.innerHTML = chatbotResults;
}
function isValidURL(possibleURL) {
	if(possibleURL.startsWith("http:") || possibleURL.startsWith("https:")) {
		let url;

		try {
			url = new URL(possibleURL);
			return true;
		} catch (error) {
			return false;
		}
	}
	return false;
}

