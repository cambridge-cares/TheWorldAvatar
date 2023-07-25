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

