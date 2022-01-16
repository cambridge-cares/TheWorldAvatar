/*
 * Copyright (c) CMCL Innovations - All Rights Reserved
 *  
 * This application and all inherent data, source files, information and graphics are 
 * the copyright and sole property of Computational Modelling Cambridge Ltd (CMCL Innovations). 
 *  
 * Any unauthorised redistribution or reproduction of part, or all, of the contents of this 
 * applicationin any form is prohibited under UK Copyright Law. You may not, except with the 
 * express written permission of CMCL Innovations, distribute or commercially exploit this
 * application or it's content. All other rights reserved.
 *  
 * For more information please contact support@cmclinnovations.com
 * 
 * ------------------------------------------------------------
 * 
 * This script contains functionality for the Marie chemistry chatbot,
 * including selection of random questions, submitting requests, and
 * parsing the resulting data for HTML display.
 * 
 */

// Variables accessed throughout the script
var scriptURL = document.currentScript.src;
var scriptDir = scriptURL.substring(0, scriptURL.lastIndexOf("/") + 1);
var imageDir = "/user/images/";

// Location of the chatbot itself
var botURL = "/marie/request/";

// Hide the results row by default
var resultsRow = document.getElementById("results-row");
resultsRow.style.display = "none";

// Currently asking a question?
var asking = 0;

// Add ENTER detection on search field
document.getElementById("input-field").addEventListener("keyup", function(event) {
	if(event.keyCode === 13) {
		askQuestion();
	}
});

// Add auto-complete for species names in the search box
$('#input-field').autocomplete({
	source: species,
	minLength: 2,
	max: 10,
	scroll: true
});

// Register click listeners for sample questions
linkSampleQuestions();


/*
 Find all 'sample-question' elements and register a click listener.
 */
function linkSampleQuestions() {
	// Find all 'sample-question' elements
	var sampleQuestions = document.getElementsByClassName("sample-question");

	// Bind a click listener to call pipeQuestionFunction
	for (var i = 0; i < sampleQuestions.length; i++) {
		let sampleQuestion = sampleQuestions.item(i);

		sampleQuestion.addEventListener(
			'click',
			function () { pipeQuestion(sampleQuestion.textContent); },
			false
		);
	}
}


/*
 Pipe the input text into the 'input-field' element and fire it off.
 */
function pipeQuestion(question) {
	// Pipe question to text field
	document.getElementById('input-field').value = question;
	$('#input-field').css("color", "inherit");
	window.scrollTo(0, 0);
	
	// Fire query automatically (requested by MK)
	askQuestion();
}


/*
 Pick a random question and enter it into the search control.
 Note that this depends on the fact that the marie-dict.js script
 is also loaded.
 */
function shuffleQuestion() {
	// Find all 'sample-question' elements
	let index = Math.floor(Math.random() * random_questions.length);
	let sampleQuestion = random_questions[index];
	pipeQuestion(sampleQuestion);
}


/*
	Resets the results containers with default content.
*/
function resetResults() {
	let spinner = imageDir + "spinner.svg";
	let html = "<img src=\"" + spinner + "\" style=\"vertical-align: middle;\" width=\"22px\">  Loading, please wait...";
	
	let chatbotResults = document.getElementById("chatbot-results");
	chatbotResults.innerHTML = html;
}


/*
 Send the current question to the chatbot.
 */
function askQuestion() {
	if(asking > 0) {
		// No concurrent questions
		return;
	}
	
	resetResults();
	let spinner = imageDir + "spinner.svg";
	
	// Make the ask button into a loading spinner
	let askButton = document.getElementById("ask-button");
	let imgTags = askButton.getElementsByTagName("img");
	imgTags.item(0).src = spinner;

	// Get the question currently within the input-field
	let inputField = document.getElementById('input-field');
	let question = inputField.value;

	if (question == "") {
		// Show an error
		$('#input-field').val("Please enter a question before submitting.");
		$('#input-field').css("color", "red");
		imgTags.item(0).src = imageDir + "search.svg";
		return;
	}

	
	// Build the URL for the question
	question = question.replace('	', ' ');
	question = question.replace(/[/+]/g, 'add_sign');

	var promises = [];
	
	asking = 1;
	// Make the request for the world avatar
	makeRequest(question, "worldavatar", "json", processChatbotResults, promises);

	// Reset the search button when all requests are complete
	$.when.apply($, promises).then(function() {
		// Revert button to search icon
		let askButton = document.getElementById("ask-button");
		let imgTags = askButton.getElementsByTagName("img");
		imgTags.item(0).src = imageDir + "search.svg";
		$('#input-field').css("color", "inherit");
	}, function() {
		// Error occurred, dealt with elsewhere
	});

	// Show the results row
	resultsRow.style.display = "block";	
}


/*
 Make a single HTTP request to the chatbot.
*/
function makeRequest(question, type, resultType, successFunction, promises) {
	let url = botURL + "chemistry_chatbot/query?type=" + type;
	let data = { "question": question };

	// Make the call
	promises.push($.ajax({
		url: url,
		data: data,
		dataType: resultType,
		timeout: (1000 * 60),
		success: function (data) {
			successFunction(data);
			asking--;
		},
		error: function (xhr, ajaxOptions, thrownError) {
			console.log(xhr.status);
			console.log(thrownError);

			successFunction(null);
			asking--;
		}
	}));
}


/*
 Process the results from a WorldAvatar request.
*/
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
			chatbotResults = parseJSONObject(jsonData);
		}
	}

	// Find the div container to add results to
	let resultsContainer = document.getElementById("chatbot-results");
	resultsContainer.innerHTML = chatbotResults;
}

/*
 Parses the input JSON Object into a HTML table.
*/
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

/*
 Parses the input JSON Array into a HTML table.
*/
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

/*
 Converts input dictionary (of column-mahor data) into a HTML table.
*/
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

/*
 Returns true if the input string is a valid URL.
*/
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

/*
 Returns true if the input URL links to an image.
*/
function isImageURL(url) {
	if(url.endsWith(".svg")) {
		return true;
	}
	if(url.endsWith(".bmp")) {
		return true;
	}
	if(url.endsWith(".png")) {
		return true;
	}
	if(url.endsWith(".jpg")) {
		return true;
	}
	if(url.endsWith(".jpeg")) {
		return true;
	}
	return false;
}



