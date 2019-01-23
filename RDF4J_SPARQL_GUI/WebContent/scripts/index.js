let getTableResultRowString = (index, resultObj) => {
	let tdNodes = '';
	
	console.log(resultObj);
	for (let x in resultObj) {
		console.log(resultObj[x]);
		tdNodes += `<td>${resultObj[x]}</td>`;
	}
	
	return `<tr class="row-query-results">
	    		<td>${index}</td>
    		` +
    		tdNodes +
			`</tr>`
};

$("#execute").on("click", () => {
	let queryString = $("#query-string").val();
	let queryResultsTable = $("#table-query-results");
	$(".query-result").remove();
	$(".row-query-results").remove();
	
	
	$.ajax({
		type: 'GET',
		url: "/RDF4J_SPARQL_GUI/SPARQLEndpointProxy",
		data: {queryString},
		success: data => {
			let trimmedResult = data.slice(1, data.length-2);
			let resultArray = trimmedResult.split('}');
			if (resultArray.length === 1) {
				alert("Your query returned 0 results.");
			} else {
				resultArray.pop();
				
				firstResult = resultArray[0] + '}';
				firstResult = firstResult.replace(/\n +/g, "");
				firstResultObj = JSON.parse(firstResult);
				$(".row-header").append(`<td class="row-query-results">Index</td>`);
				for (let x in firstResultObj) {
					$(".row-header").append(`<td class="row-query-results">${x}</td>`)
				}
				
				
				let count = 1;
				for (let result of resultArray) {
					jsonString = result + '}'
					jsonString = jsonString.replace(/\n +/g, "");
					resultObj = JSON.parse(jsonString);
					queryResultsTable.append(getTableResultRowString(count++, resultObj));
				}
				alert(`Your query return ${count-1} results.`);
			}
		},
		error: (XMLHttpRequest, textStatus, errorThrown) => { 
//            alert("Status: " + textStatus); 
//            alert("Error: " + errorThrown);
			alert("INCORRECT SPARQL QUERY!")
        }
	})
})

$("#clear").on("click", () => {
	$("#query-string").val('');
	$(".row-query-results").remove();
})
