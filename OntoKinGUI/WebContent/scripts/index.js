let getTableResultRowString = (index, resultObj) => {
	let tdNodes = '';
	
	console.log(resultObj);
	for (let x in resultObj) {
		console.log(resultObj[x]);
		tdNodes += `<td>${resultObj[x]}</td>`;
	}
	
	return `<tr class="row-query-results">
	    		<td class="index">${index}</td>
    		` +
    		tdNodes +
			`</tr>`
};

$("#execute").on("click", () => {
	let queryString = $("#query-string").val();
	let queryResultsTable = $("#table-query-results");
	$("#num-results").text("");
	$(".row-query-results").remove();
	
	
	$.ajax({
		type: 'GET',
		url: "/OntoKinGUI/OntoKinEndpointProxy",
		data: {queryString},
		success: data => {
			console.log(data)
			let trimmedResult = data.slice(1, data.length-2);
			let resultArray = trimmedResult.split('}');
			if (resultArray.length === 1) {
//				alert("Your query returned 0 results.");
				$("#num-results").text("No results found.");				
			} else {
				resultArray.pop();
				console.log(resultArray[0])
				firstResult = resultArray[0] + '}';
				firstResult = firstResult.replace(/\n +/g, "");
				console.log(firstResult)
				firstResultObj = JSON.parse(firstResult);
				$(".row-header").append(`<th class="row-query-results first-column">Index</td>`);
				for (let x in firstResultObj) {
					$(".row-header").append(`<th class="row-query-results">${x}</td>`)
				}
				
				
				let count = 1;
				for (let result of resultArray) {
					jsonString = result + '}'
					jsonString = jsonString.replace(/\n +/g, "");
					resultObj = JSON.parse(jsonString);
					queryResultsTable.append(getTableResultRowString(count++, resultObj));
				}
//				alert(`Your query return ${count-1} results.`);
				$("#num-results").text(`${count-1} results found.`);
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
	$("#num-results").text('');
	$(".row-query-results").remove();
})
