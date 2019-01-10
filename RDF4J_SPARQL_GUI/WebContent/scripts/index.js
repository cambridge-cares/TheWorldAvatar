let getTableResultRowString = (index, resultObj) => {
	return `<tr>
	    		<td>${index}</td>
	    		<td>x</td>
	    		<td>${resultObj.x}</td>
			</tr>`
}

$("#execute").on("click", () => {
	let queryString = $("#query-string").val();
	let queryResultsTable = $("#table-query-results");
	$(".query-result").remove();
	
	$.ajax({
		type: 'GET',
		url: "/RDF4J_SPARQL_GUI/SPARQLEndpointProxy",
		data: {queryString},
		success: data => {
			let trimmedResult = data.slice(1, data.length-2);
			let resultArray = trimmedResult.split('}');
			resultArray.pop();
			
			let count = 1;
			for (let result of resultArray) {
				jsonString = result + '}'
				resultObj = JSON.parse(jsonString);	
				queryResultsTable.append(getTableResultRowString(count++, resultObj));
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
})
