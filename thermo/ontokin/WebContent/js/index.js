let getTableResultRowString = (index, resultObj) => {
		let tdNodes = '';
		
		//console.log(resultObj);
		for (let x in resultObj) {
			//console.log(resultObj[x]);
			tdNodes += `<td>${resultObj[x]}</td>`;
		}
		
		return `<tr class="row-query-results">
		    		<td class="index">${index}</td>
	    		` +
	    		tdNodes +
				`</tr>`
	};

// 	$("#search_btn").on("click", () => {
	$("#execute").on("click", () => {

		//console.log('ss');
		let queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
	 	'PREFIX ontokin:' + '\n' +
		'<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>'+ '\n' +
		'SELECT ?x' + '\n' +
		'WHERE {' + '\n' +
		    '?x rdf:type ontokin:ReactionMechanism .' + '\n' +
		'} ';
		
		let search_term_name = $("#term.name"); //cl2
		let search_querySelection = $("#querySelection"); //thermo
		
// 		if (search_term_name !=  '' || search_querySelection !=  undefined || search_querySelection !=  null) {
// 			if (search_querySelection == 'thermo' ) {
// 				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + 
// 					'PREFIX ontokin:' +
// 						'<http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>'+
// 						'SELECT ?x' +
// 						'WHERE {' +
// 						    '?x rdf:type ontokin:ReactionMechanism .' +
// 						'} ';
// 			}
// 			else {
				
// 			}
			
// 		}
		
		
		let queryResultsTable = $("#table-query-results");
		$("#num-results").text("");
		$(".row-query-results").remove();
		
		
		$.ajax({
			type: 'GET',
			url: "http://localhost:8080/RDF4J_SPARQL_GUI/SPARQLEndpointProxy",
			data: {queryString},
			success: data => {
				// console.log(data)
				let trimmedResult = data.slice(1, data.length-2);
				let resultArray = trimmedResult.split('}');
				if (resultArray.length === 1) {
//					alert("Your query returned 0 results.");
					$("#num-results").text("No results found.");				
				} else {
					resultArray.pop();
					
					firstResult = resultArray[0] + '}';
					firstResult = firstResult.replace(/\n +/g, "");
					firstResultObj = JSON.parse(firstResult);
					$(".row-header").append(`<th class="row-query-results first-column">Index</th>`);
					for (let x in firstResultObj) {
						$(".row-header").append(`<th class="row-query-results">${firstResultObj[x]}</th>`)
					}
					
					
					let count = 1;
					for (let result of resultArray) {
						jsonString = result + '}'
						jsonString = jsonString.replace(/\n +/g, "");
						resultObj = JSON.parse(jsonString);
						queryResultsTable.append(getTableResultRowString(count++, resultObj));
					}
//					alert(`Your query return ${count-1} results.`);
					$("#num-results").text(`${count-1} results found.`);
				}
			},
			error: (XMLHttpRequest, textStatus, errorThrown) => { 
//	            alert("Status: " + textStatus); 
//	            alert("Error: " + errorThrown);
				alert("INCORRECT SPARQL QUERY!")
	        }
		})
		
	});