$(document).ready(function() {
	$.getJSON('/JPS_POWERPLANTEMISSION/PowerPlantEmission',
			function(tables) {
		var tablesSize = tables.length;
		var content = "";
		
		for (var tableIndex = 0; tableIndex < tablesSize; tableIndex++) {
			content += "<table id='table" + tableIndex + "'>";

			var rows = tables[tableIndex].length;
			
			for (var row = 0; row < rows; row++) {
				
				var cols = tables[tableIndex][row].length;
				
				if (row == 0) {
					content += "<tr>";
					
					for (var col = 0; col < cols; col++) {
						content += "<th>" + tables[tableIndex][row][col] + "</th>";
					}
					
					content += "</tr>";
					
				} else {
					content += "<tr>";
					
					for (var col = 0; col < cols; col++) {
						content += "<td>" + tables[tableIndex][row][col] + "</td>";
					}
					
					content += "</tr>";
				}
			}
			
			content += "</table>";
		}
		
		$("#overallTable").append(content);
	});
});