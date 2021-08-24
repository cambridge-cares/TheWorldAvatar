/**
*	This JS file handles functionality for the Electricity Consumption visualisation.
 *	Author: Wanni Xie (wx243@cam.ac.uk)
 *	Last Update Date: 19 August 2021
*/

/**
	 * Fired when a area or a regional boundaries is selected.
	 * 
	 * @param Location - place name
	 * @param Area_LACode - fuel
	 * @param TotalELecConsumption - capacity
	 * @param DomesticConsumption - SDG indicator
	 * @param Industrial_and_Commercial - coordinates
	 */
function selectArea(Location, Area_LACode, TotalELecConsumption, DomesticConsumption, Industrial_and_Commercial) {
	if (Location == null) {
		// Do nothing
		return;
	}

	// Set title to offtake name
	setSidePanelTitle(`
		<h2>` + Location + `</h2>
	`);

	// Show meta data
	var metaHTML = `
		<table width="100%">
			<tr>
				<td width="75%">LA Code:</td>
				<td width="25%" style="text-align: right;">` + Area_LACode + `</td>
			</tr>
			<tr>
				<td width="75%">Total Electricity Consumption:</td>
				<td width="15%" style="text-align: right;">` + TotalELecConsumption + ` GWh</td>
			</tr>
			<tr>
				<td width="75%">Domestic Consumption:</td>
				<td width="25%" style="text-align: right;">` + DomesticConsumption + ` GWh</td>
			</tr>
			<tr>
				<td width="65%">Industrial and Commercial Consumption:</td>
				<td width="35%" style="text-align: right;">` + Industrial_and_Commercial + ` GWh</td>
			</tr>
	`;

	metaHTML += "</table>";
	setSidePanelMeta(metaHTML);

	// Update text container 
	setSidePanelText(``);
}
