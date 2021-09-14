function selectBus(Bus_num, Bus_type, para_Gs, para_Bs, para_area, para_basekV, para_zone, para_Vmax, para_Vmin) {
		if (Bus_num == null) {
			// Do nothing
			return;
		}

		// Set title to offtake name
		setSidePanelTitle(`
			<h2> Bus ` + Bus_num + `</h2>
		`);

		// Show meta data
		var metaHTML = `
			<table width="100%">
				<tr>
					<td width="75%">Bus type:</td>
					<td width="25%" style="text-align: right;">` + Bus_type + `</td>
				</tr>
				<tr>
					<td width="75%">Gs (shunt conductance):</td>
					<td width="15%" style="text-align: right;">` + para_Gs + ` MW</td>
				</tr>
				<tr>
					<td width="75%">Bs (shunt susceptance):</td>
					<td width="25%" style="text-align: right;">` + para_Bs + ` MVAr</td>
				</tr>
				<tr>
					<td width="75%">Bus Area (area number):</td>
					<td width="25%" style="text-align: right;">` + para_area + ` </td>
				</tr>
				<tr>
					<td width="75%">Base kV (base voltage):</td>
					<td width="25%" style="text-align: right;">` + para_basekV + ` kV</td>
				</tr>
				<tr>
					<td width="75%">Zone (loss zone):</td>
					<td width="25%" style="text-align: right;">` + para_zone + ` kV</td>
				</tr>
				<tr>
					<td width="75%">Vmax (maximum voltage magnitude):</td>
					<td width="25%" style="text-align: right;">` + para_Vmax + ` p.u.</td>
				</tr>
				<tr>
					<td width="75%">Vmin (minimum voltage magnitude):</td>
					<td width="25%" style="text-align: right;">` + para_Vmin + ` p.u.</td>
				</tr>
		`;

		metaHTML += "</table>";
		setSidePanelMeta(metaHTML);

		// Update text container 
		setSidePanelText(``);
}
