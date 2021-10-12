/**
 * Handles the creation and population of the side panel to contain content
 * about the currently visualised locations/regions.
 */
class DigitalTwinSidePanel {

	// MapBox map
	_map;

	// Empty side panel template
	_sidePanelHTML = `
		<img id="sidePanelButton" src="./img/arrow.png" width="16px" height="16px" onclick="DT.sideHandler.setVisibility()"/>
		<div id="sidePanelInner">
			<div id="titleContainer"></div>
			<div id="propsContainer"></div>
			<div id="contentContainer"></div>
			<div id="legendContainer"></div>
			<div id="footerContainer"></div>
		</div>
	`;

	/**
	 * Initialise a new DigitalTwinSidePanel, appending the element
	 * into the document automatically.
	 */
	constructor(map) {
		this._map = map;
		this.#createSidePanel();

		// TEST
		document.addEventListener('keyup', (e) => {
			if (e.code === "ArrowUp") {
				this.setMode("large");
			} else if (e.code === "ArrowDown") {
				this.setMode("small");
			}
		});
	}

	/**
	 * 
	 */
	#createSidePanel() {
		let sidePanel = document.getElementById("sidePanel");
		
		if(sidePanel == null) {
			let newDiv = document.createElement("div")
			newDiv.id = "sidePanel";
			newDiv.classList.add("small");
			newDiv.innerHTML = this._sidePanelHTML;
			document.body.appendChild(newDiv);
		} 
	}

	/**
	 * Toggles the visibility of the title element.
	 * 
	 * @param {boolean} visible 
	 */
	toggleTitle(visible) {
		document.getElementById("titleContainer").style.visibility = visible;
	}

		/**
	 * Toggles the visibility of the properties element.
	 * 
	 * @param {boolean} visible 
	 */
	toggleProperties(visible) {
		document.getElementById("propsContainer").style.visibility = visible;
	}

	/**
	 * Toggles the visibility of the content element.
	 * 
	 * @param {boolean} visible 
	 */
	toggleContent(visible) {
		document.getElementById("contentContainer").style.visibility = visible;
	}

	/**
	 * Toggles the visibility of the legend element.
	 * 
	 * @param {boolean} visible 
	 */
	toggleLegend(visible) {
		document.getElementById("legendContainer").style.visibility = visible;
	}

	/**
	 * Toggles the visibility of the footer element.
	 * 
	 * @param {boolean} visible 
	 */
	toggleFooter(visible) {
		document.getElementById("footerContainer").style.visibility = visible;
	}

	/**
	 * Sets the title of the side panel.
	 * 
	 * @param {Stirng} title desired title HTML
	 */
	setTitle(title) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("titleContainer").innerHTML = "<h3>" + title + "</h3>";
	}

	/**
	 * Set the properties to show in a table within the side panel.
	 * 
	 * @param {Map<String, String>} data dictionary of properties 
	 */
	setProperties(data) {
		document.getElementById("sidePanel").style.visibility = "visible";

		var tableHTML = `
			<table width="100%">
				<tr>
					<td colspan="3" style="border-bottom: 1px solid lightgrey;"><b>Location Metadata</b></td>
				</tr>
				<tr>
					<td colspan="3" height="10px"></td>
				</tr>
		`;
		for (const [key, value] of Object.entries(data)) {
			tableHTML += `<tr>`;
			tableHTML += `<td width="5px"></td>`;
			tableHTML += `<td width="35%"><b>` + key + ":</b></td>";
			tableHTML += `<td width="calc(65% - 5px)">` + value + "</td>";
			tableHTML += "</tr>";
		}
		tableHTML += `
				<tr>
					<td colspan="3" height="10px"></td>
				</tr>
			</table>
		`;

		document.getElementById("propsContainer").innerHTML = tableHTML;
	}

	/**
	 * Sets the title of the side panel.
	 * 
	 * @param {Stirng} title desired title HTML
	 */
	 setTitle(title) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("titleContainer").innerHTML = "<h2>" + title + "</h2>";
	}

	/** 
	 * Set the main content of the side panel.
	 * 
	 * @param {String} contentHTML HTML to add.
	 */
	setContent(contentHTML) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("contentContainer").innerHTML = contentHTML;

		var sidePanel = document.getElementById("sidePanel");
		if(sidePanel.classList.contains("large")) {
			document.getElementById("controlsParent").style.visibility = "hidden";
		}
	}

	/**
	 * Set the legend content.
	 * 
	 * @param {String} legendHTML HTML to add. 
	 */
	setLegend(legendHTML) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("legendContainer").innerHTML = legendHTML;
	}

	/**
	 * Set the footer content.
	 * 
	 * @param {String} footerHTML HTML to add. 
	 */
	setFooter(footerHTML) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("footerContainer").innerHTML = footerHTML;
	}

	/**
	 * 
	 * @param {String} mode desired mode {"small", "large"} 
	 */
	setMode(mode) {
		var sidePanel = document.getElementById("sidePanel");
		var button = document.getElementById("sidePanelButton");

		if(mode === "large") {
			sidePanel.classList.replace("small", "large");
			button.src = "./img/close.png";
			document.getElementById("map").style.width = "100%";
			document.getElementById("controlsParent").style.visibility = "hidden";

			// Stop keyboard events
			this._map["keyboard"].disable();
			this._map.resize();

		} else {
			sidePanel.classList.replace("large", "small");
			button.src = "./img/arrow.png";
			document.getElementById("map").style.width = "calc(100% - 500px)";
			document.getElementById("controlsParent").style.visibility = "visible";

			// Allow keyboard events
			this._map["keyboard"].enable();
			this._map.resize();
		}
	}

	/**
	 * 
	 */
	setVisibility() {
		var sidePanel = document.getElementById("sidePanel");

		if(sidePanel.classList.contains("small")) {

			if(sidePanel.classList.contains("none")) {
				sidePanel.classList.remove("none");
				document.getElementById("map").style.width = "calc(100% - 500px)";
				
			} else {
				sidePanel.classList.add("none");
				document.getElementById("map").style.width = "calc(100% - 28px)";
			}

		} else if(sidePanel.classList.contains("large")) {
			
			if(sidePanel.style.visibility) {
				document.getElementById("controlsParent").style.visibility = "visible";
				sidePanel.style.visibility = "hidden";
			} else {
				document.getElementById("controlsParent").style.visibility = "hidden";
				sidePanel.style.visibility = "visible";
			}
		}

		this._map.resize();
	}
}