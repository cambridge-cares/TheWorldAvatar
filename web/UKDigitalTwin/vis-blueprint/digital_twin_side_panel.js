class DigitalTwinSidePanel {

	// Empty side panel template
	_sidePanelHTML = `
		<div id="sidePanel">
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
	constuctor() {
		let sidePanel = document.getElementById("sidePanel");
		if(sidePanel == null) {
			document.body.innerHTML += this._sidePanelHTML;
		}
	}

	/**
	 * Toggles the visibility of the title element.
	 * 
	 * @param {boolean} visible 
	 */
	toggleTitle(visible) {
		document.getElementById("titleContainer").style.visible = visible;
	}

		/**
	 * Toggles the visibility of the properties element.
	 * 
	 * @param {boolean} visible 
	 */
	toggleProperties(visible) {
		document.getElementById("propsContainer").style.visible = visible;
	}

	/**
	 * Toggles the visibility of the content element.
	 * 
	 * @param {boolean} visible 
	 */
	toggleContent(visible) {
		document.getElementById("contentContainer").style.visible = visible;
	}

	/**
	 * Toggles the visibility of the legend element.
	 * 
	 * @param {boolean} visible 
	 */
	toggleLegend(visible) {
		document.getElementById("legendContainer").style.visible = visible;
	}

	/**
	 * Toggles the visibility of the footer element.
	 * 
	 * @param {boolean} visible 
	 */
	 toggleFooter(visible) {
		document.getElementById("footerContainer").style.visible = visible;
	}

	/**
	 * Sets the title of the side panel.
	 * 
	 * @param {Stirng} title desired title HTML
	 */
	setTitle(title) {
		document.getElementById("titleContainer").innerHTML = "<h3>" + title + "</h3>";
	}

	/**
	 * Set the properties to show in a table within the side panel.
	 * 
	 * @param {Map<String, String>} data dictionary of properties 
	 */
	setProperties(data) {

	}

	/** 
	 * Set the main content of the side panel.
	 * 
	 * @param {String} contentHTML HTML to add.
	 */
	setContent(contentHTML) {
		document.getElementById("contentContainer").innerHTML = contentHTML;
	}


	/**
	 * Set the legend content.
	 * 
	 * @param {String} legendHTML HTML to add. 
	 */
	setLegend(legendHTML) {
		document.getElementById("legendContainer").innerHTML = legendHTML;
	}

		/**
	 * Set the footer content.
	 * 
	 * @param {String} footerHTML HTML to add. 
	 */
	setLegend(footerHTML) {
		document.getElementById("footerContainer").innerHTML = footerHTML;
	}

}