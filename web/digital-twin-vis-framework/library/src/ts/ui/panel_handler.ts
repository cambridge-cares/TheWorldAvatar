/**
 * Handles the side panel
 */
class PanelHandler {

    //
    _defaultHTML: string;

    //
    _previousLegendVisibility: string;

    /**
     * 
     */
	public showLinkedFiles(linkedFiles) {
		if(linkedFiles == null || linkedFiles.length == 0) return;

        let contentContainer = document.getElementById("linkedFilesContainer");
		let newHTML = "";

		for(var i = 0; i < linkedFiles.length; i++) {
			let text = linkedFiles[i]["text"];
			let url = linkedFiles[i]["url"];

			// Add link to HTML
			newHTML += "<a href='" + url + "' target='_blank'>"
			newHTML += text;
			newHTML += "</a><br/>"
		}

		contentContainer.innerHTML = newHTML;
	}

    /**
	 * Toggles the visibility of the legend element.
	 * 
	 * @param {boolean} visible 
	 */
	public toggleLegend(visible) {
		document.getElementById("legendContainer").style.display = (visible) ? "block" : "none";
	}

    /**
	 * Sets the title of the side panel.
	 * 
	 * @param {Stirng} title desired title HTML
	 */
	public setTitle(title) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("titleContainer").innerHTML = title;
	}

	/** 
	 * Set the main content of the side panel.
	 * 
	 * @param {String} contentHTML HTML to add.
	 */
    public setContent(contentHTML) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("contentContainer").innerHTML = contentHTML;
		
		var sidePanel = document.getElementById("sidePanel");
		if(sidePanel.classList.contains("large")) {
			document.getElementById("controlsParent").style.visibility = "hidden";
		}
	}

	/**
	 * Override the legend content.
	 * 
	 * @param {String} legendHTML HTML to add. 
	 */
    public setLegend(legendHTML) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("legendContainer").innerHTML = legendHTML;
	}

    /**
	 * Set the footer content.
	 * 
	 * @param {String} footerHTML HTML to add. 
	 */
	public setFooter(footerHTML) {
		var newHTML = `
			<div id="returnContainer">
				<a href="#" onclick="manager.goToDefaultPanel()">&lt; Return</a>
			</div>
			<div id="footerContent">
				` + footerHTML + `
			</div>
		`;
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("footerContainer").innerHTML = newHTML;
	}

    /**
	 * Store the current state of the side panel as it's default
	 */
	public storeDefault() {
		this._defaultHTML = document.getElementById("sidePanelInner").innerHTML;
	}

	/**
	 * Return the contents to their default state.
	 */
	public returnToDefault() {
		if(this._defaultHTML != null) {
			document.getElementById("sidePanelInner").innerHTML = this._defaultHTML;
		}
	}

    /**
	 * Changes the mode of the side panel.
	 */
	public toggleMode() {
		var sidePanel = document.getElementById("sidePanel");
		var leftButton = document.getElementById("slideButton");
		var rightButton = document.getElementById("expandButton") as HTMLImageElement;
		var legend = document.getElementById("legendContainer");

		if(sidePanel.classList.contains("small")) {
			// Make large

			sidePanel.classList.replace("small", "large");
			document.getElementById("map").style.width = "100%";
			document.getElementById("controlsContainer").style.visibility = "hidden";

			leftButton.style.visibility = "hidden";
			rightButton.src = "./img/collapse.png";

			// Hide the legend
			this._previousLegendVisibility = legend.style.visibility;
			legend.style.visibility = "hidden";
			
			// Stop keyboard events
			MapHandler.MAP["keyboard"].disable();
			MapHandler.MAP.resize();

		} else if(sidePanel.classList.contains("large")) {
			// Make small

			sidePanel.classList.replace("large", "small");
			document.getElementById("map").style.width = "calc(100% - 500px)";
			document.getElementById("controlsContainer").style.visibility = "visible";

			leftButton.style.visibility = "visible";
			rightButton.src = "./img/expand.png";

			// Show the legend (if it was visible beforehand)
			legend.style.visibility = this._previousLegendVisibility;

			// Allow keyboard events
			MapHandler.MAP["keyboard"].enable();
			MapHandler.MAP.resize();
		}
	}

    /**
	 * Updates the visibility of the side panel.
	 */
	public toggleExpansion() {
		var sidePanel = document.getElementById("sidePanel");
		var sidePanelInner = document.getElementById("sidePanelInner");
		var rightButton = document.getElementById("expandButton");

		if(sidePanel.classList.contains("small")) {

			if(sidePanel.classList.contains("collapsed")) {
				// Expand
				sidePanel.classList.replace("collapsed", "expanded")
				document.getElementById("map").style.width = "calc(100% - 500px)";
				document.getElementById("legendContainer").style.visibility = "visible";

				rightButton.style.visibility = "visible";
				sidePanelInner.style.visibility = "visible";
				
			} else if(sidePanel.classList.contains("expanded")) {
				// Collapse
				sidePanel.classList.replace("expanded", "collapsed")
				document.getElementById("map").style.width = "calc(100% - 28px)";
				document.getElementById("legendContainer").style.visibility = "hidden";

				rightButton.style.visibility = "hidden";
				sidePanelInner.style.visibility = "hidden";
			}
		} 

		MapHandler.MAP.resize();
	}

}