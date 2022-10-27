/**
 * Handles the generation of UI controls for feature seraching.
 * Concrete implementations handle filtering for searched features
 * via the relevant mapping API.
 */
abstract class SearchHandler {

    /**
     * Array of all possible search properties.
     */
    protected allSearchProperties;

    /**
     * Selected property from settings.json defining the search parameters.
     */
    protected property;

    /**
     * Initialise a new search handler.
     */
    constructor() {
        this.allSearchProperties = Manager.SETTINGS.getSetting("search");
        this.generateControls();
    }

    /**
     * Toggle the visibility of the search controls.
     */
    public toggle() {
        let finderContainer = document.getElementById("finderContainer");
        if(finderContainer === null || finderContainer === undefined) return;

        if(finderContainer.style.display === "table") {
            finderContainer.style.display = "none";
        } else {
            finderContainer.style.display = "table";
        }

        let sidePanel = document.getElementById("sidePanel");
        if(sidePanel === null || sidePanel === undefined) return;

        if(sidePanel.classList.contains("expanded")) {
            finderContainer.style.width = "calc(100% - 540px)";
        } else {
            finderContainer.style.width = "calc(100% - 80px)";
        }
    }

    /**
     * Generates UI controls for searching (using the parameters set
     * within the settings file) and adds them to the HTML document.
     */
    private generateControls() {
        let finderContainer = document.getElementById("finderContainer");
        if(finderContainer !== null) return;

        // Build container
        finderContainer = document.createElement("div");
        finderContainer.id = "finderContainer";
        finderContainer.style.display = "none";

        // Build label
        let finderLabel = document.createElement("label");
        finderLabel.id = "finderLabel";
        finderLabel.innerHTML = "Find locations where";
        finderContainer.appendChild(finderLabel);
    
        // Build drop down
        let propertyDrop = document.createElement("select");
        propertyDrop.id = "finderSelect";
        propertyDrop.classList.add("finderSelectClass");

        this.allSearchProperties.forEach(property => {
            let option = document.createElement("option");
            option.classList.add("finderSelectClass");
            option.value = property["property"];
            option.text = property["description"];
            propertyDrop.appendChild(option);
        });

        // Add buttons
        let helpContainer = document.createElement("div");
        helpContainer.classList.add("tooltip");
        helpContainer.classList.add("searchButton");
        helpContainer.addEventListener("click", () => this.showHelp());
        helpContainer.innerHTML = "<span class='tooltiptext lower'>Help</span>";
        let helpButton = document.createElement("i");
        helpButton.classList.add("fas");
        helpButton.classList.add("fa-question");
        helpButton.classList.add("fa-lg");
        helpContainer.appendChild(helpButton);

        let confirmContainer = document.createElement("div");
        confirmContainer.classList.add("tooltip");
        confirmContainer.classList.add("searchButton");
        confirmContainer.innerHTML = "<span class='tooltiptext lower'>Run this search</span>";
        confirmContainer.addEventListener("click", () => this.startSearch());
        let confirmButton = document.createElement("i");
        confirmButton.classList.add("fas");
        confirmButton.classList.add("fa-search-location");
        confirmButton.classList.add("fa-lg");
        confirmContainer.appendChild(confirmButton);

        let hideContainer = document.createElement("div");
        hideContainer.classList.add("tooltip");
        hideContainer.classList.add("searchButton");
        hideContainer.innerHTML = "<span class='tooltiptext lower'>Hide search controls</span>";
        hideContainer.addEventListener("click", () => this.toggle());
        let hideButton = document.createElement("i");
        hideButton.classList.add("far");
        hideButton.classList.add("fa-minus-square");
        hideButton.classList.add("fa-lg");
        hideContainer.appendChild(hideButton);

        let cancelContainer = document.createElement("div");
        cancelContainer.classList.add("tooltip");
        cancelContainer.classList.add("searchButton");
        cancelContainer.addEventListener("click", () => this.cancelSearch());
        cancelContainer.innerHTML = "<span class='tooltiptext lower'>Cancel search</span>";
        let cancelButton = document.createElement("i");
        cancelButton.classList.add("far");
        cancelButton.classList.add("fa-window-close");
        cancelButton.classList.add("fa-lg");
        cancelContainer.appendChild(cancelButton);

        // Add selection logic
        let self = this;
        propertyDrop.addEventListener("change", function() {
            self.changeProperty(propertyDrop.value);
        });
        
        // Append to document
        finderContainer.appendChild(propertyDrop);
        finderContainer.appendChild(cancelContainer);
        finderContainer.appendChild(hideContainer);
        finderContainer.appendChild(confirmContainer);
        finderContainer.appendChild(helpContainer);
        document.body.appendChild(finderContainer);

        // Fire logic for property change
        propertyDrop.dispatchEvent(new Event('change'));
    }

    /**
     * Show search help.
     */
    private showHelp() {

    }

    /**
     * Fired when the target search property is changed, updates
     * subsequent UI components.
     * 
     * @param value newly selected property value
     */
    private changeProperty(value: string) {
        let finderContainer = document.getElementById("finderContainer");

        // Cache the selected property
        this.allSearchProperties.forEach(property => {
            if(property["property"] === value) this.property = property;
        })

        // Initialise the range select if not done before
        let rangeSelect = document.getElementById("finderRangeSelect");
        if(rangeSelect === null || rangeSelect === undefined) {
            rangeSelect = document.createElement("select");
            rangeSelect.id = "finderRangeSelect";
            rangeSelect.classList.add("finderSelectClass");
           
            finderContainer.appendChild(rangeSelect);
        }

        // Find the search term field
        let searchField = document.getElementById("finderField") as HTMLInputElement;

        // Update subsequent UI components as needed
        switch(this.property["type"].toLowerCase()) {
            case "string": {
                rangeSelect.innerHTML = `
                    <option value="contains">contains</option>
                    <option value="starts">starts with</option>
                `;

                if(searchField === null || searchField === undefined) {
                    searchField = document.createElement("input");
                    searchField.id = "finderField";
        
                    finderContainer.appendChild(searchField);
                }
                searchField.style.display = "table-cell";
                searchField.type = "text";
                searchField.value = "";
                searchField.placeholder = "search term";
            }
            break;

            case "number": {
                rangeSelect.innerHTML = `
                    <option value="equals">equals</option>
                    <option value="lesser">is lesser than</option>
                    <option value="greater">is greater than</option>
                `;

                if(searchField === null || searchField === undefined) {
                    searchField = document.createElement("input");
                    searchField.id = "finderField";
                    finderContainer.appendChild(searchField);
                }
                searchField.style.display = "table-cell";
                searchField.type = "number";
                searchField.value = "0";
                searchField.placeholder = "search term";
            }
            break;

            case "boolean": {
                rangeSelect.innerHTML = `
                    <option value="true">is true</option>
                    <option value="false">is false</option>
                `;

                if(searchField !== null && searchField !== undefined) {
                    searchField.style.display = "none";
                }
            }
            break;
        }
    }

    /**
     * Starts the searching process by identifying the input search term
     * before calling implementation specific methods.
     */
    private startSearch() {
        switch(this.property["type"]) {
            case "string":
            case "number":  {
                let finderField = document.getElementById("finderField") as HTMLInputElement;
                if(finderField == null) return;
        
                let searchTerm = finderField.value;
                this.runSearch(searchTerm);
            }
            break;
       
            case "boolean": {
                let rangeSelect = document.getElementById("finderRangeSelect") as HTMLInputElement;
                if(rangeSelect.value === "true") {
                    this.runSearch(true);
                } else {
                    this.runSearch(false);
                }
            }
            break;
        }
    }

    /**
     * Execute the search functionality with the mapping library.
     * 
     * @param searchTerm string, number, or boolean
     */
    public abstract runSearch(searchTerm: string | number | boolean);

    /**
     * Cancel the current search.
     */
    public abstract cancelSearch();
}

