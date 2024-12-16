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

        let attrContainer = document.getElementById("attributionContainer");

        if(finderContainer.style.display === "table") {
            finderContainer.style.display = "none";

            // Re-enable attributions if content was set
            if(attrContainer != null && Manager.SETTINGS.getSetting("attribution") != null) {
                attrContainer.style.display = "block";
            }
            this.cancelSearch();
            
        } else {
            finderContainer.style.display = "table";

            // Hide the attributions container
            if(attrContainer != null) attrContainer.style.display = "none";

            // Temporarily turn of location clustering
            this.turnOffClustering();

            // Cache any existing filters
            this.cacheExisting();
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
        let self = this;

        let finderContainer = document.getElementById("finderContainer");
        if(finderContainer !== null) return;

        // Build container
        finderContainer = document.createElement("div");
        finderContainer.id = "finderContainer";
        finderContainer.style.display = "none";

        // Build label
        let finderLabel = document.createElement("label");
        finderLabel.id = "finderLabel";
        finderLabel.innerHTML = "Find locations where:";
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

        let cancelContainer = document.createElement("div");
        cancelContainer.classList.add("tooltip");
        cancelContainer.classList.add("searchButton");
        cancelContainer.addEventListener("click", () => {
            this.cancelSearch()

            // Hide the container
            let finderContainer = document.getElementById("finderContainer");
            finderContainer.style.display = "none";

            // Re-enable attributions if content was set
            let attrContainer = document.getElementById("attributionContainer");
            if(attrContainer != null && Manager.SETTINGS.getSetting("attribution") != null) {
                attrContainer.style.display = "block";
            }

            // Turn location clustering back on
            this.turnOnClustering();
        });
        cancelContainer.innerHTML = "<span class='tooltiptext lower'>Cancel search</span>";
        let cancelButton = document.createElement("i");
        cancelButton.classList.add("far");
        cancelButton.classList.add("fa-window-close");
        cancelButton.classList.add("fa-lg");
        cancelContainer.appendChild(cancelButton);

        // Add selection logic
        propertyDrop.addEventListener("change", function() {
            self.changeProperty(propertyDrop.value);
            self.startSearch();
        });
        
        // Append to document
        finderContainer.appendChild(propertyDrop);
        finderContainer.appendChild(cancelContainer);
        document.body.appendChild(finderContainer);

        // Fire logic for property change
        propertyDrop.dispatchEvent(new Event('change'));
    }

    /**
     * Fired when the target search property is changed, updates
     * subsequent UI components.
     * 
     * @param value newly selected property value
     */
    private changeProperty(value: string) {
        let self = this;
        let finderContainer = document.getElementById("finderContainer");

        if(value == null) {
            this.property = this.allSearchProperties[0];
        } else {
            // Cache the selected property
            this.allSearchProperties.forEach(property => {
                if(property["property"] === value) this.property = property;
            })
        }

        // Initialise the range select if not done before
        let rangeSelect = document.getElementById("finderRangeSelect");
        if(rangeSelect === null || rangeSelect === undefined) {
            rangeSelect = document.createElement("select");
            rangeSelect.id = "finderRangeSelect";
            rangeSelect.classList.add("finderSelectClass");
            rangeSelect.addEventListener("change", function() {
                self.startSearch();
            });
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
                
                searchField.addEventListener('input', function() {
                    self.startSearch();
                });
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

        // Focus on the search field
        if(searchField !== null && searchField !== undefined) {
            searchField.focus();
        }
    }

    /**
     * Starts the searching process by identifying the input search term
     * before calling implementation specific methods.
     */
    private startSearch() {
        let type = this.property["type"];
        
        switch(type) {
            case "string":
            case "number":  {
                let finderField = document.getElementById("finderField") as HTMLInputElement;
                if(finderField == null) return;
        
                let searchTerm = finderField.value;
                this.runSearch(searchTerm, type);
            }
            break;
       
            case "boolean": {
                let rangeSelect = document.getElementById("finderRangeSelect") as HTMLInputElement;
                if(rangeSelect.value === "true") {
                    this.runSearch(true, type);
                } else {
                    this.runSearch(false, type);
                }
            }
            break;
        }
    }

    /**
     * Cache any existing filters when a search starts.
     */
    public abstract cacheExisting() ;

    /**
     * Execute the search functionality with the mapping library.
     * 
     * @param searchTerm string, number, or boolean
     * @param type serach term type
     */
    public abstract runSearch(searchTerm, type);

    /**
     * Cancel the current search.
     */
    public abstract cancelSearch();

    /**
     * Where applicable, disable any location clustering.
     */
    public abstract turnOffClustering();

    /**
     * Where applicable, re-enable any location clustering.
     */
    public abstract turnOnClustering();
}

