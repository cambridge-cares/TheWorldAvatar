/**
 * This component creates a dropdown list of values for the user to search on the parameter of interest.
*/
class SelectDropdownComponent {
  private readonly container: HTMLDivElement;
  private readonly dropdown_options_container: HTMLDivElement;
  private option_index: number = 1;

  /**
    * Create a new dropdown list for the specified parameter.
    * @param {string} parameterName - The name of the parameter of interest.
    * @param {string} stackUrl - The base url for the stack. Typically domain:port.
    * @param {string} plotNamespace - The SPARQL namespace containing the land plot triples.
*/
  constructor(parameterName: string, stackUrl: string, plotNamespace: string) {
    // Create a new container item
    this.container = createDiv();
    this.container.classList.add("line-padding");
    // Create a button that can be clicked to reveal the list of dropdown options
    let dropdownButtonElement: HTMLElement = createHTMLElement("button");
    dropdownButtonElement.textContent = "Select " + parameterName;
    // Add a dropdown option list for zone types
    this.dropdown_options_container = createDiv();
    // Retrieve all available zone types
    this.retrieveZoneTypes(stackUrl, plotNamespace)
      .then((zoneTypes: string[]) => {
        // Add the options to the container
        zoneTypes.forEach((option: string) => {
          this.addOption(parameterName, option, option);
        });
      });
    // Event listeners
    // When the button is clicked, display the dropdown options
    dropdownButtonElement.addEventListener('click', () => {
      this.dropdown_options_container.style.display = 'block';
    });

    // Close the list of dropdown options when a user click outside this component
    document.addEventListener('click', (event) => {
      if (!dropdownButtonElement.contains(event.target as Node) && !this.dropdown_options_container.contains(event.target as Node)) {
        this.dropdown_options_container.style.display = 'none'
      }
    });
    // Append the new elements to the containers
    this.container.appendChild(dropdownButtonElement);
    this.container.appendChild(this.dropdown_options_container);
  };

  /**
   * Append this component onto the parent component.
   * @param {HTMLElement} parentElement - The parent element to append this component too.
   */
  public render(parentElement: HTMLElement): void {
    parentElement.appendChild(this.container);
  };

  /**
    * Retrieve an array of zone types to populate as values.
    * @param {string} stackUrl - The base url for the stack. Typically domain:port.
    * @param {string} plotNamespace - The SPARQL namespace containing the land plot triples.
 * @returns {Promise<string[]>} A Promise that resolves to an array of zone type string.
  */
  private async retrieveZoneTypes(stackUrl: string, plotNamespace: string): Promise<string[]> {
    let sparqlQuery: string = `
    PREFIX ontozoning:<https://www.theworldavatar.com/kg/ontozoning/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?zoneType
    WHERE {
      ?zone a ontozoning:Zone;
        ontozoning:hasZoneType/rdfs:label ?zoneType.
    }`;
    let endpoint: string = stackUrl + "/blazegraph/namespace/" + plotNamespace + "/sparql";
    try {
      // Retrieve the sparql results and process it for zone types
      let bindings: SparqlResult[] = await execSparqlQuery(endpoint, sparqlQuery);
      return bindings.map((binding: SparqlResult) => binding.zoneType.value)
        .sort((a, b) => a.localeCompare(b)); // Sort the results
    } catch (error) {
      console.error("Error executing Sparql query:", error);
      throw error;
    }
  };

  /**
     * Add an option to the dropdown options.
      * @param {string} parameterName - The name of the parameter of interest.
      * @param {string} value - The value of the checkbox once checked.
      * @param {string} label - The label name that users will see along the checkbox.
      * @returns {void}
     */
  private addOption(parameterName: string, value: string, label: string): void {
    // Generate an ID for this option based on the parameter name and the current index
    // All parameter names must be simplified with lower case and no white spaces
    // The index should be incremented after use
    let optionId: string = parameterName.trim().replace(/\s/g, "").toLowerCase() + this.option_index++;
    // Create a checkbox element with a value
    let checkBoxElement: HTMLInputElement = <HTMLInputElement>createHTMLElement("input");
    checkBoxElement.type = "checkbox";
    checkBoxElement.id = optionId;
    checkBoxElement.value = value;

    // Create a label for the checkbox element
    let labelElement: HTMLLabelElement = <HTMLLabelElement>createHTMLElement("label", { classes: ["line-item"] });
    labelElement.htmlFor = optionId; // Required to link the label to its input element
    // The following sequence will create the preferred format: [] label
    labelElement.appendChild(checkBoxElement);
    labelElement.appendChild(document.createTextNode(label));

    // Append to the dropdown options container
    this.dropdown_options_container.appendChild(labelElement);
  }
};