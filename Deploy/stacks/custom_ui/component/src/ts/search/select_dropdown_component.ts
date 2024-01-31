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
    this.container = createDiv({ classes: ["line-padding"] });
    // Create a button that can be clicked to reveal the list of dropdown options
    let dropdownButtonElement: HTMLElement = createHTMLElement("button", { classes: ["control-button"] });
    dropdownButtonElement.textContent = "Select " + parameterName;
    // Add a dropdown option list for land use types
    this.dropdown_options_container = createDiv({ classes: ["dropdown-box"] });
    this.dropdown_options_container.style.display = "none";
    // Retrieve all available land use types
    this.retrieveLandUseTypes(stackUrl, plotNamespace)
      .then((sparqlValues: string[]) => {
        // Add the options to the container
        sparqlValues.forEach((option: string) => {
          this.addOption(parameterName, option, option);
        });
      });
    // Event listeners
    dropdownButtonElement.onclick = () => {
      // When the options are hidden, display them on clicking the button
      if (this.dropdown_options_container.style.display === "none") {
        this.dropdown_options_container.style.display = "block";
      } else {
        // When the options are shown, hide them on click the button
        this.dropdown_options_container.style.display = "none";
      }
    };

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
    * Retrieve an array of land use types to populate as values.
    * @param {string} stackUrl - The base url for the stack. Typically domain:port.
    * @param {string} plotNamespace - The SPARQL namespace containing the land plot triples.
 * @returns {Promise<string[]>} A Promise that resolves to an array of land use type string.
  */
  private async retrieveLandUseTypes(stackUrl: string, plotNamespace: string): Promise<string[]> {
    let sparqlQuery: string = `
    PREFIX opr:<https://www.theworldavatar.com/kg/ontoplanningregulation/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?landUseType
    WHERE {
      ?regulation a opr:LandUseRegulation;
        opr:appliesTo/rdfs:label ?landUseType.
    }`;
    let endpoint: string = stackUrl + "/blazegraph/namespace/" + plotNamespace + "/sparql";
    try {
      // Retrieve the sparql results and process it for land use types
      let bindings: SparqlResult[] = await execSparqlQuery(endpoint, sparqlQuery);
      return bindings.map((binding: SparqlResult) => binding.landUseType.value)
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