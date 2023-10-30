/**
 * This component provides a list of parameters that users can select to search for the related urban entities visualised in the TWA-VF.
 */
class SeachEntityComponent extends DynamicComponent {
  private baseStackUrl: string;
  private numerical_placeholder_message: string = "Type a number";

  /**
   * Create a new HTML element to support the application requirements.
   * @param {string} title - The title displayed.
   * @param {any} mapboxMapHandler - The map object created for Mapbox.
   * @param {string} layerId - The ID name of the layer to set filters on.
  */
  constructor(title: string, stackUrl: string, mapboxMapHandler: any, layerId: string) {
    // Call the super class constructor
    super(title);
    this.baseStackUrl = stackUrl;
    this.initContainerAttributes();
    let parentElement: HTMLElement = this.container_content;
    // Create a dropdown component for zone types
    new SelectDropdownComponent("Zone Type").render(parentElement);
    // Create a text input component for site area
    let siteAreaTextInput: SearchTextInputComponent = new SearchTextInputComponent("Site Area [m2]", this.numerical_placeholder_message, "Invalid input. Please enter a numerical value.");
    siteAreaTextInput.render(parentElement);
    // Create a submit button
    let submitButton: HTMLButtonElement = <HTMLButtonElement>createHTMLElement('button');
    submitButton.textContent = "Submit";
    submitButton.addEventListener("click", () => this.handleSubmit(mapboxMapHandler, layerId, [siteAreaTextInput]));
    parentElement.appendChild(submitButton);
  };

  /**
   * Override method to ensure the content is rendered as the first child node of the parent element.
   * @param {HTMLElement} parentElement - The parent element to append this component to.
  */
  public override render(parentElement: HTMLElement): void {
    this.container.appendChild(this.container_title);
    this.container.appendChild(this.container_content);
    // Override this part of the method
    parentElement.insertBefore(this.container, parentElement.firstChild);
  };

  /**
   * Initialises all default container attributes for this component.
   * @returns {void}
  */
  private initContainerAttributes(): void {
    // Follows the convention of the other control elements
    this.container.id = "searchContainer";
    this.container.classList.add("controlBlock");
    this.container_title.id = "controlTitle";
    this.container_title.classList.add("controlTitle");
    this.container_content.removeAttribute("id");
    this.container_content.classList.add("controlContents");
  };

  /**
   * An event handler that triggers when a button is clicked. 
   * This event retrieves all search parameter inputs from the the users, 
   * and retrieves the associated plots that meet this criteria. 
   * The map will filter and only show the plots that fit the criteria.
   * @param {any} mapboxMapHandler - The map object created for Mapbox.
   * @param {string} layerId - The ID name of the layer to set filters on.
   * @param {SearchTextInputComponent[]} textComponentArray - An array of text input components.
   * @returns {void}
  */
  private handleSubmit(mapboxMapHandler: any, layerId: string, textComponentArray: SearchTextInputComponent[]): void {
    // Reset the filters
    mapboxMapHandler.setFilter(layerId, null);
    // Retrieve the options for the zone type search parameter
    let zoneTypes: string = this.retrieveSelectedOptions(this.container_content.firstElementChild);
    // Retrieve the option for site area search parameter
    let siteArea: string = this.retrieveTextInput(textComponentArray[0], true);
    // Define the request parameters
    let params: { subs: string } = {
      subs: `{"area":"${siteArea}", "zonetype":"${zoneTypes}"}`
    };

    // Send the GET request
    $.ajax({
      url: `${this.baseStackUrl}/filter-agent/filter`,
      type: "GET",
      data: params,
      success: function (response) {
        // Retrieved response should be in the form of ["result1", "result2"]
        // Parse each value as an array item
        let plotResults: string[] = response.slice(1, -1)  // Remove the brackets '[' and ']'
          .split(', ');
        // If there are plots to filter, set the expression and filter for the specified layer
        // Note that the condition is to detect whether the first object is not an empty string.
        // The filter agent returns an empty string if no plot(s) of interest is found, so checking the length is not viable
        if (plotResults[0].length > 0) {
          // Filter expression
          let zoneFilterExpression: [string, string[], [string, string[]]] = ["in", ['get', 'iri'], ['literal', plotResults]];
          mapboxMapHandler.setFilter(layerId, zoneFilterExpression);
        }
      },
      error: function (error) {
        // Handle any errors here
        console.error("Error:", error);
      }
    });
  };

  /**
   * Retrieve the selected checkbox options from the specified dropdown container.
   * This method will parse the results into a string format suitable for the Filter agent.
   * @param {Element} dropDownContainer - The container element with the input elements to extract data from.
   * @returns {void}
  */
  private retrieveSelectedOptions(dropDownContainer: Element): string {
    // Initialise the array to store the values of each checkbox-option selected
    let selectedOptions: string[] = [];
    // Select all checkboxes that are checked within the container
    let checkboxes: NodeList = dropDownContainer.querySelectorAll('input[type="checkbox"]:checked');    // Iterate through the selected checkboxes
    // For each element, push the selected zone types into the array for storage
    checkboxes.forEach((checkbox: HTMLInputElement) => {
      // Value should correspond to the knowledge graph input
      selectedOptions.push(checkbox.value);
    });

    // Initialise string that should be sent to the Filter Agent
    // If there are no selected options, null should be returned and enclosed in single quotes
    let optionsString: string = "'null'";
    // When there are options selected, parse them by enclosing them in single quotes
    if (selectedOptions.length > 0) {
      let wrappedValues: string[] = selectedOptions.map(value => `'${value}'`);
      optionsString = wrappedValues.join(','); // Add ',' between each option
    }
    return optionsString;
  };

  /**
   * Retrieve the current text input from the component.
   * @param {SearchTextInputComponent} component - The component to extract current value.
   * @param {boolean} isNumerical - A boolean indicating whether only numerical values are accepted.
   * @returns {string} the current text.
  */
  private retrieveTextInput(component: SearchTextInputComponent, isNumerical: boolean): string {
    let currentText: string = component.getCurrentValue();
    // Use regex to verify only digits are inside
    let isANumber: boolean = /^\d+$/.test(currentText);
    // If the current text is an empty string, do not do anything
    if (currentText === "") {
      currentText = "'null'";
      // When the text input must be a number and the user input is not
    } else if (isNumerical && !isANumber) {
      // Set the text to null for meeting the filter agent requirements
      currentText = "'null'";
      // Invoke the error message
      component.invokeError();
    }
    return currentText;
  };
};