// The configuration options for the component.
interface ConfigurationOptions {
  /**
   * The base url for the stack. Typically http://domain:port
   * @type {string}
   */
  stackUrl: string;

  /**
   * The SPARQL namespace containing the land plot triples.
   * @type {string}
   */
  plotNamespace: string;

  /**
   * The ID name of the VF Mapbox layer to set filters on. Eg 0.0.plot
   * @type {string}
   */
  layerId: string;
}

/**
 * This component provides a list of parameters that users can select to search for the related urban entities visualised in the TWA-VF.
 */
class SeachEntityComponent extends DynamicComponent {
  private options: ConfigurationOptions;
  private numerical_placeholder_message: string = "Type a number";
  private loader: Loader;
  private overlay: Overlay;
  private INVALID_INPUT_MESSAGE: string = "Invalid input. Please enter a numerical value.";

  /**
   * Create a new HTML element to support the application requirements.
   * @param {string} title - The title displayed.
   * @param {any} mapboxMapHandler - The map object created for Mapbox.
  */
  constructor(title: string, mapboxMapHandler: any, options: ConfigurationOptions) {
    // Call the super class constructor
    super(title);
    this.options = options;
    this.initContainerAttributes();
    this.loader = new Loader();
    this.overlay = new Overlay();
    this.loader.render(this.container);
    this.overlay.render(this.container);
    let parentElement: HTMLElement = this.container_content;
    // Create a dropdown component for zone types
    new SelectDropdownComponent("Zone Type", this.options.stackUrl, options.plotNamespace).render(parentElement);
    // Create a text input component for site area
    let siteAreaTextInput: SearchTextInputComponent = new SearchTextInputComponent("Plot Area [m2]", this.numerical_placeholder_message, this.INVALID_INPUT_MESSAGE);
    siteAreaTextInput.render(parentElement);
    // Create a text input component for GFA
    let gfaTextInput: SearchTextInputComponent = new SearchTextInputComponent("Gross Floor Area [m2]", this.numerical_placeholder_message, this.INVALID_INPUT_MESSAGE);
    gfaTextInput.render(parentElement);
    // Create a container for the action buttons
    let buttonContainer: HTMLElement = createDiv({ classes: ["line-item", "evenly-space-container"] });
    // Create a submit button
    let submitButton: HTMLButtonElement = <HTMLButtonElement>createHTMLElement("button", { classes: ["control-button"] });
    submitButton.textContent = "Submit";
    submitButton.addEventListener("click", () => this.handleSubmit(mapboxMapHandler, this.options.layerId, [siteAreaTextInput, gfaTextInput]));
    buttonContainer.appendChild(submitButton);
    // Create a clear all button
    let clearButton: HTMLButtonElement = <HTMLButtonElement>createHTMLElement("button", { classes: ["control-button"] });
    clearButton.textContent = "Clear inputs";
    clearButton.addEventListener("click", () => this.handleClear());
    buttonContainer.appendChild(clearButton);
    parentElement.appendChild(buttonContainer);
  };

  /**
   * Override method to ensure the content is rendered before the specified element.
   * @param {HTMLElement} htmlElement - The specified HTML element.
  */
  public override render(htmlElement: HTMLElement): void {
    this.container.appendChild(this.container_title);
    this.container.appendChild(this.container_content);
    // Override this part of the method
    htmlElement.parentNode.insertBefore(this.container, htmlElement);
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
    this.container_content.classList.add("scrollable-content");
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
    try {
      // Retrieve the option for site area search parameter
      let areaInputs: string[] = this.retrieveMinMaxInput(textComponentArray[0], true);
      let gfaInputs: string[] = this.retrieveMinMaxInput(textComponentArray[1], true);
      if (zoneTypes === "''" && areaInputs[0] === "''" && gfaInputs[0] === "''") {
        return;
      }
      // Show the loader and overlay in this order
      this.overlay.show();
      this.loader.show();
      // Define the request parameters
      let params: { subs: string, namespace: string } = {
        subs: `{"minarea":"${areaInputs[0]}", "maxarea":"${areaInputs[1]}", 
        "mingfa":"${gfaInputs[0]}", "maxgfa":"${gfaInputs[1]}", "zonetype":"${zoneTypes}"}`,
        namespace: this.options.plotNamespace
      };
      // Create a variable for this object context so that the fields are still accesible in ajax
      let _self = this;
      // Send the GET request
      $.ajax({
        url: `${this.options.stackUrl}/filter-agent/filter`,
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
          // Hide the loader and overlay in this order
          _self.loader.hide();
          _self.overlay.hide();
        },
        error: (error) => {
          // Handle any errors here
          console.error("Error:", error);
        }
      });
    } catch (error) {
      // Do not do anything if the error message is expected
      if (error.message != this.INVALID_INPUT_MESSAGE) {
        // If it is unexpected, throw it to the user
        throw error;
      }
    }
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
    // If there are no selected options, an empty string should be returned with single quote
    let optionsString: string = "''";
    // When there are options selected, parse them by enclosing them in single quotes
    if (selectedOptions.length > 0) {
      let wrappedValues: string[] = selectedOptions.map(value => `'${value}'`);
      optionsString = wrappedValues.join(','); // Add ',' between each option
    }
    return optionsString;
  };

  /**
   * Retrieve the current min and max inputs from the component.
   * @param {SearchTextInputComponent} component - The component to extract current values.
   * @param {boolean} isNumerical - A boolean indicating whether only numerical values are accepted.
   * @returns {string[]} the current min and max values in this order.
  */
  private retrieveMinMaxInput(component: SearchTextInputComponent, isNumerical: boolean): string[] {
    let currentTexts: string[] = component.getCurrentValues();
    let currentMinVal: string = currentTexts[0];
    let currentMaxVal: string = currentTexts[1];
    // If both values are empty, return an array of empty strings with single quotes
    // These single quotes will end the filter condition earlier in the filter agent to improve query performance
    if (currentMinVal === "" && currentMaxVal === "") {
      return ["''", "''"];
      // If there is only an empty min value, the min value should default to 0
    } else if (currentMinVal === "") {
      currentMinVal = "0";
      // If there is only an empty max value, the max value should default to a very large number
    } else if (currentMaxVal === "") {
      currentMaxVal = "9999999999";
    }

    // Use regex to verify only digits are inside
    let isANumber: boolean = /^\d+$/.test(currentMinVal[0]) && /^\d+$/.test(currentMaxVal[1]);
    // When the text input must be a number and the user input is not
    if (isNumerical && !isANumber) {
      // Invoke the error message
      component.invokeError();
    }
    return [currentMinVal, currentMaxVal]
  };

  /**
   * An event handler for clearing the user inputs.
   * @returns {void}
  */
  private handleClear(): void {
    // Select the dropdown container and uncheck all checked boxes
    let dropdownElement: Element = this.container_content.firstElementChild;
    let checkboxes: NodeList = dropdownElement.querySelectorAll("input[type=\"checkbox\"]:checked");
    checkboxes.forEach((checkbox: HTMLInputElement) => {
      checkbox.checked = false;
    });

    // Select the text input elements and reset them
    let areaTextInputElement: Element = dropdownElement.nextElementSibling;
    this.clearTextInputs(areaTextInputElement);
    let gfaTextInputElement: Element = areaTextInputElement.nextElementSibling;
    this.clearTextInputs(gfaTextInputElement);
  };

  /**
   * An event handler to clear text inputs.
   * @param {Element} textInputElement - The text input element to clear inputs.
   * @returns {void}
  */
  private clearTextInputs(textInputElement: Element): void {
    let inputValues: NodeList = textInputElement.querySelectorAll("input[type=\"text\"]");
    inputValues.forEach((inputValue) => {
      (inputValue as HTMLInputElement).value = "";
    });
  };
};