interface SelectOption {
  value: string;
  label: string;
}

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
  */
  constructor(parameterName: string) {
    // Create a new container item
    this.container = createDiv();
    // Create a button that can be clicked to reveal the list of dropdown options
    let dropdownButtonElement: HTMLElement = createHTMLElement("button");
    dropdownButtonElement.textContent = "Select " + parameterName;
    // Add a dropdown option list
    this.dropdown_options_container = createDiv();
    // Retrieve all available option values
    let options: SelectOption[] = this.retrieveOptionValues();
    // Add the options to the container
    options.forEach((option) => {
      this.addOption(parameterName, option.value, option.label);
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
     * Retrieve an array of option values. Will be refactored to retrieve values from the knowledge graph.
      * @returns {SelectOption[]} An array of SelectOption.
     */
  private retrieveOptionValues(): SelectOption[] {
    return [
      { value: "Automiele", label: "Special Automeile Area" },
      { value: "Industry", label: "Industrial Area" },
    ];
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