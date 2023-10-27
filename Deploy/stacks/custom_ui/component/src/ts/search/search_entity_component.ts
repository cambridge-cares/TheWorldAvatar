/**
 * This component provides a list of parameters that users can select to search for the related urban entities visualised in the TWA-VF.
*/
class SeachEntityComponent extends DynamicComponent {
  private readonly baseStackUrl: string = 'http://localhost:3838';
  /**
    * Create a new HTML element to support the application requirements.
    * @param {string} title - The title displayed.
  */
  constructor(title: string) {
    // Call the super class constructor
    super(title);
    this.initContainerAttributes();
    let parentElement: HTMLElement = this.container_content;
    // Create a dropdown component for zone types
    new SelectDropdownComponent("Zone Type").render(parentElement);

    const submitButton = createHTMLElement('button');
    submitButton.textContent = 'Submit';
    submitButton.addEventListener('click', () => this.handleSubmit());

    parentElement.appendChild(submitButton);
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
     * An event handler that retrieves all values selected as inputs for a post request.
      * @returns {void}
     */
  private handleSubmit(): void {
    // Retrieve the options for the zone type search parameter
    let zoneTypes: string = this.retrieveSelectedOptions(this.container_content.firstElementChild);

    // Define the request parameters
    let params: { subs: string } = {
      subs: `{"area":"'null'", "zonetype":"${zoneTypes}"}`
    };

    // Send the GET request
    $.ajax({
      url: `${this.baseStackUrl}/filter-agent/filter`,
      type: 'GET',
      data: params,
      success: function (response) {
        // Retrieved value should be in the form of ["result1", "result2"]
        let plotResults: string[] = response.slice(1, -1)  // Remove the brackets '[' and ']'
          .split(', ');
      },
      error: function (error) {
        // Handle any errors here
        console.error('Error:', error);
      }


    });
  };

  /**
     * Initialises all default container attributes for this component.
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
   * Override method to ensure the content is renderede as the first child node of the parent element.
   * @param {HTMLElement} parentElement - The parent element to append this component to.
   */
  public override render(parentElement: HTMLElement): void {
    this.container.appendChild(this.container_title);
    this.container.appendChild(this.container_content);
    // oV
    parentElement.insertBefore(this.container, parentElement.firstChild);
  };
};