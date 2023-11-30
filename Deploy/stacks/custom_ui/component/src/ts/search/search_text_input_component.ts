/**
 * This component creates a text input element that allows user to type their search parameters.
*/
class SearchTextInputComponent {
  private readonly container: HTMLDivElement;
  private readonly min_text_input_element: HTMLInputElement;
  private readonly max_text_input_element: HTMLInputElement;
  private readonly error_message_element: HTMLDivElement;

  /**
    * Create a new text input field for the specified parameter.
    * @param {string} parameterName - The name of the parameter of interest.
    * @param {string} placeholderText - The placeholder text.
    * @param {string} errorMessage - An optional error message for displaying errors. If no error is required, please ignore this.
  */
  constructor(parameterName: string, placeholderText: string, errorMessage: string = "") {
    // Create a new container item
    this.container = createDiv({ classes: ["line-padding"] });
    // Create a name display for the search parameter
    let searchParameterName: HTMLElement = createDiv();
    searchParameterName.textContent = parameterName;

    // Create a minimum container with a label and input
    let minContainer: HTMLElement = createDiv({ classes: ["line-item", "space-around-container"] });
    let minLabel: HTMLElement = createDiv();
    minLabel.innerHTML = "Min:"
    // Create a text input element to allow users to type their requirements
    this.min_text_input_element = this.genInputElement(placeholderText);

    // Create a maximum container with a label and input
    let maxContainer: HTMLElement = createDiv({ classes: ["line-item", "space-around-container"] });
    let maxLabel: HTMLElement = createDiv();
    maxLabel.innerHTML = "Max:"
    this.max_text_input_element = this.genInputElement(placeholderText);
    // Append the new elements to the containers
    minContainer.appendChild(minLabel);
    minContainer.appendChild(this.min_text_input_element);
    maxContainer.appendChild(maxLabel);
    maxContainer.appendChild(this.max_text_input_element);
    this.container.appendChild(searchParameterName);
    this.container.appendChild(minContainer);
    this.container.appendChild(maxContainer);
    // If there is an error message, create the error element
    if (errorMessage.length > 0) {
      this.error_message_element = createErrorMessageElement(errorMessage);
      this.container.appendChild(this.error_message_element);
    }
  };

  /**
   * Retrieves the current min and max value as an array.
  */
  public getCurrentValues(): string[] {
    return [this.min_text_input_element.value, this.max_text_input_element.value];
  };

  /**
   * Invoke the error message when required. The message will disappear after 5 seconds.
  */
  public invokeError(): void {
    // Show the error message
    this.error_message_element.style.display = "block";
    console.log(this.error_message_element.textContent);
    // Set a timeout to remove the error message after 5 seconds
    setTimeout(() => {
      this.error_message_element.style.display = "none";
    }, 5000); // 5000 milliseconds = 5 seconds
    throw new Error(this.error_message_element.textContent)
  };

  /**
   * Append this component onto the parent component.
   * @param {HTMLElement} parentElement - The parent element to append this component to.
  */
  public render(parentElement: HTMLElement): void {
    parentElement.appendChild(this.container);
  };

  /**
   * Generate an input element with the specified placeholder text.
   * @param {string} placeholderText - The placeholder text when user has not made any inputs.
   * @returns {HTMLInputElement} the new input element.
  */
  public genInputElement(placeholderText: string): HTMLInputElement {
    let inputElement = <HTMLInputElement>createHTMLElement("input");
    inputElement.type = "text";
    inputElement.placeholder = placeholderText;
    inputElement.value = "";
    inputElement.style.width = "8rem";
    return inputElement;
  };
};