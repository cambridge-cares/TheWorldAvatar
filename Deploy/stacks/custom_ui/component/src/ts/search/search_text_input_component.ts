/**
 * This component creates a text input element that allows user to type their search parameters.
*/
class SearchTextInputComponent {
  private readonly container: HTMLDivElement;
  private readonly text_input_element: HTMLInputElement;
  private readonly error_message_element: HTMLDivElement;

  /**
    * Create a new text input field for the specified parameter.
    * @param {string} parameterName - The name of the parameter of interest.
    * @param {string} placeholderText - The placeholder text.
    * @param {string} errorMessage - An optional error message for displaying errors. If no error is required, please ignore this.
  */
  constructor(parameterName: string, placeholderText: string, errorMessage: string = "") {
    // Create a new container item
    this.container = createDiv();
    // Create a name display for the search parameter
    let searchParameterName: HTMLElement = createDiv();
    searchParameterName.textContent = parameterName;
    // Create a text input element to allow users to type their requirements
    this.text_input_element = <HTMLInputElement>createHTMLElement("input");
    this.text_input_element.type = "text";
    this.text_input_element.placeholder = placeholderText;
    this.text_input_element.value = "";
    // Append the new elements to the containers
    this.container.appendChild(searchParameterName);
    this.container.appendChild(this.text_input_element);
    // If there is an error message, create the error element
    if (errorMessage.length > 0) {
      this.error_message_element = createErrorMessageElement(errorMessage);
      this.container.appendChild(this.error_message_element);
    }
  };

  /**
   * Retrieves the current value.
  */
  public getCurrentValue(): string {
    return this.text_input_element.value;
  };

  /**
   * Invoke the error message when required. The message will disappear after 5 seconds.
  */
  public invokeError(): void {
    // Show the error message
    this.error_message_element.style.display = "block";
    console.log("Invalid Input: " + this.error_message_element.textContent);
    // Set a timeout to remove the error message after 5 seconds
    setTimeout(() => {
      this.error_message_element.style.display = "none";
    }, 5000); // 5000 milliseconds = 5 seconds
  };

  /**
   * Append this component onto the parent component.
   * @param {HTMLElement} parentElement - The parent element to append this component to.
  */
  public render(parentElement: HTMLElement): void {
    parentElement.appendChild(this.container);
  };
};