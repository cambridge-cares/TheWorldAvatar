/**
 * Creates a new div element that displays an error message.
 * @param {string} errorMessage - The error message to display.
 * @returns {HTMLDivElement} The newly created error element.
*/
function createErrorMessageElement(errorMessage: string): HTMLDivElement {
  let errorElement: HTMLDivElement = createDiv();
  errorElement.textContent = errorMessage;
  errorElement.style.color = "red";
  // Initially, hide the error message
  errorElement.style.display = "none";
  return errorElement;
};

/**
 * Creates a new div element with an optional id attribute and/or optional classes.
 * @param {Object} options - An optional configuration object.
 * @param {string} [options.id] - The id attribute to set for the div element.
 * @param {string[]} [options.classes] - An array of class names to add to the div element.
 * @returns {HTMLDivElement} The newly created div element.
*/
function createDiv(options?: { id?: string; classes?: string[]; }): HTMLDivElement {
  return <HTMLDivElement>createHTMLElement("div", options);
};

/**
 * Create a new HTML element with an optional id attribute and/or optional classes.
 * @param {string} elementType - The HTML element to create.
 * @param {Object} options - An optional configuration object.
 * @param {string} [options.id] - The id attribute to set for the div element.
 * @param {string[]} [options.classes] - An array of class names to add to the div element.
 * @returns {HTMLDivElement} The newly created div element.
*/
function createHTMLElement(elementType: string, options?: { id?: string; classes?: string[]; }): HTMLElement {
  const htmlElement: HTMLElement = document.createElement(elementType);
  // If there are inputs for options,
  if (options) {
    // Set the id if one is available
    if (options.id) { htmlElement.id = options.id; }
    // Set the classes if they are available
    if (options.classes) { htmlElement.classList.add(...options.classes); }
  }
  return htmlElement;
};