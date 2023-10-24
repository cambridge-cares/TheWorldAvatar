/**
 * Creates a new div element with an optional id attribute and/or optional classes.
 * @param {Object} options - An optional configuration object.
 * @param {string} [options.id] - The id attribute to set for the div element.
 * @param {string[]} [options.classes] - An array of class names to add to the div element.
 * @returns {HTMLDivElement} The newly created div element.
*/
function createDiv(options?: { id?: string; classes?: string[]; }): HTMLDivElement {
  const divElement = document.createElement("div");
  // If there are inputs for options,
  if (options) {
    // Set the id if one is available
    if (options.id) { divElement.id = options.id; }
    // Set the classes if they are available
    if (options.classes) { divElement.classList.add(...options.classes); }
  }
  return divElement;
};