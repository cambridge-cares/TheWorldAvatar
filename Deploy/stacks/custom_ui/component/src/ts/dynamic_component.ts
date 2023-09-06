/**
 * This abstract class is a versatile adaptive collapsible container element.
 * It is designed to facilitate a dynamic role-based rendering of its child components,
 * presenting different content and functionalities based on their user role.
*/
abstract class DynamicComponent {
  private readonly container: HTMLElement;
  private readonly container_content: HTMLElement;
  private readonly id_container: string = "dynamicInterfaceContainer";
  private readonly id_title: string = "dynamicComponentTitle";
  private readonly id_content: string = "dynamicContent";

  /**
    * Create a new HTML element with collapsible content to support the dynamic component.
    * @param {string} title - The title displayed.
  */
  constructor(title: string) {
    console.log("Rendering an additional custom UI component...");
    // Initialise new container elements
    this.container = this.createDiv({ id: this.id_container });
    this.container_content = this.createDiv({ id: this.id_content });
    // Render the core container with a title and nested content container
    this.renderCoreStructure(title);
  };

  /**
    * Render the common structure.
    * @param {string} title - The title displayed.
  */
  private renderCoreStructure(title: string): void {
    // Create a title element
    let titleElement: HTMLElement = document.createElement("button");
    titleElement.textContent = title;
    titleElement.setAttribute("id", this.id_title);
    // Add an event for the title, so that on clicking, it will expand the content
    titleElement.addEventListener("click", () => this.toggleContent());
    // Create an empty content container for developers to add later
    this.container_content.style.height = "0";
    // Append these elements in the following order into the container
    this.container.appendChild(titleElement);
    this.container.appendChild(this.container_content);
  };

  /**
   * Toggle the content on and off whenever the collapsible button is clicked.
 */
  private toggleContent(): void {
    if (this.container_content.style.height === "0px") {
      // Height will accommodate content
      this.container_content.style.height = "auto";
      // Hide the text so that the animation is not as jarring
      this.container_content.style.opacity = "1";
    } else {
      this.container_content.style.height = "0";
      this.container_content.style.opacity = "0";
    }
  }

  /**
    * Creates a new div element with an optional id attribute and/or optional classes.
    * @param {Object} options - An optional configuration object.
    * @param {string} [options.id] - The id attribute to set for the div element.
    * @param {string[]} [options.classes] - An array of class names to add to the div element.
    * @returns {HTMLDivElement} The newly created div element.
  */
  public createDiv(options?: { id?: string; classes?: string[]; }): HTMLDivElement {
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

  /**
    * Retrieve the container's HTML content element for further modifications.
    * @return {HTMLElement} The dynamic container's content element.
  */
  public getContent(): HTMLElement { return this.container_content; };

  /**
   * Renders this content onto the webpage.
   * @param {HTMLElement} parentElement - The parent element to append this component too.
   */
  public render(parentElement: HTMLElement): void {
    parentElement.appendChild(this.container);
  };
};