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
    this.container = createDiv({ id: this.id_container });
    this.container_content = createDiv({ id: this.id_content });
    // Render the core container with a title and nested content container
    this.renderCoreStructure(title);
  };

  /**
    * Render the common structure.
    * @param {string} title - The title displayed.
  */
  private renderCoreStructure(title: string): void {
    // Create a title element
    let titleElement: HTMLElement = createHTMLElement("button", { id: this.id_title });
    titleElement.textContent = title;
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