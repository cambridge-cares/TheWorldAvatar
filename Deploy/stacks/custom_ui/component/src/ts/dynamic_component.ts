/**
 * This abstract class is a versatile adaptive collapsible container element.
 * It is designed to facilitate a dynamic role-based rendering of its child components,
 * presenting different content and functionalities based on their user role.
*/
abstract class DynamicComponent {
  protected container: HTMLElement;
  protected container_title: HTMLElement;
  protected container_content: HTMLElement;
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
    this.container = createDiv({ id: this.id_container});
    this.container_content = createDiv({ id: this.id_content });
    // Create a title element
    this.container_title = createDiv({ id: this.id_title });
    this.container_title.textContent = title;
  };

  /**
   * Renders this content onto the webpage.
   * @param {HTMLElement} parentElement - The parent element to append this component to.
   */
  public render(parentElement: HTMLElement): void {
    this.container.appendChild(this.container_title);
    this.container.appendChild(this.container_content);
    parentElement.appendChild(this.container);
  };
};