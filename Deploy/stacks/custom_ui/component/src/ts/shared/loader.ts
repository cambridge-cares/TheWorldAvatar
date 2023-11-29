/**
 * This is a loader screen which can be displayed when retrieving or processing data.
*/
class Loader {
  protected container: HTMLElement;
  protected loader: HTMLElement;
  protected loader_text: HTMLElement;
  private readonly id_container: string = "loaderContainer";
  private readonly id_loader: string = "loader";
  private readonly id_text: string = "loaderText";

  /**
    * Create a new HTML container with a loader and text.
  */
  constructor() {
    this.container = createDiv({ id: this.id_container });
    this.loader = createDiv({ id: this.id_loader });
    this.loader_text = createDiv({ id: this.id_text });
    this.loader_text.textContent = "LOADING";
    this.hide();
  };

  /**
  * Shows the loader when called.
  */
  public show(): void {
    this.container.style.display = "block";
  };

  /**
   * Hides the loader when called.
   */
  public hide(): void {
    this.container.style.display = "none";
  };

  /**
   * Renders this content onto the webpage.
   * @param {HTMLElement} parentElement - The parent element to append this component to.
   */
  public render(parentElement: HTMLElement): void {
    this.container.appendChild(this.loader);
    this.container.appendChild(this.loader_text);
    parentElement.appendChild(this.container);
  };
};