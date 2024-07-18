/**
 * This is a page wide overlay that hides the content with a black filter.
*/
class Overlay {
  protected overlay: HTMLElement;
  private readonly id_overlay: string = "overlay";

  /**
    * Create a new HTML element of the overlay.
  */
  constructor() {
    this.overlay = createDiv({ id: this.id_overlay });
    this.overlay.style.position = "fixed";
    this.overlay.style.top = "0";
    this.overlay.style.left = "0";
    this.overlay.style.height = "100vh%";
    this.overlay.style.width = "100vw";
    this.overlay.style.background = "rgba(0, 0, 0, 0.8)";
    this.overlay.style.zIndex = "1000";
    this.hide();
  };

  /**
   * Shows the overlay when called.
   */
  public show(): void {
    this.overlay.style.display = "block";
  };

  /**
   * Hides the overlay when called.
   */
  public hide(): void {
    this.overlay.style.display = "none";
  };

  /**
   * Renders this content onto the webpage.
   * @param {HTMLElement} parentElement - The parent element to append this component to.
   */
  public render(parentElement: HTMLElement): void {
    parentElement.appendChild(this.overlay);
  };
};