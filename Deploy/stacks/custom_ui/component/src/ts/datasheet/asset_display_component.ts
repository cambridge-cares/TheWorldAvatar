/**
 * This component display assets and their datasheet information as cards on TWA-VF.
*/
class AssetDisplayComponent extends DynamicComponent {
  /**
    * Create a new HTML element to support the application requirements.
    * @param {string} title - The title displayed.
  */
  constructor(title: string) {
    // Call the super class constructor
    super(title);
    // Create a new card container
    let card_container: HTMLElement = createDiv({ classes: ["asset-card-container"] });
    // Set up the counter to number each asset
    let counter: number = 1;
    // Create the asset map and then create a card for each asset
    this.createAssetMap().forEach((specs, nameKey) => {
      this.createCardElement(card_container, nameKey, specs, counter);
      counter++;
    });
    // Append the card container to the additional content container
    super.getContent().appendChild(card_container);
  };

  /**
   * Create the asset map to populate the cards
   * @returns {Map<string, string[]>} the asset map.
   */
  private createAssetMap(): Map<string, string[]> {
    let assetMap: Map<string, string[]> = new Map<string, string[]>();
    let specs: string[] =
      ["Monocrystalline", "415 - 430 Wp", "21.25 ~ 22.02%",
        "154€", "Axitec Energy", "1722 x 1134 x 30 mm", "21.8kg"];
    assetMap.set("AXIperfect FXXL WB", specs);
    specs =
      ["TOPCon", "415 - 425 Wp", "21.25 ~ 21.76%",
        "168€", "Sonnex Energie", "1722 x 1134 x 30 mm", "25.4kg"];
    assetMap.set("Sonnex Energie Bifacial Dual Glass Black-NEX Series", specs);
    specs =
      ["TOPCon", "420 Wp", "21.5%",
        "105€", "Solar Hero", "1722 x 1134 x 30 mm", "23.7kg"];
    assetMap.set("HERO M10-16BB N-Type Bifacial", specs);
    specs =
      ["PERC", "395 - 415 Wp", "20.23  21.25 % ",
        "73€", "Dr. Grob Energy", "1722 x 1134 x 30 mm", "24.5kg"];
    assetMap.set("DGJMO-08", specs);
    return assetMap;
  }

  /**
   * Create a card element for an asset with its name and specifications. 
   * The element will be appended to the parent HTML element.
   * @returns {void}
   */
  private createCardElement(parentElement: HTMLElement, assetName: string, assetSpecifications: string[], counter: number): void {
    // Create a new card element
    let card: HTMLElement = createDiv({ classes: ["asset-card"] });
    // Create a new title element from the asset name input
    let cardTitle: HTMLElement = createDiv({ classes: ["json-key", "asset-card-line"] });
    cardTitle.style.borderBottom = "1px solid #d3d3d3";
    cardTitle.style.paddingBottom = "0.3em";
    cardTitle.textContent = counter + ") " + assetName;
    // Append the title into the card so that it will always appear first
    card.appendChild(cardTitle);
    // Generate card line according to each specification
    this.genCardLine(card, "Type", assetSpecifications[0]);
    this.genCardLine(card, "Performance", assetSpecifications[1]);
    this.genCardLine(card, "Panel efficiency", assetSpecifications[2]);
    this.genCardLine(card, "Cost per module", assetSpecifications[3]);
    this.genCardLine(card, "Vendor", assetSpecifications[4]);
    this.genCardLine(card, "Dimensions (L x W x H)", assetSpecifications[5]);
    this.genCardLine(card, "Weight", assetSpecifications[6]);

    // Add the created card element to the parent
    parentElement.appendChild(card);
  };

  /**
   * Generate the card line with the required content and append it to the card.
   * @returns {void}
   */
  private genCardLine(card: HTMLElement, assetInfoCategory: string, assetInfo: string): void {
    // Initialise new HTML elements
    let cardLine: HTMLElement = createDiv({ classes: ["asset-card-line"] }); // container for the entire text
    let emptySpace: HTMLElement = createHTMLElement("span", { classes: ["empty-icon"] }); // to realign the starting position of text
    let category: HTMLElement = createHTMLElement("span", { classes: ["json-key"] }); // category of this information
    let separator: HTMLElement = createHTMLElement("span", { classes: ["json-separator"] });
    let information: HTMLElement = createHTMLElement("span", { classes: ["json-value"] });
    //Add the classes and their text content accordingly
    category.textContent = assetInfoCategory;
    separator.textContent = ":";
    information.textContent = assetInfo;
    // Append these into the card in the right order
    cardLine.appendChild(emptySpace);
    cardLine.appendChild(category);
    cardLine.appendChild(separator);
    cardLine.appendChild(information);
    card.appendChild(cardLine);
  };
};