class PanelHandler {
  /**
   * Fetches the stack URL from the server and modifies the path segment.
   * This is used to construct the base URL for subsequent API calls.
   *
   * @returns {Promise<string>} A promise that resolves to the modified stack URL.
   * @throws {Error} Throws an error if the network response is not OK or if fetching the stack data fails.
   */

  static async fetchStack(): Promise<string> {
    try {
      const response = await fetch("/api/visualisation/data");
      if (!response.ok) {
        throw new Error("Network response was not ok");
      }
      const data = await response.json();
      const modifiedStack = data.stack.replace('central', 'water');
      return modifiedStack;
    } catch (error) {
      console.error("Failed to fetch stack:", error);
      throw new Error("Failed to fetch stack data");
    }
  }

  /**
   * Fetches supporting data for a given feature based on its IRI and a specified scenario ID.
   * This data includes, but is not limited to, metadata and timeseries data related to the feature.
   *
   * @param {any} feature The feature object, expected to contain an 'iri' property.
   * @param {string} scenarioID A scenario identifier used in constructing the request URL.
   * @returns {Promise<any | null>} A promise that resolves to the fetched data or null if an error occurs or required information is missing.
   */
  static async addSupportingData(feature: any, scenarioID: string): Promise<any> {
    const iri = feature?.iri;

    if (!iri) {
      console.error("Feature is missing required information (IRI).");
      return null;
    }

    const stack = await PanelHandler.fetchStack(); 
    if (!stack) {
      console.error("Failed to obtain stack information.");
      return null;
    }

    const agentURL = `${stack}/CReDoAccessAgent/getMetadataPrivate/${scenarioID}?iri=${encodeURIComponent(iri)}`;
    try {
      const response = await fetch(agentURL);
      if (!response.ok) {
        throw new Error(`HTTP error! Status: ${response.status}`);
      }
      const data = await response.json();
      return data; 
    } catch (error) {
      console.error("Failed to fetch feature info:", error);
      return null;
    }
  }

  // Placeholder methods for future implementation
  private validateAndHandleResponse(rawJSON: any): void {
    // Validates and processes the raw JSON response from the API.
  }

  private updateMetadataUI(meta: any): void {
    // Updates the UI with the fetched metadata information.
  }
}

export default PanelHandler;
