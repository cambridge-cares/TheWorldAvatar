package com.cmclinnovations.agent.utils;

import java.net.URI;
import java.net.URISyntaxException;

public class StringResource {
  // Private constructor to prevent instantiation
  private StringResource() {
    throw new AssertionError("This class cannot be instantiated!");
  }

  /**
   * Get local name of the IRI for namespaces containing # or /.
   * 
   * @param iri Input.
   */
  public static String getLocalName(String iri) {
    if (isValidIRI(iri)) {
      int index = iri.indexOf("#");
      if (index != -1) {
        return iri.substring(index + 1);
      }
      String[] parts = iri.split("/");
      return parts[parts.length - 1];
    }
    return iri;
  }

  /**
   * Validates if the input is an IRI or not.
   * 
   * @param iri Input.
   */
  private static boolean isValidIRI(String iri) {
    try {
      URI uri = new URI(iri);
      // Check if the URI has valid scheme, path, etc
      return uri.getScheme() != null && uri.getHost() != null;
    } catch (URISyntaxException e) {
      // If a URISyntaxException is thrown, the string is not a valid IRI
      return false;
    }
  }
}