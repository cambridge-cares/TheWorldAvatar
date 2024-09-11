package com.cmclinnovations.agent.utils;

import java.net.URI;
import java.net.URISyntaxException;

import com.fasterxml.jackson.databind.JsonNode;

public class StringResource {
  // Private constructor to prevent instantiation
  private StringResource() {
    throw new UnsupportedOperationException("This class cannot be instantiated!");
  }

  /**
   * Get a string from the specified field name of the input field.
   * 
   * @param field     object containing the string.
   * @param fieldName target field key.
   */
  public static String getNodeString(JsonNode field, String fieldName) {
    return optNodeString(field, fieldName, null);
  }

  /**
   * Get an optional string from the input field.
   * 
   * @param field         object containing the string.
   * @param fieldName     target field key.
   * @param defaultOption default value if there is no such field.
   */
  public static String optNodeString(JsonNode field, String fieldName, String defaultOption) {
    JsonNode fieldNode = field.path(fieldName);
    if (fieldNode.isMissingNode()) {
      if (defaultOption == null) {
        return "";
      }
      return defaultOption;
    }
    return fieldNode.asText();
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