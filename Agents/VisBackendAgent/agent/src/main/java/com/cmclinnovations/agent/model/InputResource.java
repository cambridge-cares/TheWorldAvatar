package com.cmclinnovations.agent.model;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Serves as a data model for a input component.
 */
public class InputResource extends UIResource {
  public static final String CONFIG_TYPE = "input";

  /**
   * Standard constructor with the specified label.
   * 
   * @param label the public facing label for the associated group of inputs.
   */
  public InputResource(String label) {
    super(label, CONFIG_TYPE);
  }

  /**
   * Add query configuration.
   * 
   * @param queryNode A configuration node containing three items - label, filter,
   *                  and type.
   */
  @Override
  public void addQueryConfig(JsonNode queryNode) {
    if (queryNode.size() != 3) {
      throw new IllegalArgumentException(
          "Invalid number of configuration elements for input! Each input element can only have three configuration.");
    }
    String inputType = queryNode.path("type").asText();

    if (!(inputType.equals("all") || inputType.equals("number"))) {
      throw new IllegalArgumentException("Invalid input type! Only all and number is allowed.");
    }
    this.queries.offer(queryNode);
  }

  /**
   * Transform the resource into a JSON compliant HTTP response.
   */
  @Override
  public JsonNode toJsonResponse() {
    ObjectNode inputJson = this.objMapper.createObjectNode();
    inputJson.put("label", this.getLabel());
    inputJson.put("type", this.getType());
    ArrayNode options = this.objMapper.createArrayNode();
    while (this.hasNextQuery()) {
      options.add(this.getNextQuery());
    }
    inputJson.set("options", options);
    return inputJson;
  }
}