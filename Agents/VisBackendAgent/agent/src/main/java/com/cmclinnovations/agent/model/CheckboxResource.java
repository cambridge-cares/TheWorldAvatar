package com.cmclinnovations.agent.model;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Serves as a data model for a checkbox component.
 */
public class CheckboxResource extends UIResource {
  public static final String CONFIG_TYPE = "checkbox";

  /**
   * Standard constructor with the specified label.
   * 
   * @param label the public facing label for the associated group of inputs.
   */
  public CheckboxResource(String label) {
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
    if (queryNode.size() != 1) {
      throw new IllegalArgumentException(
          "Invalid number of configuration elements for checkbox! Each checkbox element can only have one configuration.");
    }
    this.queries.offer(queryNode);
  }

  /**
   * Transform the resource into a JSON compliant HTTP response.
   */
  @Override
  public JsonNode toJsonResponse() {
    ObjectNode checkboxJson = this.objMapper.createObjectNode();
    checkboxJson.put("label", this.getLabel());
    checkboxJson.put("type", this.getType());
    ArrayNode options = this.objMapper.createArrayNode();
    while (this.hasNextQuery()) {
      options.add(this.getNextQuery());
    }
    checkboxJson.set("options", options);
    return checkboxJson;
  }
}