package com.cmclinnovations.agent.model;

import java.util.ArrayDeque;
import java.util.Queue;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Serves as a data model for a dropdown component.
 */
public class DropdownResource extends UIResource {
  public final boolean isRequired;
  private final Queue<JsonNode> options;
  public static final String CONFIG_TYPE = "dropdown";
  private static final Logger LOGGER = LogManager.getLogger(DropdownResource.class);

  /**
   * Standard constructor with the specified label.
   * 
   * @param label      the public facing parameter name for the dropdown.
   * @param isRequired a boolean indicating if the dropdown must include.
   */
  public DropdownResource(String label, boolean isRequired) {
    super(label, CONFIG_TYPE);
    this.isRequired = isRequired;
    this.options = new ArrayDeque<>();
  }

  /**
   * Add dropdown option config.
   * 
   * @param group The group that the value belongs to.
   * @param value The value of the dropdown option.
   */
  public void addOption(String group, String value) {
    ObjectNode option = this.objMapper.createObjectNode();
    option.put("group", group);
    option.put("value", value);
    this.options.offer(option);
  }

  /**
   * Add query configuration.
   * 
   * @param queryNode A configuration node containing two items - the file
   *                  location and namespace.
   */
  @Override
  public void addQueryConfig(JsonNode queryNode) {
    if (queryNode.size() != 2) {
      throw new IllegalArgumentException(
          "Invalid number of configuration elements for dropdown! Each dropdown element can only have two configuration.");
    }
    // Transforms any relative file path of sparql files to the query directory
    String fileLocation = queryNode.path("file").asText();
    if (!(fileLocation.startsWith(".") || fileLocation.startsWith("/"))) {
      ((ObjectNode) queryNode).put("file", StringResource.QUERY_DIR + fileLocation);
    }
    this.queries.add(queryNode);
  }

  /**
   * Transform the resource into a JSON compliant HTTP response.
   */
  @Override
  public JsonNode toJsonResponse() {
    ObjectNode dropdownJson = this.objMapper.createObjectNode();
    // Only generate a configuration if this is valid.
    if (this.options.isEmpty()) {
      LOGGER.info("No dropdown values are retrieved for {} and will be ignored.", this.getLabel());
    } else {
      dropdownJson.put("label", this.getLabel());
      dropdownJson.put("type", this.getType());
      dropdownJson.put("isRequired", this.isRequired);
      ArrayNode optionsNode = this.objMapper.createArrayNode();
      while (!this.options.isEmpty()) {
        optionsNode.add(this.options.poll());
      }
      dropdownJson.set("options", optionsNode);
    }
    return dropdownJson;
  }
}