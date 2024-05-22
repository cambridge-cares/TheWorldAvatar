package com.cmclinnovations.agent.model;

import java.util.ArrayDeque;
import java.util.Queue;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Serves as a base resource with common properties for various UI.
 */
public abstract class UIResource {
  private final String label;
  private final String type;
  protected final Queue<JsonNode> queries;
  protected final ObjectMapper objMapper = new ObjectMapper();

  /**
   * Standard constructor with the specified label and type.
   *
   * @param label the label of the UI component.
   * @param type  the type of the UI component.
   */
  protected UIResource(String label, String type) {
    this.label = label;
    this.type = type;
    this.queries = new ArrayDeque<>();
  }

  public String getLabel() {
    return this.label;
  }

  public String getType() {
    return this.type;
  }

  /**
   * Indicates if there are any query parameters in the resource.
   */
  public boolean hasNextQuery() {
    return !this.queries.isEmpty();
  }
  
  /**
   * Retrieve the next query node available.
   */
  public JsonNode getNextQuery() {
    return this.queries.poll();
  }

  /**
   * Add the specified query config for this resource.
   *
   * @param query the configuration to add.
   */
  public abstract void addQueryConfig(JsonNode query);

  /**
   * Transform the resource into a JSON compliant HTTP response.
   */
  public abstract JsonNode toJsonResponse();
}