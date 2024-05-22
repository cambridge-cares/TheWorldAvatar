package com.cmclinnovations.agent.model;

import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class InputResourceTest {
  private InputResource sampleResource;
  private ObjectNode sampleNode;
  public static final String SAMPLE_LABEL = "Test Input";
  public static final String SAMPLE_INPUT_LABEL = "Min";
  public static final String SAMPLE_INPUT_FILTER = "minarea";
  public static final String SAMPLE_INPUT_TYPE = "number";
  private static final ObjectMapper MAPPER = new ObjectMapper();

  @BeforeEach
  void setUp() {
    this.sampleResource = new InputResource(SAMPLE_LABEL);
    this.sampleNode = MAPPER.createObjectNode();
    this.sampleNode.put("label", SAMPLE_INPUT_LABEL);
    this.sampleNode.put("filter", SAMPLE_INPUT_FILTER);
    this.sampleNode.put("type", SAMPLE_INPUT_TYPE);
  }

  @Test
  void testGetLabel() {
    assertEquals(SAMPLE_LABEL, this.sampleResource.getLabel());
  }

  @Test
  void testGetType() {
    assertEquals(InputResource.CONFIG_TYPE, this.sampleResource.getType());
  }

  @Test
  void testHasNextQuery_empty() {
    assertFalse(this.sampleResource.hasNextQuery());
  }

  @Test
  void testHasNextQuery_withQuery() {
    // Act
    this.sampleResource.addQueryConfig(this.sampleNode);
    // Assert
    assertTrue(this.sampleResource.hasNextQuery());
  }

  @Test
  void testgetNextQuery() {
    // Act
    this.sampleResource.addQueryConfig(this.sampleNode);
    // Assert
    assertEquals(this.sampleNode, this.sampleResource.getNextQuery());
  }

  @Test
  void testAddQueryConfig_validInput() {
    // Act
    this.sampleResource.addQueryConfig(this.sampleNode);

    // Assert
    assertEquals(1, this.sampleResource.queries.size());
  }

  @Test
  void testAddQueryConfig_invalidNumberOfConfigs() {
    // Arrange
    ObjectNode queryNode = MAPPER.createObjectNode();
    queryNode.put("label", SAMPLE_INPUT_LABEL);

    // Act & Assert
    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
        () -> this.sampleResource.addQueryConfig(queryNode));
    assertEquals(
        "Invalid number of configuration elements for input! Each input element can only have three configuration.",
        exception.getMessage());
  }

  @Test
  void testAddQueryConfig_invalidInputType() {
    // Arrange
    this.sampleNode.put("type", "invalidType");

    // Act & Assert
    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
        () -> this.sampleResource.addQueryConfig(this.sampleNode));
    assertEquals(
        "Invalid input type! Only all and number is allowed.",
        exception.getMessage());
  }

  @Test
  void testToJsonResponse() {
    // Arrange
    this.sampleResource.addQueryConfig(this.sampleNode);

    // Act
    JsonNode jsonResponse = this.sampleResource.toJsonResponse();

    // Assert
    assertEquals(SAMPLE_LABEL, jsonResponse.path("label").asText());
    assertEquals(InputResource.CONFIG_TYPE, jsonResponse.path("type").asText());
    JsonNode options = jsonResponse.path("options");
    assertTrue(options.isArray());
    assertEquals(1, options.size());
  }
}
