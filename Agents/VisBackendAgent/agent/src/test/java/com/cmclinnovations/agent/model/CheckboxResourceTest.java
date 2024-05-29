package com.cmclinnovations.agent.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class CheckboxResourceTest {
  private CheckboxResource sampleResource;
  private ObjectNode sampleNode;
  public static final String SAMPLE_LABEL = "Test Checkbox";
  public static final String EXPRESSION_JSON_STRING = "[\"all\",[\">\",[\"get\",\"area\"],1000]]";
  private static final ObjectMapper MAPPER = new ObjectMapper();

  @BeforeEach
  void setUp() throws JsonProcessingException {
    this.sampleResource = new CheckboxResource(SAMPLE_LABEL);
    this.sampleNode = MAPPER.createObjectNode();
    JsonNode sampleExpression = MAPPER.readTree(EXPRESSION_JSON_STRING);
    this.sampleNode.set("expression", sampleExpression);
  }

  @Test
  void testGetLabel() {
    assertEquals(SAMPLE_LABEL, this.sampleResource.getLabel());
  }

  @Test
  void testGetType() {
    assertEquals(CheckboxResource.CONFIG_TYPE, this.sampleResource.getType());
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
    queryNode.put("expression", EXPRESSION_JSON_STRING);
    queryNode.put("extra", EXPRESSION_JSON_STRING);

    // Act & Assert
    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
        () -> this.sampleResource.addQueryConfig(queryNode));
    assertEquals(
        "Invalid number of configuration elements for checkbox! Each checkbox element can only have one configuration.",
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
    assertEquals(CheckboxResource.CONFIG_TYPE, jsonResponse.path("type").asText());
    JsonNode options = jsonResponse.path("options");
    assertTrue(options.isArray());
    assertEquals(1, options.size());
    assertEquals(EXPRESSION_JSON_STRING, options.path(0).path("expression").toString());

  }
}
