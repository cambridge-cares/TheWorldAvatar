package com.cmclinnovations.agent.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class DropdownResourceTest {
  private DropdownResource sampleResource;
  private ObjectNode sampleNode;
  public static final String SAMPLE_LABEL = "Test Dropdown";
  public static final String SAMPLE_FILE_LOCATION = "file.sparql";
  public static final String SAMPLE_NAMESPACE = "kb";
  public static final String SAMPLE_OPTION_GROUP = "Group A";
  public static final String SAMPLE_OPTION_ONE = "First";
  public static final String SAMPLE_OPTION_TWO = "Second";
  public static final boolean SAMPLE_IS_REQUIRED = true;
  private static final ObjectMapper MAPPER = new ObjectMapper();

  @BeforeEach
  void setUp() {
    this.sampleResource = new DropdownResource(SAMPLE_LABEL, SAMPLE_IS_REQUIRED);
    this.sampleNode = MAPPER.createObjectNode();
    this.sampleNode.put("file", SAMPLE_FILE_LOCATION);
    this.sampleNode.put("namespace", SAMPLE_NAMESPACE);
  }

  @Test
  void testGetLabel() {
    // Assert
    assertEquals(SAMPLE_LABEL, this.sampleResource.getLabel());
  }

  @Test
  void testGetType() {
    // Assert
    assertEquals(DropdownResource.CONFIG_TYPE, this.sampleResource.getType());
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
  void testAddOption_addOneOption() {
    // Act
    this.sampleResource.addOption(SAMPLE_OPTION_GROUP, SAMPLE_OPTION_ONE);

    // Assert
    JsonNode jsonResponse = this.sampleResource.toJsonResponse();
    assertEquals(SAMPLE_LABEL, jsonResponse.path("label").asText());
    assertEquals(DropdownResource.CONFIG_TYPE, jsonResponse.path("type").asText());
    assertEquals(SAMPLE_IS_REQUIRED, jsonResponse.path("isRequired").asBoolean());

    JsonNode options = jsonResponse.path("options");
    assertTrue(options.isArray());
    assertEquals(1, options.size());
    assertEquals(SAMPLE_OPTION_GROUP, options.get(0).path("group").asText());
    assertEquals(SAMPLE_OPTION_ONE, options.get(0).path("value").asText());
  }

  @Test
  void testAddOption_addTwoOptions() {
    // Act
    this.sampleResource.addOption(SAMPLE_OPTION_GROUP, SAMPLE_OPTION_ONE);
    this.sampleResource.addOption(SAMPLE_OPTION_GROUP, SAMPLE_OPTION_TWO);

    // Assert
    JsonNode jsonResponse = this.sampleResource.toJsonResponse();
    assertEquals(SAMPLE_LABEL, jsonResponse.path("label").asText());
    assertEquals(DropdownResource.CONFIG_TYPE, jsonResponse.path("type").asText());
    assertEquals(SAMPLE_IS_REQUIRED, jsonResponse.path("isRequired").asBoolean());

    JsonNode options = jsonResponse.path("options");
    assertTrue(options.isArray());
    assertEquals(2, options.size());
    assertEquals(SAMPLE_OPTION_GROUP, options.get(0).path("group").asText());
    assertEquals(SAMPLE_OPTION_ONE, options.get(0).path("value").asText());
    assertEquals(SAMPLE_OPTION_GROUP, options.get(1).path("group").asText());
    assertEquals(SAMPLE_OPTION_TWO, options.get(1).path("value").asText());
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
    this.sampleNode.put("file", SAMPLE_FILE_LOCATION);

    // Act & Assert
    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
        () -> this.sampleResource.addQueryConfig(queryNode));
    assertEquals(
        "Invalid number of configuration elements for dropdown! Each dropdown element can only have two configuration.",
        exception.getMessage());
  }

  @Test
  void testToJsonResponse_emptyResponse() {
    // Act
    JsonNode jsonResponse = this.sampleResource.toJsonResponse();

    // Assert
    assertTrue(jsonResponse.isEmpty());
  }
}
