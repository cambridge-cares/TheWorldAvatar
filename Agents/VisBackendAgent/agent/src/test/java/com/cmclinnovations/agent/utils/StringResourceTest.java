package com.cmclinnovations.agent.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.cmclinnovations.agent.service.FileServiceTest;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

class StringResourceTest {
  private static final String SAMPLE_FIELD_NAME = "name";
  private static final String SAMPLE_FIELD_AGE = "age";
  private static final String TEST_DEFAULT = "default";
  private static final String TEST_NAME = "John";
  private static final String TEST_AGE = "50";

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  @Test
  void testGetNodeString_Success() {
    // Set up
    JsonNode sample = genJsonNode(TEST_NAME, TEST_AGE);
    // Execute and assert
    assertEquals(TEST_NAME, StringResource.getNodeString(sample, SAMPLE_FIELD_NAME),
        "If the name field is available, the method should retrieve its text contents.");
    assertEquals(TEST_AGE, StringResource.getNodeString(sample, SAMPLE_FIELD_AGE),
        "If the age field is available, the method should retrieve its text contents.");
  }

  @Test
  void testGetNodeString_MissingField() throws Exception {
    // Set up
    JsonNode sample = OBJECT_MAPPER.createObjectNode();
    // Execute and assert
    assertEquals("", StringResource.getNodeString(sample, SAMPLE_FIELD_NAME),
        "A missing field should return an empty string.");
  }

  @Test
  void testOptNodeString_FieldPresent() throws Exception {
    // Set up
    JsonNode sample = genJsonNode(TEST_NAME, null);
    // Execute and assert
    assertEquals(TEST_NAME, StringResource.optNodeString(sample, SAMPLE_FIELD_NAME, TEST_DEFAULT),
        "If a valid field is available, the method should retrieve its text contents.");
  }

  @Test
  void testOptNodeString_MissingField() throws Exception {
    // Set up
    JsonNode sample = genJsonNode(TEST_NAME, null);
    // Execute and assert
    assertEquals("", StringResource.optNodeString(sample, SAMPLE_FIELD_AGE, null),
        "A missing field should return an empty string without any default value set");
  }

  @Test
  void testOptNodeString_MissingFieldWithDefault() throws Exception {
    // Set up
    JsonNode sample = genJsonNode(TEST_NAME, null);
    // Execute and assert
    assertEquals(TEST_DEFAULT, StringResource.optNodeString(sample, SAMPLE_FIELD_AGE, TEST_DEFAULT),
        "A missing field should return the default value when set");
  }

  @Test
  void testGetLocalName() {
    assertEquals(FileServiceTest.IRI_LOCAL_NAME_TEST_CASE1,
        StringResource.getLocalName(FileServiceTest.IRI_TEST_CASE1));
    assertEquals(FileServiceTest.IRI_LOCAL_NAME_TEST_CASE2,
        StringResource.getLocalName(FileServiceTest.IRI_TEST_CASE2));
    assertEquals(FileServiceTest.IRI_LOCAL_NAME_TEST_CASE3,
        StringResource.getLocalName(FileServiceTest.IRI_TEST_CASE3));
  }

  /**
   * Generate a sample json node with optional name and age parameters.
   *
   * @param name Optional value for the name field.
   * @param age  Optional value for the age field.
   */
  private static ObjectNode genJsonNode(String name, String age) {
    ObjectNode jsonNode = OBJECT_MAPPER.createObjectNode();
    if (name != null) {
      jsonNode.put(SAMPLE_FIELD_NAME, name);
    }
    if (age != null) {
      jsonNode.put(SAMPLE_FIELD_AGE, age);
    }
    return jsonNode;
  }
}
