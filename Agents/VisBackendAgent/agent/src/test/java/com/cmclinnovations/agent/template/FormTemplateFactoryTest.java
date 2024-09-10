package com.cmclinnovations.agent.template;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

class FormTemplateFactoryTest {
  private FormTemplateFactory formTemplateFactory;
  private static ObjectMapper objectMapper;

  @BeforeAll
  static void init() {
    objectMapper = new ObjectMapper();
  }

  @BeforeEach
  void setup() {
    this.formTemplateFactory = new FormTemplateFactory();
  }

  @Test
  void testGenTemplate_EmptyInput() {
    // Set up
    ArrayNode emptyData = objectMapper.createArrayNode();
    // Execute
    Map<String, Object> result = this.formTemplateFactory.genTemplate(emptyData);
    // Assert
    assertTrue(result.isEmpty(), "Template should be empty when input data is empty");
  }

  @Test
  void testGenTemplate_InvalidInput() {
    // Set up
    ArrayNode sample = objectMapper.createArrayNode();
    // Mocking an invalid JSON object that does not have a valid type
    ObjectNode invalidShape = genPropertyShape("testInvalidShape", "invalidType");
    sample.add(invalidShape);
    // Execute & assert
    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
      this.formTemplateFactory.genTemplate(sample);
    });
    assertEquals("Invalid input node! Only property shape and property group is allowed.", exception.getMessage());
  }

  /**
   * Generate a sample property shape.
   *
   * @param id        Identifier value
   * @param typeClass The type of class for this property
   */
  public static ObjectNode genPropertyShape(String id, String typeClass) {
    // Init empty JSON object
    ObjectNode propertyShape = objectMapper.createObjectNode();
    // Add ID
    propertyShape.put(FormTemplateFactory.ID_KEY, id);
    // Add type as "@type:[class]"
    ArrayNode typeValueNode = objectMapper.createArrayNode();
    typeValueNode.add(typeClass);
    propertyShape.set(FormTemplateFactory.TYPE_KEY, typeValueNode);
    // Return object
    return propertyShape;
  }
}
