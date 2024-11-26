package com.cmclinnovations.agent.template;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.cmclinnovations.agent.utils.ShaclResource;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

class FormTemplateFactoryTest {
  private FormTemplateFactory formTemplateFactory;
  private static ObjectMapper objectMapper;

  private static final String XSD_INTEGER_TYPE = "integer";
  private static final String XSD_STRING_TYPE = "string";

  private static final String STRING_TEST_CASE_ID = "string_id";
  private static final String STRING_TEST_CASE_NAME = "name";
  private static final String STRING_TEST_CASE_DESCRIPTION = "The first name of a person";
  private static final String STRING_TEST_CASE_ORDER = "1";
  private static final String NUMBER_TEST_CASE_ID = "dec_id";
  private static final String NUMBER_TEST_CASE_NAME = "age";
  private static final String NUMBER_TEST_CASE_DESCRIPTION = "The age of a person";
  private static final String NUMBER_TEST_CASE_ORDER = "2";

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
    Map<String, Object> result = this.formTemplateFactory.genTemplate(emptyData, new HashMap<>());
    // Assert
    assertTrue(result.isEmpty(), "Template should be empty when input data is empty");
  }

  @Test
  void testGenTemplate_InvalidInput() {
    // Set up
    ArrayNode sample = objectMapper.createArrayNode();
    // Mocking an invalid JSON object that does not have a valid type
    ObjectNode invalidShape = genPropertyShape("testInvalidShape", "invalidType", "invalid field",
        "This shape should fail", ShaclResource.XSD_PREFIX + XSD_STRING_TYPE, "");
    sample.add(invalidShape);
    // Execute & assert
    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
      this.formTemplateFactory.genTemplate(sample, new HashMap<>());
    });
    assertEquals("Invalid input node! Only property shape and property group is allowed.", exception.getMessage());
  }

  @Test
  void testGenTemplate() {
    // Set up
    ArrayNode sample = objectMapper.createArrayNode();
    ObjectNode firstShape = genPropertyShape(STRING_TEST_CASE_ID,
        ShaclResource.SHACL_PREFIX + ShaclResource.PROPERTY_SHAPE, STRING_TEST_CASE_NAME,
        STRING_TEST_CASE_DESCRIPTION, ShaclResource.XSD_PREFIX + XSD_STRING_TYPE, STRING_TEST_CASE_ORDER);
    sample.add(firstShape);
    ObjectNode secShape = genPropertyShape(NUMBER_TEST_CASE_ID,
        ShaclResource.SHACL_PREFIX + ShaclResource.PROPERTY_SHAPE, NUMBER_TEST_CASE_NAME,
        NUMBER_TEST_CASE_DESCRIPTION, ShaclResource.XSD_PREFIX + XSD_INTEGER_TYPE, NUMBER_TEST_CASE_ORDER);
    sample.add(secShape);
    // Execute
    Map<String, Object> result = this.formTemplateFactory.genTemplate(sample, new HashMap<>());
    // Assert
    assertTrue(result.containsKey("@context"), "Context should be added to the form");
    validatePropertyShape(result, STRING_TEST_CASE_ID, STRING_TEST_CASE_NAME, STRING_TEST_CASE_DESCRIPTION,
        XSD_STRING_TYPE, STRING_TEST_CASE_ORDER);
    validatePropertyShape(result, NUMBER_TEST_CASE_ID, NUMBER_TEST_CASE_NAME, NUMBER_TEST_CASE_DESCRIPTION,
        XSD_INTEGER_TYPE, NUMBER_TEST_CASE_ORDER);
  }

  /**
   * Generate a sample property shape.
   *
   * @param id          Identifier value
   * @param typeClass   The type of class for this property
   * @param name        The name of this property
   * @param description The description for this property
   * @param dataType    The data type of the property, which must correspond to an
   *                    xsd type
   * @param order       A field to arrange the properties
   */
  public static ObjectNode genPropertyShape(String id, String typeClass, String name, String description,
      String dataType, String order) {
    // Init empty JSON object
    ObjectNode propertyShape = objectMapper.createObjectNode();
    // Add ID
    propertyShape.put(ShaclResource.ID_KEY, id);
    // Add type as "@type:[class]"
    ArrayNode typeValueNode = objectMapper.createArrayNode()
        .add(typeClass);
    propertyShape.set(ShaclResource.TYPE_KEY, typeValueNode);
    // Add name as`"sh:name":[{"@value" : "name"}]`
    ObjectNode nameValueNode = objectMapper.createObjectNode()
        .put(ShaclResource.VAL_KEY, name);
    propertyShape.set(ShaclResource.SHACL_PREFIX + ShaclResource.NAME_PROPERTY,
        objectMapper.createArrayNode().add(nameValueNode));
    // Add description as `"sh:description":[{"@value" : "description"}]`
    ObjectNode descriptionValueNode = objectMapper.createObjectNode()
        .put(ShaclResource.VAL_KEY, description);
    propertyShape.set(ShaclResource.SHACL_PREFIX + ShaclResource.DESCRIPTION_PROPERTY,
        objectMapper.createArrayNode().add(descriptionValueNode));
    // Add datatype as `"sh:datatype":[{"@id" : "data type"}]`
    ObjectNode dataTypeValueNode = objectMapper.createObjectNode()
        .put(ShaclResource.ID_KEY, dataType);
    propertyShape.set(ShaclResource.SHACL_PREFIX + ShaclResource.DATA_TYPE_PROPERTY,
        objectMapper.createArrayNode().add(dataTypeValueNode));
    // Add order as`"sh:order":[{"@type": "xsd:type", "@value" : "order"}]`
    ObjectNode orderValueNode = objectMapper.createObjectNode()
        .put(ShaclResource.TYPE_KEY, ShaclResource.XSD_PREFIX + XSD_INTEGER_TYPE)
        .put(ShaclResource.VAL_KEY, order);
    propertyShape.set(ShaclResource.SHACL_PREFIX + ShaclResource.ORDER_PROPERTY,
        objectMapper.createArrayNode().add(orderValueNode));
    // Return object
    return propertyShape;
  }

  /**
   * Verify the generated shape content.
   *
   * @param result      Results for validation
   * @param id          Identifier value
   * @param name        The name of this property
   * @param description The description for this property
   * @param dataType    The data type of the property, which must correspond to an
   *                    xsd type
   * @param order       A field to arrange the properties
   */
  public static void validatePropertyShape(Map<String, Object> result, String id, String name, String description,
      String dataType, String order) {
    List<Map<String, Object>> propertyShapesList = (List<Map<String, Object>>) result
        .get(ShaclResource.PROPERTY_PROPERTY);
    Optional<Map<String, Object>> outputProperty = propertyShapesList.stream()
        .filter(current -> id.equals(current.get(ShaclResource.ID_KEY)))
        .findFirst();
    outputProperty.ifPresentOrElse(
        property -> {
          assertEquals(name,
              ((Map<String, Object>) property.get(ShaclResource.NAME_PROPERTY)).get(ShaclResource.VAL_KEY),
              "Name should match");
          assertEquals(description,
              ((Map<String, Object>) property.get(ShaclResource.DESCRIPTION_PROPERTY))
                  .get(ShaclResource.VAL_KEY),
              "Description should match");
          assertEquals(dataType, property.get(ShaclResource.DATA_TYPE_PROPERTY), "Data type should match");
          assertEquals(order, property.get(ShaclResource.ORDER_PROPERTY).toString(), "Order should match");
        },
        () -> assertTrue(false, "No matching property is found for the id"));
  }
}
