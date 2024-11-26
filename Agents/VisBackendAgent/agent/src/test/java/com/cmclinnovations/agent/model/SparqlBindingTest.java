package com.cmclinnovations.agent.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class SparqlBindingTest {
  private SparqlBinding sampleBinding;

  public static final String TYPE_BINDING_KEY = "type";
  public static final String DATA_TYPE_BINDING_KEY = "datatype";
  public static final String VAL_BINDING_KEY = "value";
  public static final String LANGUAGE_BINDING_KEY = "xml:lang";

  public static final String FIELD_ONE = "field1";
  public static final String FIELD_VALUE_ONE = "value1";
  public static final String FIELD_TWO = "field2";
  public static final String FIELD_TWO_DATA_TYPE = "http://www.w3.org/2001/XMLSchema#number";
  public static final String FIELD_VALUE_TWO = "value2";
  public static final String FIELD_THREE = "field3";
  public static final String FIELD_THREE_LANGUAGE = "en";
  public static final String FIELD_VALUE_THREE = "value3";

  public static final String FIELD_TYPE_LITERAL = "literal";
  public static final String FIELD_TYPE_URI = "uri";
  public static final String FIELD_DEFAULT_DATA_TYPE = "http://www.w3.org/2001/XMLSchema#string";
  public static final String FIELD_DEFAULT_LANGUAGE = "";

  private static final String MISSING_FIELD = "field";

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  @BeforeEach
  public void setUp() {
    ObjectNode sampleInput = OBJECT_MAPPER.createObjectNode();
    sampleInput.set(FIELD_ONE, genResponseField(FIELD_TYPE_LITERAL, FIELD_VALUE_ONE, null, null));
    sampleInput.set(FIELD_TWO, genResponseField(FIELD_TYPE_URI, FIELD_VALUE_TWO, FIELD_TWO_DATA_TYPE, null));
    sampleInput.set(FIELD_THREE, genResponseField(FIELD_TYPE_LITERAL, FIELD_VALUE_THREE, null, FIELD_THREE_LANGUAGE));
    sampleBinding = new SparqlBinding(sampleInput);
  }

  @Test
  void testContainsField() {
    assertTrue(this.sampleBinding.containsField(FIELD_ONE), "Valid field one should return true.");
    assertTrue(this.sampleBinding.containsField(FIELD_TWO), "Valid field two should return true.");
    assertTrue(this.sampleBinding.containsField(FIELD_THREE), "Valid field three should return true.");

    assertFalse(this.sampleBinding.containsField(MISSING_FIELD), "Missing field should return false.");
  }

  @Test
  void testGet() {
    Map<String, SparqlResponseField> bindings = this.sampleBinding.get();
    assertNotNull(bindings);
    assertEquals(3, bindings.size());
    validateResponseField(bindings.get(FIELD_ONE), FIELD_TYPE_LITERAL, FIELD_VALUE_ONE, FIELD_DEFAULT_DATA_TYPE,
        FIELD_DEFAULT_LANGUAGE);
    validateResponseField(bindings.get(FIELD_TWO), FIELD_TYPE_URI, FIELD_VALUE_TWO, FIELD_TWO_DATA_TYPE,
        FIELD_DEFAULT_LANGUAGE);
    validateResponseField(bindings.get(FIELD_THREE), FIELD_TYPE_LITERAL, FIELD_VALUE_THREE, FIELD_DEFAULT_DATA_TYPE,
        FIELD_THREE_LANGUAGE);
  }

  @Test
  void testGetFieldValue() {
    assertEquals(FIELD_VALUE_ONE, this.sampleBinding.getFieldValue(FIELD_ONE),
        "Field one should return corresponding value!");
    assertEquals(FIELD_VALUE_TWO, this.sampleBinding.getFieldValue(FIELD_TWO),
        "Field two should return corresponding value!");
    assertEquals(FIELD_VALUE_THREE, this.sampleBinding.getFieldValue(FIELD_THREE),
        "Field three should return corresponding value!");
  }

  @Test
  void testGetFieldValue_NonExistentField() {
    assertNull(this.sampleBinding.getFieldValue(MISSING_FIELD),
        "Missing field should return null!");
  }

  /**
   * Generates a response field in the following format:
   * {type: "literal", "value": "any value", "datatype": "follows XML", "xml:lang"
   * : "en"
   * }
   * 
   * @param fieldType The field type - literal or uri
   * @param value     The value of the field
   * @param dataType  Optional data type following XML namespace like string,
   *                  integer
   * @param language  Optional language of interest
   */
  public static ObjectNode genResponseField(String fieldType, String value, String dataType, String language) {
    ObjectNode field = OBJECT_MAPPER.createObjectNode();
    field.put(TYPE_BINDING_KEY, fieldType);
    field.put(VAL_BINDING_KEY, value);
    if (dataType != null) {
      field.put(DATA_TYPE_BINDING_KEY, dataType);
    }
    if (language != null) {
      field.put(LANGUAGE_BINDING_KEY, language);
    }
    return field;
  }

  /**
   * Asserts the response field complies with the expectation.
   * 
   * @param target            The target field for validation
   * @param expectedFieldType The expected field type - literal or uri
   * @param expectedValue     The expected value of the field
   * @param expectedDataType  The expected data type following XML namespace like
   *                          string, integer
   * @param expectedLanguage  The expected language of interest
   */
  private static void validateResponseField(SparqlResponseField target, String expectedFieldType,
      String expectedValue, String expectedDataType, String expectedLanguage) {
    assertEquals(expectedFieldType, target.type(),
        "Field types do not match!");
    assertEquals(expectedValue, target.value(),
        "Field values do not match!");
    assertEquals(expectedDataType, target.dataType(),
        "Field data types do not match!");
    assertEquals(expectedLanguage, target.lang(),
        "Field languages do not match!");
  }
}