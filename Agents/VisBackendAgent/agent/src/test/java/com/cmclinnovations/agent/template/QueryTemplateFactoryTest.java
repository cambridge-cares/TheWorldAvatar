package com.cmclinnovations.agent.template;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayDeque;
import java.util.Queue;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.ObjectMapper;

class QueryTemplateFactoryTest {
  private static ObjectMapper TEST_MAPPER;
  private static QueryTemplateFactory TEMPLATE_FACTORY;

  private static final String SAMPLE_PREFIX = "http://example.com/";
  private static final String SAMPLE_CONCEPT = SAMPLE_PREFIX + "Concept";
  private static final String SAMPLE_PRED_PATH = SAMPLE_PREFIX + "propPath1";
  private static final String SAMPLE_NESTED_PRED_PATH = SAMPLE_PREFIX + "propPath2";
  private static final String SAMPLE_PARENT_PATH = SAMPLE_PREFIX + "parentPath1";
  private static final String SAMPLE_SUB_PATH = SAMPLE_PREFIX + "subPath1";
  private static final String SAMPLE_OPTIONAL_PATH = SAMPLE_PREFIX + "optionalPath1";
  private static final String SAMPLE_FIELD = "field";
  private static final String SAMPLE_PARENT_FIELD = "parent field";
  private static final String SAMPLE_OPTIONAL_FIELD = "optional field";

  private static final String SAMPLE_FILTER = "01j82";

  private static final String CLAZZ_VAR = "clazz";
  private static final String NAME_VAR = "name";
  private static final String IS_OPTIONAL_VAR = "isoptional";
  private static final String IS_PARENT_VAR = "isparent";
  private static final String MULTIPATH_VAR = "multipath";
  private static final String MULTISUBPATH_VAR = "multisubpath";

  @BeforeAll
  static void setup() {
    TEST_MAPPER = new ObjectMapper();
    TEMPLATE_FACTORY = new QueryTemplateFactory(TEST_MAPPER);
  }

  @Test
  void testGenGetTemplate() {
    // Set up
    Queue<Queue<SparqlBinding>> nestedBindings = new ArrayDeque<>();
    Queue<SparqlBinding> bindings = new ArrayDeque<>();
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_FIELD, SAMPLE_PRED_PATH, "", false, false);
    nestedBindings.offer(bindings);
    bindings = new ArrayDeque<>();
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_FIELD, SAMPLE_NESTED_PRED_PATH, "", false, false);
    nestedBindings.offer(bindings);
    // Execute
    Queue<String> results = TEMPLATE_FACTORY.genGetTemplate(nestedBindings, null, false);
    // Assert
    assertEquals(2, results.size());
    String result = results.poll();
    assertNotNull(result);
    assertTrue(result.startsWith("SELECT * WHERE {"));
    assertEquals(
        genExpectedQueryStart(SAMPLE_CONCEPT) + genQueryLine(
            StringResource.parseIriForQuery(SAMPLE_PRED_PATH) + "/"
                + StringResource.parseIriForQuery(SAMPLE_NESTED_PRED_PATH),
            "?" + SAMPLE_FIELD) + "}",
        result);
  }

  @Test
  void testGenGetTemplate_FilterId() {
    // Set up
    Queue<Queue<SparqlBinding>> nestedBindings = new ArrayDeque<>();
    Queue<SparqlBinding> bindings = new ArrayDeque<>();
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_FIELD, SAMPLE_PRED_PATH, "", false, false);
    nestedBindings.offer(bindings);
    bindings = new ArrayDeque<>();
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_FIELD, SAMPLE_NESTED_PRED_PATH, "", false, false);
    nestedBindings.offer(bindings);
    // Execute
    Queue<String> results = TEMPLATE_FACTORY.genGetTemplate(nestedBindings, SAMPLE_FILTER, false);
    // Assert
    assertEquals(2, results.size());
    String result = results.poll();
    assertNotNull(result);
    assertTrue(result.startsWith(
        genExpectedQueryStart(SAMPLE_CONCEPT) + genQueryLine(
            StringResource.parseIriForQuery(SAMPLE_PRED_PATH) + "/"
                + StringResource.parseIriForQuery(SAMPLE_NESTED_PRED_PATH),
            "?" + SAMPLE_FIELD)));
    // Additional filter clause
    assertTrue(result.endsWith("FILTER STRENDS(STR(?id), \"" + SAMPLE_FILTER + "\")}"));
  }

  @Test
  void testGenGetTemplate_HasParent_MissingFilter() {
    // Set up
    Queue<Queue<SparqlBinding>> nestedBindings = new ArrayDeque<>();
    Queue<SparqlBinding> bindings = new ArrayDeque<>();
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_FIELD, SAMPLE_PARENT_PATH, SAMPLE_SUB_PATH, false, true);
    nestedBindings.offer(bindings);
    // The method should throw an exception because filterId is required when
    // hasParent is true
    IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, () -> {
      TEMPLATE_FACTORY.genGetTemplate(nestedBindings, null, true);
    });
    assertTrue(thrown.getMessage().contains("Detected a parent without a valid filter ID!"));
  }

  // Mock isParent doesnt work at nested level
  @Test
  void testGenGetTemplate_HasParent() {
    // Set up
    Queue<Queue<SparqlBinding>> nestedBindings = new ArrayDeque<>();
    Queue<SparqlBinding> bindings = new ArrayDeque<>();
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_PARENT_FIELD, SAMPLE_PARENT_PATH, SAMPLE_SUB_PATH, false, true);
    nestedBindings.offer(bindings);
    // Execute
    Queue<String> results = TEMPLATE_FACTORY.genGetTemplate(nestedBindings, SAMPLE_FILTER, true);
    // Assert
    String parentVariable = SAMPLE_PARENT_FIELD.replaceAll("\\s+", "_");
    assertEquals(2, results.size());
    String result = results.poll();
    assertNotNull(result);
    assertTrue(result.startsWith(
        genExpectedQueryStart(SAMPLE_CONCEPT) + genQueryLine(
            StringResource.parseIriForQuery(SAMPLE_PARENT_PATH) + "/"
                + StringResource.parseIriForQuery(SAMPLE_SUB_PATH),
            "?" + parentVariable)));
    // Additional parent filter
    assertTrue(result.endsWith("FILTER STRENDS(STR(?" + parentVariable + "), \"" + SAMPLE_FILTER + "\")}"));
  }

  // Mock isOptional doesnt work at nested level
  @Test
  void testGenGetTemplate_OptionalFields() {
    // Set up
    Queue<Queue<SparqlBinding>> nestedBindings = new ArrayDeque<>();
    Queue<SparqlBinding> bindings = new ArrayDeque<>();
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_OPTIONAL_FIELD, SAMPLE_OPTIONAL_PATH, "", true, false);
    nestedBindings.offer(bindings);
    // Execute
    Queue<String> results = TEMPLATE_FACTORY.genGetTemplate(nestedBindings, null, false);
    // Assert
    String optVariable = SAMPLE_OPTIONAL_FIELD.replaceAll("\\s+", "_");
    assertEquals(2, results.size());
    // Additional optional field
    assertTrue(results.poll().endsWith(
        "OPTIONAL{" + genQueryLine(StringResource.parseIriForQuery(SAMPLE_OPTIONAL_PATH), "?" + optVariable) + "}}"));
    assertTrue(results.poll().endsWith(
        "OPTIONAL{" + genQueryLine(StringResource.parseIriForQuery(SAMPLE_OPTIONAL_PATH), "?" + optVariable) + "}}"));
  }

  /**
   * Generate the starting part of the query.
   * 
   * @param clazz Class restriction in query
   */
  private String genExpectedQueryStart(String clazz) {
    return "SELECT * WHERE {?iri a <" + clazz + ">.";
  }

  /**
   * Generate each query line.
   * 
   * @param predicatePath Predicate path(s)
   * @param objectVar     Object variable to replace
   */
  private String genQueryLine(String predicatePath, String objectVar) {
    return "?iri " + predicatePath + " " + objectVar + ".";
  }

  /**
   * Generates a mock version of one SPARQL binding.
   * 
   * @param resultBindings   Stores the binding generated
   * @param clazz            The target class of the query
   * @param multiPathPred    The multi path for the predicate
   * @param multiSubPathPred The multi sub path for the predicate
   * @param varName          The variable name
   * @param isOptional       Indicates if the field is optional
   * @param isParent         Indicates if the field is the parent field
   */
  private static void genMockSPARQLBinding(Queue<SparqlBinding> resultBindings, String clazz, String varName,
      String multiPathPred, String multiSubPathPred, boolean isOptional, boolean isParent) {
    SparqlBinding binding = mock(SparqlBinding.class);
    when(binding.getFieldValue(CLAZZ_VAR)).thenReturn(clazz);
    when(binding.getFieldValue(NAME_VAR)).thenReturn(varName);
    // Only create mock interactions if there are values

    if (!multiPathPred.isEmpty()) {
      when(binding.containsField(MULTIPATH_VAR)).thenReturn(true);
      when(binding.getFieldValue(MULTIPATH_VAR)).thenReturn(multiPathPred);
    }
    // Only create mock interactions if there are values
    if (!multiSubPathPred.isEmpty()) {
      when(binding.containsField(MULTISUBPATH_VAR)).thenReturn(true);
      when(binding.getFieldValue(MULTISUBPATH_VAR)).thenReturn(multiSubPathPred);
    }
    when(binding.getFieldValue(IS_OPTIONAL_VAR)).thenReturn(String.valueOf(isOptional));
    when(binding.getFieldValue(IS_PARENT_VAR)).thenReturn(String.valueOf(isParent));
    resultBindings.offer(binding);
  }
}
