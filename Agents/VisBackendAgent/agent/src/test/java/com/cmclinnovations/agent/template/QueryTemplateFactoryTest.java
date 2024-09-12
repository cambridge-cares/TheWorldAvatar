package com.cmclinnovations.agent.template;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Queue;

import org.junit.jupiter.api.Test;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.utils.StringResource;

class QueryTemplateFactoryTest {
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
  private static final String MULTIPATH_FIRST_VAR = "multipath1";
  private static final String MULTIPATH_SEC_VAR = "multipath2";
  private static final String MULTIPATH_THIRD_VAR = "multipath3";
  private static final String MULTISUBPATH_FIRST_VAR = "multisubpath1";
  private static final String MULTISUBPATH_SEC_VAR = "multisubpath2";
  private static final String MULTISUBPATH_THIRD_VAR = "multisubpath3";
  private static final String MULTISUBPATH_FORTH_VAR = "multisubpath4";
  private static final List<String> PROPERTY_PATH_VARIABLES = Arrays.asList(
      MULTIPATH_FIRST_VAR,
      MULTIPATH_SEC_VAR,
      MULTIPATH_THIRD_VAR,
      MULTISUBPATH_FIRST_VAR,
      MULTISUBPATH_SEC_VAR,
      MULTISUBPATH_THIRD_VAR,
      MULTISUBPATH_FORTH_VAR);

  @Test
  void testGenGetTemplate() {
    // Set up
    List<SparqlBinding> bindings = new ArrayList<>();
    Queue<String> paths = genSPARQLBindingPaths(SAMPLE_PRED_PATH, SAMPLE_NESTED_PRED_PATH, "",
        "", "", "", "");
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_FIELD, paths, false, false);
    QueryTemplateFactory sampleTemplate = new QueryTemplateFactory();
    // Execute
    String result = sampleTemplate.genGetTemplate(bindings, null, false);
    // Assert
    assertNotNull(result);
    assertTrue(result.startsWith("SELECT * WHERE {"));
    assertEquals(
        genExpectedQueryStart(SAMPLE_CONCEPT) + StringResource.parseIriForQuery(SAMPLE_PRED_PATH) + "/"
            + StringResource.parseIriForQuery(SAMPLE_NESTED_PRED_PATH)
            + " ?" + SAMPLE_FIELD + ".}",
        result);
  }

  @Test
  void testGenGetTemplate_FilterId() {
    // Set up
    List<SparqlBinding> bindings = new ArrayList<>();
    Queue<String> paths = genSPARQLBindingPaths(SAMPLE_PRED_PATH, SAMPLE_NESTED_PRED_PATH, "",
        "", "", "", "");
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_FIELD, paths, false, false);
    QueryTemplateFactory sampleTemplate = new QueryTemplateFactory();
    // Execute
    String result = sampleTemplate.genGetTemplate(bindings, SAMPLE_FILTER, false);
    // Assert
    assertNotNull(result);
    assertTrue(result.startsWith(
        genExpectedQueryStart(SAMPLE_CONCEPT) + StringResource.parseIriForQuery(SAMPLE_PRED_PATH) + "/"
            + StringResource.parseIriForQuery(SAMPLE_NESTED_PRED_PATH)
            + " ?" + SAMPLE_FIELD + "."));
    // Additional filter clause
    assertTrue(result.endsWith("FILTER STRENDS(STR(?id), \"" + SAMPLE_FILTER + "\")}"));
  }

  @Test
  void testGenGetTemplate_HasParent_MissingFilter() {
    List<SparqlBinding> bindings = new ArrayList<>();
    Queue<String> paths = genSPARQLBindingPaths(SAMPLE_PARENT_PATH, "", "",
        SAMPLE_SUB_PATH, "", "", "");
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_PARENT_FIELD, paths, false, true);
    QueryTemplateFactory sampleTemplate = new QueryTemplateFactory();
    // The method should throw an exception because filterId is required when
    // hasParent is true
    IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, () -> {
      sampleTemplate.genGetTemplate(bindings, null, true);
    });
    assertTrue(thrown.getMessage().contains("Detected a parent without a valid filter ID!"));
  }

  @Test
  void testGenGetTemplate_HasParent() {
    // Set up
    List<SparqlBinding> bindings = new ArrayList<>();
    Queue<String> paths = genSPARQLBindingPaths(SAMPLE_PARENT_PATH, "", "",
        SAMPLE_SUB_PATH, "", "", "");
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_PARENT_FIELD, paths, false, true);
    QueryTemplateFactory sampleTemplate = new QueryTemplateFactory();
    // Execute
    String result = sampleTemplate.genGetTemplate(bindings, SAMPLE_FILTER, true);
    // Assert
    String parentVariable = SAMPLE_PARENT_FIELD.replaceAll("\\s+", "_");
    assertNotNull(result);
    assertTrue(result.startsWith(
        genExpectedQueryStart(SAMPLE_CONCEPT) + StringResource.parseIriForQuery(SAMPLE_PARENT_PATH) + "/"
            + StringResource.parseIriForQuery(SAMPLE_SUB_PATH)
            + " ?" + parentVariable + "."));
    // Additional parent filter
    assertTrue(result.endsWith("FILTER STRENDS(STR(?" + parentVariable + "), \"" + SAMPLE_FILTER + "\")}"));
  }

  @Test
  void testGenGetTemplate_OptionalFields() {
    // Set up
    List<SparqlBinding> bindings = new ArrayList<>();
    Queue<String> paths = genSPARQLBindingPaths(SAMPLE_PRED_PATH, SAMPLE_NESTED_PRED_PATH, "",
        "", "", "", "");
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_FIELD, paths, false, false);
    // Generate optional path
    paths = genSPARQLBindingPaths(SAMPLE_OPTIONAL_PATH, "", "",
        "", "", "", "");
    genMockSPARQLBinding(bindings, SAMPLE_CONCEPT, SAMPLE_OPTIONAL_FIELD, paths, true, false);
    QueryTemplateFactory sampleTemplate = new QueryTemplateFactory();
    // Execute
    String result = sampleTemplate.genGetTemplate(bindings, null, false);
    // Assert
    String optVariable = SAMPLE_OPTIONAL_FIELD.replaceAll("\\s+", "_");
    assertNotNull(result);
    // Additional optional field
    assertTrue(result.endsWith(
        "OPTIONAL{?iri " + StringResource.parseIriForQuery(SAMPLE_OPTIONAL_PATH) + " ?" + optVariable + ".}}"));
  }

  /**
   * Generate the starting part of the query.
   * 
   * @param clazz Class restriction in query
   */
  private String genExpectedQueryStart(String clazz) {
    return "SELECT * WHERE {?iri a <" + clazz + ">;";
  }

  /**
   * Generates the SPARQL binding paths
   * 
   * @param multipath1    Value of binding path. Use an empty string if it should
   *                      be optional.
   * @param multipath2    Value of binding path. Use an empty string if it should
   *                      be optional.
   * @param multipath3    Value of binding path. Use an empty string if it should
   *                      be optional.
   * @param multisubpath1 Value of binding path. Use an empty string if it should
   *                      be optional.
   * @param multisubpath2 Value of binding path. Use an empty string if it should
   *                      be optional.
   * @param multisubpath3 Value of binding path. Use an empty string if it should
   *                      be optional.
   * @param multisubpath4 Value of binding path. Use an empty string if it should
   *                      be optional.
   */
  private static Queue<String> genSPARQLBindingPaths(String multipath1, String multipath2, String multipath3,
      String multisubpath1, String multisubpath2, String multisubpath3, String multisubpath4) {
    Queue<String> results = new ArrayDeque<>();
    results.offer(multipath1);
    results.offer(multipath2);
    results.offer(multipath3);
    results.offer(multisubpath1);
    results.offer(multisubpath2);
    results.offer(multisubpath3);
    results.offer(multisubpath4);
    return results;
  }

  /**
   * Generates a mock version of one SPARQL binding.
   * 
   * @param resultBindings Stores the binding generated
   * @param clazz          The target class of the query
   * @param paths          A queue of paths corresponding to the length of
   *                       available property path variables. If it does not
   *                       exist, pass an empty string ""
   * @param varName        The variable name
   * @param isOptional     Indicates if the field is optional
   * @param isParent       Indicates if the field is the parent field
   */
  private static void genMockSPARQLBinding(List<SparqlBinding> resultBindings, String clazz, String varName,
      Queue<String> paths, boolean isOptional, boolean isParent) {
    if (PROPERTY_PATH_VARIABLES.size() != paths.size()) {
      throw new IllegalArgumentException("Invalid size of paths parameter");
    }

    SparqlBinding binding = mock(SparqlBinding.class);
    when(binding.getFieldValue(CLAZZ_VAR)).thenReturn(clazz);
    when(binding.getFieldValue(NAME_VAR)).thenReturn(varName);

    PROPERTY_PATH_VARIABLES.forEach((pathVarName) -> {
      String pathValue = paths.poll();
      // Only create mock interactions if there are values
      if (!pathValue.isEmpty()) {
        when(binding.containsField(pathVarName)).thenReturn(true);
        when(binding.getFieldValue(pathVarName)).thenReturn(pathValue);
      }
    });
    when(binding.getFieldValue(IS_OPTIONAL_VAR)).thenReturn(String.valueOf(isOptional));
    when(binding.getFieldValue(IS_PARENT_VAR)).thenReturn(String.valueOf(isParent));
    resultBindings.add(binding);
  }
}
