package com.cmclinnovations.agent.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.cmclinnovations.agent.utils.ShaclResource;
import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Holds the binding for each row in the SPARQL response. Each row has the
 * following format:
 * 
 * {
 * "type": SparqlResponseField,
 * "label": SparqlResponseField,
 * "description": SparqlResponseField,
 * "xml:lang": SparqlResponseField
 * }
 * 
 * In which SparqlResponseField is another JSON object
 */
public class SparqlBinding {
  private Map<String, SparqlResponseField> bindings;
  private Map<String, List<SparqlResponseField>> bindingList;
  private List<String> sequence;

  /**
   * Constructs a new model.
   */
  public SparqlBinding(ObjectNode sparqlRow) {
    this.bindings = new HashMap<>();
    this.bindingList = new HashMap<>();
    this.sequence = new ArrayList<>();
    Iterator<Map.Entry<String, JsonNode>> iterator = sparqlRow.fields();
    while (iterator.hasNext()) {
      Map.Entry<String, JsonNode> sparqlCol = iterator.next();
      JsonNode sparqlField = sparqlCol.getValue();
      String type = StringResource.getNodeString(sparqlField, "type");
      // Defaults to null if it is a URI, else it should be string
      String dataTypeDefaultOption = type.equals("uri") ? null : ShaclResource.XSD_STRING;
      this.bindings.put(sparqlCol.getKey(), new SparqlResponseField(
          type,
          StringResource.getNodeString(sparqlField, "value"),
          StringResource.optNodeString(sparqlField, "datatype", dataTypeDefaultOption),
          StringResource.optNodeString(sparqlField, "xml:lang", null)));
    }
  }

  /**
   * Retrieve the Bindings as a map object.
   * 
   * @return a map containing either SparqlResponseField or
   *         List<SparqlResponseField> as its values.
   */
  public Map<String, Object> get() {
    Map<String, Object> resultBindings;
    if (!this.sequence.isEmpty()) {
      // Sort the map if there is a sequence
      resultBindings = new LinkedHashMap<>();
      this.sequence.forEach(variable -> {
        String field = StringResource.parseQueryVariable(variable);
        if (this.bindings.get(field) != null) {
          resultBindings.put(field, this.bindings.get(field));
        }
      });
      return resultBindings;
    }
    resultBindings = new HashMap<>();
    // When there are array results,
    if (!this.bindingList.isEmpty()) {
      // Append array results to the result mappings
      this.bindingList.keySet().forEach(arrayField -> {
        resultBindings.put(arrayField,
            this.bindingList.get(arrayField));
        // Remove the existing mapping, so that it is not overwritten
        this.bindings.remove(arrayField);
      });
    }
    resultBindings.putAll(this.bindings);
    return resultBindings;
  }

  /**
   * Retrieve the field names.
   */
  public Set<String> getFields() {
    return this.bindings.keySet();
  }

  /**
   * Adds the sequence for the fields.
   * 
   * @param sequence List of order that fields should be in.
   */
  public void addSequence(List<String> sequence) {
    this.sequence = sequence;
  }

  /**
   * Add field value only if there are distinct values.
   * 
   * @param field The field of interest.
   * @param value The value for checking.
   */
  public void addFieldValueIfNew(String field, String value) {
    SparqlResponseField currentValue = this.bindings.get(field);
    // Condition that there are distinct value
    if (!currentValue.value().equals(value)) {
      List<SparqlResponseField> fields = this.bindingList.putIfAbsent(field, List.of(currentValue));
      SparqlResponseField newFieldVal = new SparqlResponseField(currentValue.type(), value, currentValue.dataType(),
          currentValue.lang());
      fields.add(newFieldVal);
    }
  }

  /**
   * Retrieve the field value.
   * 
   * @param field Field of interest
   */
  public String getFieldValue(String field) {
    SparqlResponseField fieldBinding = this.bindings.get(field);
    if (fieldBinding == null) {
      return null;
    }
    return fieldBinding.value();
  }

  /**
   * Verify if the bindings have the specified field
   * 
   * @param field Field of interest
   */
  public boolean containsField(String field) {
    return this.bindings.containsKey(field);
  }
}