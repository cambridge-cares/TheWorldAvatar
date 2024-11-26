package com.cmclinnovations.agent.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

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
  private List<SparqlVariableOrder> sequence;

  /**
   * Constructs a new model.
   */
  public SparqlBinding(ObjectNode sparqlRow) {
    this.bindings = new HashMap<>();
    this.sequence = new ArrayList<>();
    Iterator<Map.Entry<String, JsonNode>> iterator = sparqlRow.fields();
    while (iterator.hasNext()) {
      Map.Entry<String, JsonNode> sparqlCol = iterator.next();
      JsonNode sparqlField = sparqlCol.getValue();
      String type = StringResource.getNodeString(sparqlField, "type");
      // Defaults to null if it is a URI, else it should be string
      String dataTypeDefaultOption = type.equals("uri") ? null : "http://www.w3.org/2001/XMLSchema#string";
      this.bindings.put(sparqlCol.getKey(), new SparqlResponseField(
          type,
          StringResource.getNodeString(sparqlField, "value"),
          StringResource.optNodeString(sparqlField, "datatype", dataTypeDefaultOption),
          StringResource.optNodeString(sparqlField, "xml:lang", null)));
    }
  }

  /**
   * Retrieve the Bindings as a map object.
   */
  public Map<String, SparqlResponseField> get() {
    if (!this.sequence.isEmpty()) {
      // Sort the map if there is a sequence
      Map<String, SparqlResponseField> sortedBindings = new LinkedHashMap<>();
      this.sequence
          .forEach(variable -> {
            String field = variable.property();
            if (this.bindings.get(field) != null) {
              sortedBindings.put(field, this.bindings.get(field));
            }
          });
      return sortedBindings;
    }
    return this.bindings;
  }

  /**
   * Retrieve the Bindings as a map object.
   */
  public void addSequence(List<SparqlVariableOrder> sequence) {
    this.sequence = sequence;
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