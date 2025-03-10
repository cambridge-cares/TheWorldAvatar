package com.cmclinnovations.agent.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.OptionalInt;
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
   * Add fields as an array only if there are distinct values.
   * 
   * @param field      The field of interest.
   * @param secBinding The secondary binding for checking.
   */
  public void addFieldArray(SparqlBinding secBinding) {
    // Check for the complete list of fields between the two to iterate over
    Set<String> existingFields = this.getFields();
    Set<String> fieldsForIteration = secBinding.getFields();
    if (!existingFields.equals(fieldsForIteration) && existingFields.size() > fieldsForIteration.size()) {
      fieldsForIteration = existingFields;
    }

    // Longest existing field array before iteration
    int longestArraySize = this.getLongestFieldArray();
    fieldsForIteration.forEach((field) -> {
      // Create placeholder fields
      SparqlResponseField existingField;
      SparqlResponseField newField;
      // When both bindings contain the field
      if (this.bindings.containsKey(field) && secBinding.containsField(field)) {
        // If the values are duplicates, skip the function
        if (this.bindings.get(field).value().equals(secBinding.getFieldValue(field))) {
          return;
        }
        // If the values are distinct,the fields should be added as they are
        existingField = this.bindings.get(field);
        newField = secBinding.getFieldResponse(field);
      } else if (this.bindings.containsKey(field)) {
        // Execute only if the primary/first binding contains the field
        existingField = this.bindings.get(field);
        // New field is empty value
        newField = new SparqlResponseField(existingField.type(), "", existingField.dataType(), existingField.lang());
      } else {
        // Execute only if the secondary binding contains the field
        newField = secBinding.getFieldResponse(field);
        // Existing value should be empty
        existingField = new SparqlResponseField(newField.type(), "", newField.dataType(), newField.lang());
      }

      List<SparqlResponseField> fields = this.bindingList.computeIfAbsent(field, k -> {
        List<SparqlResponseField> initFields = new ArrayList<>();
        // Do not move this outside the compute function to prevent duplicates
        initFields.add(existingField);
        // If null values are appended for the existing field, check if there should be
        // more empty fields appended to the array
        if (existingField.value().isEmpty()) {
          // Prepend empty fields when there are least two array items in the max
          for (int i = 1; i < longestArraySize; i++) {
            initFields.add(existingField);
          }
        }
        return initFields;
      });
      fields.add(newField);
    });

  }

  /**
   * Retrieve the SPARQL field response.
   * 
   * @param field Field of interest
   */
  public SparqlResponseField getFieldResponse(String field) {
    return this.bindings.get(field);
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

  /**
   * Retrieve the longest field array size.
   */
  private int getLongestFieldArray() {
    OptionalInt maxSize = this.bindingList.values().stream()
        .filter(Objects::nonNull) // Filter out null lists
        .mapToInt(List::size) // Map each list to its size
        .max();
    return maxSize.orElse(0);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (o == null || getClass() != o.getClass())
      return false;
    SparqlBinding that = (SparqlBinding) o;
    return Objects.equals(this.get(), that.get());
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.get());
  }
}