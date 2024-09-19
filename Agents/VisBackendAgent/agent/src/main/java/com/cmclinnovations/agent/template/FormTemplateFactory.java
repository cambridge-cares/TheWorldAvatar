package com.cmclinnovations.agent.template;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Iterator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;

public class FormTemplateFactory implements ShaclTemplateFactory {
  // Data stores
  private Queue<JsonNode> properties;
  private Map<String, Object> form;
  private Map<String, JsonNode> groups;
  private Map<String, Queue<Map<String, Object>>> dependentShapes;

  private final ObjectMapper objectMapper;
  private static final Logger LOGGER = LogManager.getLogger(FormTemplateFactory.class);

  /**
   * Constructs a new form template factory.
   */
  public FormTemplateFactory() {
    this.objectMapper = new ObjectMapper();
  }

  /**
   * Generate form template in JSON object format.
   * 
   * @param data        Data to be parsed for form template.
   * @param defaultVals Default values for the form template if there is an
   *                    existing entity.
   */
  public Map<String, Object> genTemplate(ArrayNode data, Map<String, Object> defaultVals) {
    this.reset(); // Reset each time method is called to prevent any data storage
    LOGGER.debug("Generating template from query results...");
    this.sortData(data);

    // No template should be generated if there are no properties
    if (this.properties.isEmpty()) {
      return new HashMap<>();
    } else {
      this.addContext();
      this.parseInputs(defaultVals);
    }

    return this.form;
  }

  /**
   * Resets the factory.
   */
  private void reset() {
    this.form = new HashMap<>();
    this.groups = new HashMap<>();
    this.dependentShapes = new HashMap<>();
    this.properties = new ArrayDeque<>();
  }

  /**
   * Adds the context for the form template.
   */
  private void addContext() {
    Map<String, String> context = new HashMap<>();
    context.put(COMMENT_PROPERTY, RDFS_PREFIX + COMMENT_PROPERTY);
    context.put(LABEL_PROPERTY, RDFS_PREFIX + LABEL_PROPERTY);

    context.put(PROPERTY_GROUP, SHACL_PREFIX + PROPERTY_GROUP);
    context.put(PROPERTY_SHAPE, SHACL_PREFIX + PROPERTY_SHAPE);
    context.put(NAME_PROPERTY, SHACL_PREFIX + NAME_PROPERTY);
    context.put(DESCRIPTION_PROPERTY, SHACL_PREFIX + DESCRIPTION_PROPERTY);
    context.put(ORDER_PROPERTY, SHACL_PREFIX + ORDER_PROPERTY);
    context.put(GROUP_PROPERTY, SHACL_PREFIX + GROUP_PROPERTY);
    context.put(PROPERTY_PROPERTY, SHACL_PREFIX + PROPERTY_PROPERTY);
    context.put(DEFAULT_VAL_PROPERTY, SHACL_PREFIX + DEFAULT_VAL_PROPERTY);
    context.put(CLASS_PROPERTY, SHACL_PREFIX + CLASS_PROPERTY);
    context.put(DATA_TYPE_PROPERTY, SHACL_PREFIX + DATA_TYPE_PROPERTY);
    context.put(IN_PROPERTY, SHACL_PREFIX + IN_PROPERTY);
    context.put(QUALIFIED_VAL_SHAPE_PROPERTY, SHACL_PREFIX + QUALIFIED_VAL_SHAPE_PROPERTY);
    this.form.put("@context", context);
  }

  /**
   * Sorts the data fields into property or property group.
   * 
   * @param fields Fields to category.
   */
  private void sortData(ArrayNode fields) {
    // All array nodes will be followed up with a get(index) if they are arrays,
    // else, path are used for object nodes
    for (JsonNode field : fields) {
      if (field.has(TYPE_KEY)) {
        String type = field.path(TYPE_KEY).get(0).asText();
        if (type.equals(SHACL_PREFIX + PROPERTY_SHAPE)) {
          // This checker is added as the SPARQL query might provide a response with
          // multiple duplicate property shapes due to the presence of different nested
          // value shapes
          if (field.has(SHACL_PREFIX + QUALIFIED_VAL_SHAPE_PROPERTY)) {
            // Check if there is an existing shape with another qualifier name
            String dependentIdentifier = field
                .path(SHACL_PREFIX + NAME_PROPERTY).get(0)
                .path(VAL_KEY).asText();
            Map<String, Object> dependentShape = this.parseInputModel(field
                .path(SHACL_PREFIX + QUALIFIED_VAL_SHAPE_PROPERTY)
                .get(0), null);
            this.dependentShapes.computeIfAbsent(dependentIdentifier, k -> {
              // When there is no existing shape, add a new property for sorting
              // as well as a new empty queue
              this.properties.offer(field);
              return new ArrayDeque<>();
            }).offer(dependentShape); // Add a new instance to the associated queue
          } else {
            // Otherwise, the simplest use case is to add a new property
            this.properties.offer(field);
          }
        } else if (type.equals(SHACL_PREFIX + PROPERTY_GROUP)) {
          this.groups.put(field.path(ID_KEY).asText(), field);
        } else {
          LOGGER.error("Invalid input node! Only property shape and property group is allowed.");
          throw new IllegalArgumentException(
              "Invalid input node! Only property shape and property group is allowed.");
        }
      }
    }
  }

  /**
   * Parse the property inputs into Spring Boot compliant JSON response format.
   * 
   * @param defaultVals Default values for the form template if there is an
   *                    existing entity.
   */
  private void parseInputs(Map<String, Object> defaultVals) {
    Map<String, Map<String, Object>> parsedProperties = new HashMap<>();
    List<Map<String, Object>> results = new ArrayList<>();

    while (!this.properties.isEmpty()) {
      JsonNode currentProperty = this.properties.poll();
      // When there is a group
      if (currentProperty.has(SHACL_PREFIX + GROUP_PROPERTY)) {
        String groupId = currentProperty.path(SHACL_PREFIX + GROUP_PROPERTY)
            .get(0).path(ID_KEY).asText();
        // Retrieve existing group in parsed model if available, or else, generate one
        // from the associated group
        Map<String, Object> group = parsedProperties.getOrDefault(groupId,
            this.parseInputModel(this.groups.get(groupId), defaultVals));
        // Retrieve existing group properties in parsed model if available, or else,
        // generate one; Type cast is definitely accurate
        List<Map<String, Object>> groupProperties = (List<Map<String, Object>>) group
            .getOrDefault(PROPERTY_PROPERTY, new ArrayList<>());
        // Add new property
        groupProperties.add(parseInputModel(currentProperty, defaultVals));
        // Sort the properties based on order
        groupProperties.sort(Comparator.comparingInt(map -> (int) map.get(ORDER_PROPERTY)));
        // Update the results
        group.put(PROPERTY_PROPERTY, groupProperties);
        parsedProperties.put(groupId, group);
      } else {
        // Without a group, simply use the ID as hash key
        parsedProperties.put(currentProperty.path(ID_KEY).asText(), this.parseInputModel(currentProperty, defaultVals));
      }
    }
    parsedProperties.forEach((key, value) -> {
      results.add(value);
    });
    // Sort the results based on order
    results.sort(Comparator.comparingInt(map -> (int) map.get(ORDER_PROPERTY)));
    this.form.put(PROPERTY_PROPERTY, results);
  }

  /**
   * Parse the input into a suitable JSON model.
   * 
   * @param input       Input of interest.
   * @param defaultVals Default values for the form template if there is an
   *                    existing entity.
   */
  private Map<String, Object> parseInputModel(JsonNode input, Map<String, Object> defaultVals) {
    Map<String, Object> inputModel = new HashMap<>();
    // Transform each field into a suitable JSON format
    Iterator<Map.Entry<String, JsonNode>> iterator = input.fields();
    while (iterator.hasNext()) {
      Map.Entry<String, JsonNode> shapeFieldEntry = iterator.next();
      String shapeField = shapeFieldEntry.getKey();
      JsonNode shapeFieldNode = shapeFieldEntry.getValue();
      // Id will always be a string
      if (shapeField.equals(ID_KEY)) {
        inputModel.put(shapeField, shapeFieldNode.asText());
      } else if (shapeField.equals(TYPE_KEY)) {
        // Type will always be enclosed in a string array of one item
        inputModel.put(shapeField, shapeFieldNode.get(0).asText());
      } else if (shapeField.equals(SHACL_PREFIX + NAME_PROPERTY)) {
        Map<String, Object> nameLiteral = this.objectMapper.convertValue(shapeFieldNode.get(0), Map.class);
        inputModel.put(StringResource.getLocalName(shapeField), nameLiteral);
        if (!defaultVals.isEmpty()) {
          String parsedField = nameLiteral.get(VAL_KEY).toString().replace(" ", "_");
          inputModel.put("defaultValue", defaultVals.get(parsedField));
        }
      } else if (shapeField.equals(SHACL_PREFIX + ORDER_PROPERTY)) {
        Map<String, Object> orderMap = this.objectMapper.convertValue(shapeFieldNode.get(0), Map.class);
        inputModel.put(StringResource.getLocalName(shapeField), Integer.valueOf(orderMap.get(VAL_KEY).toString()));
      } else if (shapeField.equals(SHACL_PREFIX + DATA_TYPE_PROPERTY)) {
        // Data types are stored in @id key with xsd namespace
        // But we are only interested in the local name and extract it accordingly
        Map<String, Object> dataType = this.objectMapper.convertValue(shapeFieldNode.get(0), Map.class);
        inputModel.put(StringResource.getLocalName(shapeField),
            StringResource.getLocalName(dataType.get(ID_KEY).toString()));
      } else if (shapeField.equals(SHACL_PREFIX + QUALIFIED_VAL_SHAPE_PROPERTY)) {
        // For qualified value shape, retrieve the parsed ID and its associated queue
        // from the mapping. Note that there should be no duplicate shapes
        String dependentId = input.get(SHACL_PREFIX + NAME_PROPERTY).get(0).path(VAL_KEY).asText();
        Queue<Map<String, Object>> dependentGroup = this.dependentShapes.get(dependentId);
        inputModel.put(StringResource.getLocalName(shapeField), dependentGroup);
      } else {
        // Every other fields are stored as a nested JSON object of key:value pair
        // within a one item JSON array
        inputModel.put(StringResource.getLocalName(shapeField),
            this.objectMapper.convertValue(shapeFieldNode.get(0), Map.class));
      }
    }
    return inputModel;
  }
}
