package com.cmclinnovations.agent.template;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.agent.utils.ShaclResource;
import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;

public class FormTemplateFactory {
  // Data stores
  private Queue<JsonNode> properties;
  private Map<String, Object> form;
  private Map<String, JsonNode> groups;
  private Map<String, JsonNode> nodes;

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
    this.nodes = new HashMap<>();
    this.properties = new ArrayDeque<>();
  }

  /**
   * Adds the context for the form template.
   */
  private void addContext() {
    Map<String, String> context = new HashMap<>();
    context.put(ShaclResource.COMMENT_PROPERTY, ShaclResource.RDFS_PREFIX + ShaclResource.COMMENT_PROPERTY);
    context.put(ShaclResource.LABEL_PROPERTY, ShaclResource.RDFS_PREFIX + ShaclResource.LABEL_PROPERTY);

    context.put(ShaclResource.PROPERTY_GROUP, ShaclResource.SHACL_PREFIX + ShaclResource.PROPERTY_GROUP);
    context.put(ShaclResource.PROPERTY_SHAPE, ShaclResource.SHACL_PREFIX + ShaclResource.PROPERTY_SHAPE);
    context.put(ShaclResource.NAME_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.NAME_PROPERTY);
    context.put(ShaclResource.DESCRIPTION_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.DESCRIPTION_PROPERTY);
    context.put(ShaclResource.ORDER_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.ORDER_PROPERTY);
    context.put(ShaclResource.NODE_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.NODE_PROPERTY);
    context.put(ShaclResource.GROUP_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.GROUP_PROPERTY);
    context.put(ShaclResource.PROPERTY_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.PROPERTY_PROPERTY);
    context.put(ShaclResource.DEFAULT_VAL_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.DEFAULT_VAL_PROPERTY);
    context.put(ShaclResource.CLASS_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.CLASS_PROPERTY);
    context.put(ShaclResource.DATA_TYPE_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.DATA_TYPE_PROPERTY);
    context.put(ShaclResource.IN_PROPERTY, ShaclResource.SHACL_PREFIX + ShaclResource.IN_PROPERTY);
    this.form.put(ShaclResource.CONTEXT_KEY, context);
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
      if (field.has(ShaclResource.TYPE_KEY)) {
        String type = field.path(ShaclResource.TYPE_KEY).get(0).asText();
        if (type.equals(ShaclResource.SHACL_PREFIX + ShaclResource.PROPERTY_SHAPE)) {
          this.properties.offer(field);
        } else if (type.equals(ShaclResource.SHACL_PREFIX + ShaclResource.PROPERTY_GROUP)) {
          this.groups.put(field.path(ShaclResource.ID_KEY).asText(), field);
        } else if (type.equals(ShaclResource.SHACL_PREFIX + ShaclResource.NODE_SHAPE)) {
          this.nodes.put(field.path(ShaclResource.ID_KEY).asText(), field);
        } else {
          LOGGER.error("Invalid input node! Only property shape, property group, and node shape is allowed.");
          throw new IllegalArgumentException(
              "Invalid input node! Only property shape, property group, and node shape is allowed.");
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
    Map<String, Map<String, Map<String, Object>>> altProperties = new HashMap<>();
    Map<String, Map<String, Object>> defaultProperties = new HashMap<>();

    while (!this.properties.isEmpty()) {
      JsonNode currentProperty = this.properties.poll();
      // If the property belongs to a node shape, extract the properties into another
      // set of mapping
      if (currentProperty.has(ShaclResource.TWA_FORM_PREFIX + ShaclResource.BELONGS_TO_PROPERTY)) {
        String nodeId = currentProperty.path(ShaclResource.TWA_FORM_PREFIX + ShaclResource.BELONGS_TO_PROPERTY)
            .get(0).path(ShaclResource.ID_KEY).asText();
        Map<String, Map<String, Object>> nodeProperties = altProperties.getOrDefault(nodeId, new HashMap<>());
        this.parseProperty(currentProperty, defaultVals, nodeProperties);
        altProperties.put(nodeId, nodeProperties);
      } else {
        // Else simply generate the property
        this.parseProperty(currentProperty, defaultVals, defaultProperties);
      }
    }
    this.form.put(ShaclResource.PROPERTY_PROPERTY, this.genOutputs(defaultProperties));

    List<Map<String, Object>> nodeShape = new ArrayList<>();
    this.nodes.forEach((key, node) -> {
      Map<String, Object> output = new HashMap<>();
      output.put(ShaclResource.LABEL_PROPERTY,
          node.path(ShaclResource.RDFS_PREFIX + ShaclResource.LABEL_PROPERTY).get(0));
      output.put(ShaclResource.COMMENT_PROPERTY,
          node.path(ShaclResource.RDFS_PREFIX + ShaclResource.COMMENT_PROPERTY).get(0));
      output.put(ShaclResource.PROPERTY_PROPERTY,
          this.genOutputs(altProperties.getOrDefault(key, new HashMap<>())));
      nodeShape.add(output);
    });
    this.form.put(ShaclResource.NODE_PROPERTY, nodeShape);
  }

  /**
   * Parses and stores the properties and groups into the result mappings.
   * 
   * @param property       Target property.
   * @param defaultVals    Default values for the form template if there is an
   *                       existing entity.
   * @param resultMappings Mappings to store the parsed property.
   */
  private void parseProperty(JsonNode property, Map<String, Object> defaultVals,
      Map<String, Map<String, Object>> resultMappings) {
    // When there is a group
    if (property.has(ShaclResource.SHACL_PREFIX + ShaclResource.GROUP_PROPERTY)) {
      String groupId = property.path(ShaclResource.SHACL_PREFIX + ShaclResource.GROUP_PROPERTY)
          .get(0).path(ShaclResource.ID_KEY).asText();
      // Retrieve existing group in parsed model if available, or else, generate one
      // from the associated group
      Map<String, Object> group = resultMappings.getOrDefault(groupId,
          this.parseInputModel(this.groups.get(groupId), defaultVals));
      // Retrieve existing group properties in parsed model if available, or else,
      // generate one; Type cast is definitely accurate
      List<Map<String, Object>> groupProperties = (List<Map<String, Object>>) group
          .getOrDefault(ShaclResource.PROPERTY_PROPERTY, new ArrayList<>());
      // Add new property
      groupProperties.add(parseInputModel(property, defaultVals));
      // Update the results
      group.put(ShaclResource.PROPERTY_PROPERTY, groupProperties);
      resultMappings.put(groupId, group);
    } else {
      // Without a group, simply use the ID as hash key
      resultMappings.put(property.path(ShaclResource.ID_KEY).asText(),
          this.parseInputModel(property, defaultVals));
    }
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
      switch (shapeField) {
        case ShaclResource.ID_KEY:
          // Id will always be a string
          inputModel.put(shapeField, shapeFieldNode.asText());
          break;
        case ShaclResource.TYPE_KEY:
          // Type will always be enclosed in a string array of one item
          inputModel.put(shapeField, shapeFieldNode.get(0).asText());
          break;
        case ShaclResource.SHACL_NAME_PROPERTY:
          Map<String, Object> nameLiteral = this.objectMapper.convertValue(shapeFieldNode.get(0), Map.class);
          inputModel.put(StringResource.getLocalName(shapeField), nameLiteral);
          if (!defaultVals.isEmpty()) {
            String parsedField = nameLiteral.get(ShaclResource.VAL_KEY).toString().replace(ShaclResource.WHITE_SPACE,
                "_");
            inputModel.put("defaultValue", defaultVals.get(parsedField));
          }
          break;
        case ShaclResource.SHACL_ORDER_PROPERTY:
          Map<String, Object> orderMap = this.objectMapper.convertValue(shapeFieldNode.get(0), Map.class);
          inputModel.put(StringResource.getLocalName(shapeField),
              Integer.valueOf(orderMap.get(ShaclResource.VAL_KEY).toString()));
          break;
        case ShaclResource.SHACL_DATA_TYPE_PROPERTY:
          // Data types are stored in @id key with xsd namespace
          // But we are only interested in the local name and extract it accordingly
          Map<String, Object> dataType = this.objectMapper.convertValue(shapeFieldNode.get(0), Map.class);
          inputModel.put(StringResource.getLocalName(shapeField),
              StringResource.getLocalName(dataType.get(ShaclResource.ID_KEY).toString()));
          break;
        case ShaclResource.SHACL_IN_PROPERTY:
          ArrayNode inArray = (ArrayNode) shapeFieldNode;
          // Iterate and remove any blank node values
          Iterator<JsonNode> elements = inArray.elements();
          while (elements.hasNext()) {
            JsonNode currentElement = elements.next();
            if (currentElement.isObject()) {
              String valueConstraint = currentElement.get(ShaclResource.ID_KEY).asText();
              if (valueConstraint.startsWith("_:")) {
                elements.remove(); // Remove the current blank node
                break; // break iteration if blank node is found assuming only one blank node
              }
            }
          }
          // Convert the new array and append it into the output
          inputModel.put(ShaclResource.IN_PROPERTY, this.objectMapper.convertValue(inArray, List.class));
          break;
        default:
          // Every other fields are stored as a nested JSON object of key:value pair
          // within a one item JSON array
          inputModel.put(StringResource.getLocalName(shapeField),
              this.objectMapper.convertValue(shapeFieldNode.get(0), Map.class));
          break;
      }
    }
    return inputModel;
  }

  /**
   * Generates the Spring Boot compliant JSON response output from the target
   * properties.
   * 
   * @param properties target properties.
   */
  private List<Map<String, Object>> genOutputs(Map<String, Map<String, Object>> properties) {
    return properties.values().stream().map(propOrGroup -> {
      // For a property group which has `property` relations,
      // sort the properties before appending the group
      if (propOrGroup.containsKey(ShaclResource.PROPERTY_PROPERTY)) {
        List<Map<String, Object>> groupProperties = (List<Map<String, Object>>) propOrGroup
            .get(ShaclResource.PROPERTY_PROPERTY);
        groupProperties.sort(Comparator.comparingInt(map -> (int) map.get(ShaclResource.ORDER_PROPERTY)));
        propOrGroup.put(ShaclResource.PROPERTY_PROPERTY, groupProperties);
      }
      return propOrGroup;
    })
    // Sort the results based on order
    .sorted(Comparator.comparingInt(map -> (int) map.get(ShaclResource.ORDER_PROPERTY)))
    .toList();
  }
}
