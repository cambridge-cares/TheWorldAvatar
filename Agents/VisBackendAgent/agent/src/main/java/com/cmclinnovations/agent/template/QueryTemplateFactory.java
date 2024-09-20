package com.cmclinnovations.agent.template;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Queue;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.SparqlQueryLine;
import com.cmclinnovations.agent.utils.ShaclResource;
import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class QueryTemplateFactory {
  private String parentField;
  private final Queue<SparqlQueryLine> queryLines;
  private final Queue<SparqlQueryLine> optionalQueryLines;
  private final ObjectMapper objectMapper;
  private static final String CLAZZ_VAR = "clazz";
  private static final String NAME_VAR = "name";
  private static final String IS_OPTIONAL_VAR = "isoptional";
  private static final String IS_PARENT_VAR = "isparent";
  private static final String SUBJECT_VAR = "subject";
  private static final String PATH_PREFIX = "_proppath";
  private static final String MULTIPATH_FIRST_VAR = "multipath1";
  private static final String MULTIPATH_SEC_VAR = "multipath2";
  private static final String MULTIPATH_THIRD_VAR = "multipath3";
  private static final String MULTISUBPATH_FIRST_VAR = "multisubpath1";
  private static final String MULTISUBPATH_SEC_VAR = "multisubpath2";
  private static final String MULTISUBPATH_THIRD_VAR = "multisubpath3";
  private static final String MULTISUBPATH_FORTH_VAR = "multisubpath4";
  private static final String MULTI_NAME_PATH_FIRST_VAR = "name_multipath1";
  private static final String MULTI_NAME_PATH_SEC_VAR = "name_multipath2";
  private static final String MULTI_NAME_PATH_THIRD_VAR = "name_multipath3";
  private static final List<String> propertyPathParts = Arrays.asList(
      MULTIPATH_FIRST_VAR,
      MULTIPATH_SEC_VAR,
      MULTIPATH_THIRD_VAR,
      MULTISUBPATH_FIRST_VAR,
      MULTISUBPATH_SEC_VAR,
      MULTISUBPATH_THIRD_VAR,
      MULTISUBPATH_FORTH_VAR,
      MULTI_NAME_PATH_FIRST_VAR,
      MULTI_NAME_PATH_SEC_VAR,
      MULTI_NAME_PATH_THIRD_VAR);

  private static final Logger LOGGER = LogManager.getLogger(QueryTemplateFactory.class);

  /**
   * Constructs a new query template factory.
   * 
   */
  public QueryTemplateFactory(ObjectMapper objectMapper) {
    this.objectMapper = new ObjectMapper();
    // Queues are populated and emptied within each method call and does not need to
    // be reset
    this.queryLines = new ArrayDeque<>();
    this.optionalQueryLines = new ArrayDeque<>();
  }

  /**
   * Generate a SPARQL query template to get the required data. It will typically
   * be in the following format:
   * 
   * SELECT * WHERE {
   * ?iri a <clazz_iri>.
   * ?iri <prop_path>/<sub_path> ?property1.
   * ?iri <prop_path>/<prop_path2> ?property2.
   * }
   * 
   * @param bindings  The bindings queried from SHACL restrictions that should
   *                  be in the template.
   * @param filterId  An optional field to target the query at a specific
   *                  instance.
   * @param hasParent Indicates if the query needs to filter out parent entities.
   */
  public String genGetTemplate(List<SparqlBinding> bindings, String filterId, boolean hasParent) {
    // Validate inputs
    if (hasParent && filterId == null) {
      LOGGER.error("Detected a parent without a valid filter ID!");
      throw new IllegalArgumentException("Detected a parent without a valid filter ID!");
    }

    LOGGER.info("Generating a query template for getting data...");
    this.sortBindings(bindings, hasParent);
    StringBuilder query = new StringBuilder();
    query.append("SELECT * WHERE {")
        .append(genIriClassLine(bindings));
    // Parse the query lines
    while (!this.queryLines.isEmpty()) {
      query.append(this.queryLines.poll().contents());
    }
    // Parse the optional lines
    while (!this.optionalQueryLines.isEmpty()) {
      SparqlQueryLine currentLine = this.optionalQueryLines.poll();
      query.append(genOptionalLine(currentLine.contents()));
    }
    appendOptionalIdFilters(query, filterId, hasParent);
    // Close the query
    return query.append("}").toString();
  }

  /**
   * Generate a SPARQL query template to get the data that meets the search
   * criteria. It will typically be in the following format:
   * 
   * SELECT ?iri WHERE {
   * ?iri a <clazz_iri>.
   * ?iri <prop_path>/<sub_path> ?string_property.
   * ?iri <prop_path>/<sub_number_path> ?number_property.
   * FILTER(STR(?string_property) = STR("string criteria"))
   * FILTER(?number_property >= min_value && ?number_property <= max_value)
   * }
   * 
   * @param bindings  The bindings queried from SHACL restrictions that should
   *                  be in the template.
   * @param criterias All the required search criteria.
   */
  public String genSearchTemplate(List<SparqlBinding> bindings, Map<String, String> criterias) {
    LOGGER.info("Generating a query template for getting the data that matches the search criteria...");
    this.sortBindings(bindings, false);
    StringBuilder query = new StringBuilder();
    StringBuilder filters = new StringBuilder();
    query.append("SELECT ?iri WHERE {")
        .append(genIriClassLine(bindings));
    while (!this.queryLines.isEmpty()) {
      SparqlQueryLine currentLine = this.queryLines.poll();
      String variable = currentLine.property();
      // note that if no criteria is passed in the API, the filter will not be added
      if (criterias.containsKey(variable)) {
        filters.append(genSearchCriteria(variable, criterias));
      }
      // Do not generate any id query lines
      if (!variable.equals("id")) {
        query.append(currentLine.contents());
      }
    }
    while (!this.optionalQueryLines.isEmpty()) {
      SparqlQueryLine currentLine = this.optionalQueryLines.poll();
      String variable = currentLine.property();
      if (criterias.containsKey(variable)) {
        filters.append(genSearchCriteria(variable, criterias));
      }
      // Do not generate any id query lines
      if (!variable.equals("id")) {
        query.append(genOptionalLine(currentLine.contents()));
      }
    }

    // Close the query
    return query.append(filters).append("}").toString();
  }

  /**
   * Generate a SPARQL query template to delete the target instance.
   * 
   * @param rootNode The root node of contents to parse into a template
   * @param targetId The target instance IRI.
   */
  public String genDeleteQueryTemplate(ObjectNode rootNode, String targetId) {
    StringBuilder deleteBuilder = new StringBuilder();
    StringBuilder whereBuilder = new StringBuilder();
    this.recursiveParseNode(deleteBuilder, whereBuilder, rootNode, targetId);
    return "DELETE {" + deleteBuilder.toString() + "} WHERE {" + whereBuilder.toString() + "}";
  }

  /**
   * Generates the class restriction line of a query ie:
   * ?iri a <clazz_iri>.
   * 
   * @param bindings The bindings queried from SHACL restrictions that should
   *                 be queried in template.
   */
  private String genIriClassLine(List<SparqlBinding> bindings) {
    // Retrieve the target class from the first binding
    String targetClass = bindings.get(0).getFieldValue(CLAZZ_VAR);
    return "?iri a " + StringResource.parseIriForQuery(targetClass) + ShaclResource.FULL_STOP;
  }

  /**
   * Generates the optional query line of a query ie:
   * OPTIONAL{?iri <proppath> ?var.}
   * 
   * @param bindings The bindings queried from SHACL restrictions that should
   *                 be queried in template.
   */
  private String genOptionalLine(String content) {
    return "OPTIONAL{" + content + "}";
  }

  /**
   * Generates the search criteria query line of a query ie:
   * FILTER(STR(?var) = STR(string_criteria))
   * 
   * @param variable The name of the variable.
   * @param criteria The criteria to be met.
   */
  private String genSearchCriteria(String variable, Map<String, String> criterias) {
    String criteriaVal = criterias.get(variable);
    String formattedVar = variable.replaceAll("\\s+", "_");
    if (criteriaVal.isEmpty()) {
      return criteriaVal;
    }
    // The front end will return a range value if range parsing is required
    if (criteriaVal.equals("range")) {
      String rangeQuery = "";
      String minCriteriaVal = criterias.get("min " + variable);
      String maxCriteriaVal = criterias.get("max " + variable);
      // Append min filter if available
      if (!minCriteriaVal.isEmpty()) {
        rangeQuery += " FILTER(?" + formattedVar + " >= " + criterias.get("min " + variable);
      }
      // Append max filter if available
      if (!maxCriteriaVal.isEmpty()) {
        // Prefix should be a conditional && if the min filter is already present
        rangeQuery += rangeQuery.isEmpty() ? " FILTER(?" : " && ?";
        rangeQuery += formattedVar + " <= " + maxCriteriaVal;
      }
      if (!rangeQuery.isEmpty()) {
        rangeQuery += ")";
      }
      // Return empty string otherwise
      return rangeQuery;
    }
    return " FILTER(STR(?" + formattedVar + ") = \"" + criteriaVal + "\")";
  }

  /**
   * Sort and categorise the bindings into the default and optional queues for
   * processing.
   * 
   * @param bindings  The bindings queried from SHACL restrictions that should
   *                  be queried in template.
   * @param hasParent Indicates if the query needs to filter out parent entities.
   */
  private void sortBindings(List<SparqlBinding> bindings, boolean hasParent) {
    bindings.forEach(binding -> {
      String predSubjectLine = this.genTripleQueryLine(binding, hasParent);
      this.categoriseQueryLine(predSubjectLine, binding);
    });
  }

  /**
   * Generate the subject predicate object line
   * ie ?iri <prop_path>/<sub_path> ?property1.
   * 
   * @param binding   An individual binding queried from SHACL restrictions that
   *                  should be queried in template.
   * @param hasParent Indicates if the query needs to filter out parent entities.
   */
  private String genTripleQueryLine(SparqlBinding binding, boolean hasParent) {
    // Generate an IRI variable at the start
    StringBuilder tripleQueryBuilder = new StringBuilder("?iri ");
    StringBuilder predObjectQueryBuilder = new StringBuilder();

    // Iterate through the nested predicate paths
    propertyPathParts.forEach(propertyPathPart -> {
      if (binding.containsField(propertyPathPart)) {
        String predPath = binding.getFieldValue(propertyPathPart);
        // Do not process any paths without the http protocol as it is likely to be a
        // blank node
        if (predPath.startsWith("http")) {
          // If this is not the first part, there should be a / to denote a sequence path
          if (predObjectQueryBuilder.length() > 0) {
            predObjectQueryBuilder.append("/");
          }
          this.appendPropertyPathExpressionIfAvailable(predObjectQueryBuilder, propertyPathPart, binding);
          predObjectQueryBuilder.append(StringResource.parseIriForQuery(predPath));
        }
      }
    });
    String propertyName = binding.getFieldValue(NAME_VAR).replaceAll("\\s+", "_");
    // If the field is a parent field, and the template requires a parent, store the
    // parent field
    if (Boolean.parseBoolean(binding.getFieldValue(IS_PARENT_VAR)) && hasParent) {
      this.parentField = propertyName;
    }
    // Close with the variable
    tripleQueryBuilder.append(predObjectQueryBuilder).append(" ?").append(propertyName).append(ShaclResource.FULL_STOP);
    return tripleQueryBuilder.toString();
  }

  /**
   * Appends an additional property path expression only if it is available
   * 
   * @param builder          Builder for the query template.
   * @param propertyPathPart The current property path part variable name.
   * @param binding          An individual binding queried from SHACL restrictions
   */
  private void appendPropertyPathExpressionIfAvailable(StringBuilder builder, String propertyPathPart,
      SparqlBinding binding) {
    if (binding.containsField(propertyPathPart + PATH_PREFIX)) {
      // Separate the clauses as there may be other path prefixes in future
      // For inverse paths, simply append a ^ before
      if (binding.getFieldValue(propertyPathPart + PATH_PREFIX)
          .equals(ShaclResource.SHACL_PREFIX + "inversePath")) {
        builder.append("^");
      }
    }
  }

  /**
   * Categorises the triple query line into optional or default queues.
   * 
   * @param inputLine The line of interest
   * @param binding   An individual binding queried from SHACL restrictions
   */
  private void categoriseQueryLine(String inputLine, SparqlBinding binding) {
    boolean isOptional = Boolean.parseBoolean(binding.getFieldValue(IS_OPTIONAL_VAR));
    String propertyName = binding.getFieldValue(NAME_VAR).replaceAll("\\s+", "_");
    if (isOptional) {
      // If the value must conform to a specific subject variable,
      // a filter needs to be added directly to the same optional clause
      if (binding.containsField(SUBJECT_VAR)) {
        Map<String, String> searchCriteria = new HashMap<>();
        searchCriteria.put(propertyName, binding.getFieldValue(SUBJECT_VAR));
        inputLine += genSearchCriteria(propertyName, searchCriteria);
      }
      // Sparql query line should use original name var without the replaced _
      this.optionalQueryLines.offer(new SparqlQueryLine(binding.getFieldValue(NAME_VAR), inputLine));
    } else {
      // Sparql query line should use original name var without the replaced _
      this.queryLines.offer(new SparqlQueryLine(binding.getFieldValue(NAME_VAR), inputLine));
    }
  }

  /**
   * Appends optional filters related to IDs to the query if required.
   * 
   * @param query     Builder for the query template.
   * @param filterId  An optional field to target the query at a specific
   *                  instance.
   * @param hasParent Indicates if the query needs to filter out parent entities.
   */
  private void appendOptionalIdFilters(StringBuilder query, String filterId, boolean hasParent) {
    if (hasParent) {
      if (this.parentField == null) {
        LOGGER.error("Detected a parent but no valid parent fields are available!");
        throw new IllegalArgumentException("Detected a parent but no valid parent fields are available!");
      }
      query.append("FILTER STRENDS(STR(?")
          .append(this.parentField)
          .append("), \"")
          .append(filterId)
          .append("\")");
    } else if (filterId != null) {
      // Add filter clause if there is a valid filter ID
      query.append("FILTER STRENDS(STR(?id), \"")
          .append(filterId)
          .append("\")");
    }
  }

  /**
   * Replace the placeholders in the current node and recursively for its children
   * nodes based on the corresponding value in the replacement mappings if
   * available.
   * 
   * @param deleteBuilder A query builder for the DELETE clause.
   * @param whereBuilder  A query builder for the WHERE clause.
   * @param currentNode   Input contents to perform operation on.
   * @param targetId      The target instance IRI.
   */
  private void recursiveParseNode(StringBuilder deleteBuilder, StringBuilder whereBuilder, ObjectNode currentNode,
      String targetId) {
    // First retrieve the ID value as a subject of the triple
    String idTripleSubject = this.getFormattedQueryVariable((ObjectNode) currentNode.path(ShaclResource.ID_KEY),
        targetId);
    Iterator<Map.Entry<String, JsonNode>> iterator = currentNode.fields();
    while (iterator.hasNext()) {
      Map.Entry<String, JsonNode> field = iterator.next();
      JsonNode fieldNode = field.getValue();
      // Create the following query line for all @type fields
      if (field.getKey().equals(ShaclResource.TYPE_KEY)) {
        // If it is an object, it is definitely a replacement object, and retriving the
        // @replace key is sufficient; Otherwise, default to text
        String typeTripleObject = fieldNode.isObject()
            ? this.getFormattedQueryVariable((ObjectNode) fieldNode, targetId)
            : StringResource.parseIriForQuery(fieldNode.asText());
        StringResource.appendTriple(deleteBuilder, idTripleSubject, "rdf:type", typeTripleObject);
        StringResource.appendTriple(whereBuilder, idTripleSubject, "rdf:type", typeTripleObject);
        // For all @reverse fields
      } else if (field.getKey().equals(ShaclResource.REVERSE_KEY)) {
        if (fieldNode.isArray()) {
          LOGGER.error(
              "Invalid reverse predicate JSON-LD schema for {}! Fields must be stored in an object!",
              idTripleSubject);
          throw new IllegalArgumentException(
              "Invalid reverse predicate JSON-LD schema! Fields must be stored in an object!");
        } else if (fieldNode.isObject()) {
          // Reverse fields must be an object that contains multiple fields
          String reversePredicate = fieldNode.fieldNames().next();
          this.parseNestedNode(currentNode.path(ShaclResource.ID_KEY), fieldNode.path(reversePredicate),
              reversePredicate, deleteBuilder, whereBuilder, targetId, true);
        }
        // The @id and @context field should be ignored but continue parsing for
        // everything else
      } else if (!field.getKey().equals(ShaclResource.ID_KEY) && !field.getKey().equals(ShaclResource.CONTEXT_KEY)) {
        if (fieldNode.isObject()) {
          JsonNode targetTripleObjectNode = this.isReplacementObject(fieldNode)
              ? fieldNode
              : fieldNode.path(ShaclResource.ID_KEY);
          StringResource.appendTriple(deleteBuilder, idTripleSubject, StringResource.parseIriForQuery(field.getKey()),
              this.getFormattedQueryVariable((ObjectNode) targetTripleObjectNode, targetId));
          StringResource.appendTriple(whereBuilder, idTripleSubject, StringResource.parseIriForQuery(field.getKey()),
              this.getFormattedQueryVariable((ObjectNode) targetTripleObjectNode, targetId));
          if (!this.isReplacementObject(fieldNode)) {
            this.recursiveParseNode(deleteBuilder, whereBuilder, (ObjectNode) fieldNode, targetId);
          }
          // For arrays, create a new object node containing the ID of the current node
          // and the same field property, and parse it as per normal
        } else if (fieldNode.isArray()) {
          ArrayNode fieldArray = (ArrayNode) fieldNode;
          for (JsonNode tripleObjNode : fieldArray) {
            this.parseNestedNode(currentNode.path(ShaclResource.ID_KEY), tripleObjNode, field.getKey(), deleteBuilder,
                whereBuilder, targetId, false);
          }
        }
      }
    }
  }

  /**
   * Checks if the target node is a replacement object with a @replace key.
   * 
   * @param targetNode Target for retrieval.
   */
  private boolean isReplacementObject(JsonNode targetNode) {
    return targetNode.isObject() && targetNode.has(ShaclResource.REPLACE_KEY);
  }

  /**
   * Retrieves the query variable from the replacement node as either an IRI or
   * query variable.
   * 
   * @param replacementNode Target for retrieval.
   * @param targetId        The target instance IRI.
   */
  private String getFormattedQueryVariable(ObjectNode replacementNode, String targetId) {
    String replacementId = replacementNode.path(ShaclResource.REPLACE_KEY).asText();
    String replacementType = replacementNode.path(ShaclResource.TYPE_KEY).asText();
    // Only the id replacement field with prefixes will be returned as an IRI
    if (replacementType.equals("iri") && replacementId.equals("id")) {
      return StringResource.parseIriForQuery(replacementNode.path("prefix").asText() + targetId);
    }
    return ShaclResource.VARIABLE_MARK + replacementId.replaceAll("\\s+", "_");
  }

  /**
   * Parses a nested node (two layers down) with the required parameters.
   * 
   * @param idNode        The ID node of the current top level node.
   * @param objectNode    The node acting as the object of the triple.
   * @param predicatePath The predicate path of the triple.
   * @param deleteBuilder A query builder for the DELETE clause.
   * @param whereBuilder  A query builder for the WHERE clause.
   * @param targetId      The target instance IRI.
   * @param isReverse     Indicates if the variable should be inverse or not.
   */
  private void parseNestedNode(JsonNode idNode, JsonNode objectNode, String predicatePath, StringBuilder deleteBuilder,
      StringBuilder whereBuilder, String targetId, boolean isReverse) {
    if (isReverse) {
      if (objectNode.isObject()) {
        // A reverse node indicates that the original object should now be the subject
        // And the Id Node should become the object
        ObjectNode nestedReverseNode = (ObjectNode) objectNode;
        nestedReverseNode.set(predicatePath, idNode);
        this.recursiveParseNode(deleteBuilder, whereBuilder, nestedReverseNode, targetId);
      } else if (objectNode.isArray()) {
        // For reverse arrays, iterate and recursively parse each object as a reverse
        // node
        ArrayNode objArray = (ArrayNode) objectNode;
        for (JsonNode nestedReverseObjNode : objArray) {
          this.parseNestedNode(idNode, nestedReverseObjNode, predicatePath, deleteBuilder, whereBuilder,
              targetId, true);
        }
      }
    } else {
      // This aspect is used for parsing arrays without any reversion
      // Creating a new node where the ID node is the parent is sufficient
      ObjectNode nestedNode = this.objectMapper.createObjectNode();
      nestedNode.set(ShaclResource.ID_KEY, idNode);
      nestedNode.set(predicatePath, objectNode);
      this.recursiveParseNode(deleteBuilder, whereBuilder, nestedNode, targetId);
    }
  }
}
