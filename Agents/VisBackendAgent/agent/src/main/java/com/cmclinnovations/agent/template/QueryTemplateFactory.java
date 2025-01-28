package com.cmclinnovations.agent.template;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Queue;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.SparqlQueryLine;
import com.cmclinnovations.agent.model.type.LifecycleEventType;
import com.cmclinnovations.agent.utils.LifecycleResource;
import com.cmclinnovations.agent.utils.ShaclResource;
import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;

public class QueryTemplateFactory {
  private String parentField;
  private List<String> sortedVars;
  private Map<String, String> queryLines;
  private Map<String, List<Integer>> varSequence;
  private final ObjectMapper objectMapper;
  private static final String ID_PATTERN_1 = "<([^>]+)>/\\^<\\1>";
  private static final String ID_PATTERN_2 = "\\^<([^>]+)>/<\\1>";
  private static final String CLAZZ_VAR = "clazz";
  private static final String NAME_VAR = "name";
  private static final String INSTANCE_CLASS_VAR = "instance_clazz";
  private static final String ORDER_VAR = "order";
  private static final String IS_OPTIONAL_VAR = "isoptional";
  private static final String IS_PARENT_VAR = "isparent";
  private static final String IS_CLASS_VAR = "isclass";
  private static final String SUBJECT_VAR = "subject";
  private static final String PATH_PREFIX = "_proppath";
  private static final String MULTIPATH_VAR = "multipath";
  private static final String NODE_NAME_VAR = "nodename";
  private static final String MULTI_NAME_PATH_VAR = "name_multipath";
  private static final String RDF_TYPE = "rdf:type";
  private static final Logger LOGGER = LogManager.getLogger(QueryTemplateFactory.class);

  /**
   * Constructs a new query template factory.
   * 
   */
  public QueryTemplateFactory(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
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
   * @param bindings       The bindings queried from SHACL restrictions that
   *                       should be in the template.
   * @param filterId       An optional field to target the query at a specific
   *                       instance.
   * @param hasParent      Indicates if the query needs to filter out parent
   *                       entities.
   * @param lifecycleEvent Optional parameter to dictate the filter for a relevant
   *                       lifecycle event.
   */
  public Queue<String> genGetTemplate(Queue<Queue<SparqlBinding>> bindings, String filterId, boolean hasParent,
      LifecycleEventType lifecycleEvent) {
    // Validate inputs
    if (hasParent && filterId == null) {
      LOGGER.error("Detected a parent without a valid filter ID!");
      throw new IllegalArgumentException("Detected a parent without a valid filter ID!");
    }
    // Reset from previous iterations
    this.parentField = "";
    this.queryLines = new HashMap<>();
    this.varSequence = new HashMap<>();
    this.sortedVars = new ArrayList<>();
    // Code starts after reset and validation
    LOGGER.info("Generating a query template for getting data...");
    StringBuilder selectVariableBuilder = new StringBuilder();
    StringBuilder whereBuilder = new StringBuilder();
    // Extract the first binding class but it should not be removed from the queue
    String targetClass = bindings.peek().peek().getFieldValue(CLAZZ_VAR);
    this.sortBindings(bindings, hasParent);

    // Retrieve all variables if no sequence of variable is present
    if (this.varSequence.isEmpty()) {
      selectVariableBuilder.append("*");
    } else {
      // Else sort the variable and add them to the query
      // Add a status variable for lifecycle if available
      if (lifecycleEvent != null) {
        this.varSequence.put(LifecycleResource.STATUS_KEY, Stream.of(1, 0).toList());
        this.varSequence.put(LifecycleResource.SCHEDULE_START_DATE_KEY, Stream.of(2, 0).toList());
        this.varSequence.put(LifecycleResource.SCHEDULE_END_DATE_KEY, Stream.of(2, 1).toList());
        this.varSequence.put(LifecycleResource.SCHEDULE_START_TIME_KEY, Stream.of(2, 2).toList());
        this.varSequence.put(LifecycleResource.SCHEDULE_END_TIME_KEY, Stream.of(2, 3).toList());
        this.varSequence.put(LifecycleResource.SCHEDULE_TYPE_KEY, Stream.of(2, 4).toList());
      }
      this.sortedVars = new ArrayList<>(this.varSequence.keySet());
      this.sortedVars
          .sort((key1, key2) -> ShaclResource.compareLists(this.varSequence.get(key1), this.varSequence.get(key2)));
      // Append a ? before the property
      this.sortedVars.forEach(variable -> selectVariableBuilder.append(ShaclResource.VARIABLE_MARK)
          .append(StringResource.parseQueryVariable(variable))
          .append(ShaclResource.WHITE_SPACE));

    }
    this.queryLines.values().forEach(whereBuilder::append);
    this.appendOptionalIdFilters(whereBuilder, filterId, hasParent);
    this.appendOptionalLifecycleFilters(whereBuilder, lifecycleEvent);
    return this.genFederatedQuery(selectVariableBuilder.toString(), whereBuilder.toString(), targetClass);
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
  public Queue<String> genSearchTemplate(Queue<Queue<SparqlBinding>> bindings, Map<String, String> criterias) {
    // Reset from previous iterations
    this.parentField = "";
    this.queryLines = new HashMap<>();
    this.varSequence = new HashMap<>();
    this.sortedVars = new ArrayList<>();
    // Code starts after reset
    LOGGER.info("Generating a query template for getting the data that matches the search criteria...");
    StringBuilder whereBuilder = new StringBuilder();
    StringBuilder filters = new StringBuilder();
    // Extract the first binding class but it should not be removed from the queue
    String targetClass = bindings.peek().peek().getFieldValue(CLAZZ_VAR);
    this.sortBindings(bindings, false);

    this.queryLines.entrySet().forEach(currentLine -> {
      String variable = currentLine.getKey();
      // Do not generate or act on any id query lines
      if (!variable.equals("id")) {
        // note that if no criteria or empty string is passed in the API, the filter
        // will not be added
        if (criterias.containsKey(variable) && !criterias.get(variable).isEmpty()) {
          // If there is no search filters to be added, this variable should not be added
          String searchFilters = genSearchCriteria(variable, criterias);
          if (!searchFilters.isEmpty()) {
            whereBuilder.append(currentLine.getValue());
            filters.append(searchFilters);
          }
        }
      }
    });
    return this.genFederatedQuery("?iri", whereBuilder.append(filters).toString(), targetClass);
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
   * Retrieve the sequence of the variables.
   */
  public List<String> getSequence() {
    return this.sortedVars;
  }

  /**
   * Generates two federated queries with a replaceable endpoint [endpoint]. The
   * first query is for mixed endpoints; the second query is for non-ontop
   * endpoints.
   * 
   * @param selectVariables The variables to be selected in a SPARQL SELECT query.
   * @param whereClause     The contents for the SPARQL query's WHERE clause.
   * @param targetClass     Target class to reach.
   */
  private Queue<String> genFederatedQuery(String selectVariables, String whereClause, String targetClass) {
    Queue<String> results = new ArrayDeque<>();
    String iriClass = StringResource.parseIriForQuery(targetClass);
    results.offer(
        "SELECT DISTINCT " + selectVariables + " WHERE {?iri a " + iriClass + ShaclResource.FULL_STOP + whereClause
            + "}");
    results.offer(
        "SELECT DISTINCT " + selectVariables + " WHERE {?iri a/rdfs:subClassOf+ " + iriClass + ShaclResource.FULL_STOP
            + whereClause + "}");
    return results;
  }

  /**
   * Sort and categorise the bindings into the default and optional queues for
   * processing.
   * 
   * @param nestedBindings The bindings queried from SHACL restrictions that
   *                       should
   *                       be queried in template.
   * @param hasParent      Indicates if the query needs to filter out parent
   *                       entities.
   */
  private void sortBindings(Queue<Queue<SparqlBinding>> nestedBindings, boolean hasParent) {
    Map<String, SparqlQueryLine> queryLineMappings = new HashMap<>();
    while (!nestedBindings.isEmpty()) {
      Queue<SparqlBinding> bindings = nestedBindings.poll();
      Queue<String> parents = new ArrayDeque<>();
      while (!bindings.isEmpty()) {
        SparqlBinding binding = bindings.poll();
        String multiPartPredicate = this.getPredicate(binding, MULTIPATH_VAR);
        String multiPartLabelPredicate = this.getPredicate(binding, MULTI_NAME_PATH_VAR);
        String parent = this.genQueryLine(binding, multiPartPredicate, multiPartLabelPredicate, hasParent,
            queryLineMappings);
        if (parent != null) {
          parents.offer(parent);
        }
      }
      while (!parents.isEmpty()) {
        String parentField = parents.poll();
        queryLineMappings.remove(parentField);
        this.varSequence.remove(parentField);
      }
    }
    this.parseQueryLines(queryLineMappings);
  }

  /**
   * Gets the predicate associated with the input variable. Returns an empty
   * string if not found.
   * 
   * @param binding              An individual binding queried from SHACL
   *                             restrictions that should be queried in template.
   * @param propertyPathVariable The current property path part variable name.
   */
  private String getPredicate(SparqlBinding binding, String propertyPathVariable) {
    if (binding.containsField(propertyPathVariable)) {
      String predPath = binding.getFieldValue(propertyPathVariable);
      // Do not process any paths without the http protocol as it is likely to be a
      // blank node
      if (predPath.startsWith("http")) {
        String parsedPredPath = StringResource.parseIriForQuery(predPath);
        // Check if there are path prefixes in the SHACL restrictions
        // Each clause should be separated as we may use other path prefixes in future
        if (binding.containsField(propertyPathVariable + PATH_PREFIX)) {
          // For inverse paths, simply append a ^ before the parsed IRI
          if (binding.getFieldValue(propertyPathVariable + PATH_PREFIX)
              .equals(ShaclResource.SHACL_PREFIX + "inversePath")) {
            return "^" + parsedPredPath;
          }
        }
        // If no path prefixes are available, simply return the <predicate>
        return parsedPredPath;
      }
    }
    return "";
  }

  /**
   * Generates a query line from the input binding and store it within the target
   * mappings.
   * 
   * @param binding                 An individual binding queried from SHACL
   *                                restrictions that should be queried in
   *                                template.
   * @param multiPartPredicate      The current predicate part value for the main
   *                                property.
   * @param multiPartLabelPredicate The current predicate part value to reach the
   *                                label of the property.
   * @param hasParent               Indicates if there is supposed to be a parent
   *                                field.
   * @param queryLineMappings       The target mappings storing the generated
   *                                query
   *                                lines.
   */
  private String genQueryLine(SparqlBinding binding, String multiPartPredicate, String multiPartLabelPredicate,
      boolean hasParent, Map<String, SparqlQueryLine> queryLineMappings) {
    String parentNode = binding.getFieldValue(NODE_NAME_VAR);
    String propertyName = binding.getFieldValue(NAME_VAR);
    // Filter out any nested id properties
    if (parentNode != null && propertyName.equals("id")) {
      return null;
    }
    // Parse the isclass variable if it exists, or else defaults to false
    String isClassVal = binding.getFieldValue(IS_CLASS_VAR);
    boolean isClassVar = isClassVal != null ? Boolean.parseBoolean(isClassVal) : false;
    String instanceClass = binding.getFieldValue(INSTANCE_CLASS_VAR);
    // For existing mappings,
    if (queryLineMappings.containsKey(propertyName)) {
      SparqlQueryLine currentQueryLine = queryLineMappings.get(propertyName);
      // Update the mapping with the extended predicates
      queryLineMappings.put(propertyName,
          new SparqlQueryLine(propertyName, instanceClass,
              parsePredicate(currentQueryLine.predicate(), multiPartPredicate),
              parsePredicate(currentQueryLine.labelPredicate(), multiPartLabelPredicate),
              currentQueryLine.subjectFilter(), currentQueryLine.isOptional(), isClassVar));
    } else {
      // When initialising a new query line
      String subjectVar = binding.containsField(SUBJECT_VAR) ? binding.getFieldValue(SUBJECT_VAR) : "";
      boolean isOptional = Boolean.parseBoolean(binding.getFieldValue(IS_OPTIONAL_VAR));
      // Parse ordering only for label query, as we require the heading order in csv
      // Order field will not exist for non-label query
      if (binding.containsField(ORDER_VAR)) {
        int order = Integer.parseInt(binding.getFieldValue(ORDER_VAR));
        List<Integer> orders = new ArrayList<>();
        // If there is an existing parent node, append the order in front
        if (parentNode != null && this.varSequence.containsKey(parentNode)) {
          orders.addAll(this.varSequence.get(parentNode));
        }
        orders.add(order);
        this.varSequence.put(propertyName, orders);
      }
      // If the field is a parent field, and the template requires a parent, store the
      // parent field
      if (Boolean.parseBoolean(binding.getFieldValue(IS_PARENT_VAR)) && hasParent) {
        this.parentField = StringResource.parseQueryVariable(propertyName);
      }
      if (parentNode != null) {
        SparqlQueryLine parentLine = queryLineMappings.get(parentNode);
        multiPartPredicate = parsePredicate(parentLine.predicate(), multiPartPredicate);
      }
      queryLineMappings.put(propertyName,
          new SparqlQueryLine(propertyName, instanceClass, multiPartPredicate, multiPartLabelPredicate, subjectVar,
              isOptional, isClassVar));
    }
    return parentNode;
  }

  /**
   * Parses the predicate to concatenante the current and next predicate in a
   * SPARQL compliant format.
   * 
   * @param currentPredicate Current predicate in the existing mapping
   * @param nextPredicate    Next predicate for appending.
   */
  private String parsePredicate(String currentPredicate, String nextPredicate) {
    if (nextPredicate.isEmpty()) {
      return currentPredicate;
    }
    if (currentPredicate.isEmpty()) {
      return nextPredicate;
    } else {
      return currentPredicate + "/" + nextPredicate;
    }
  }

  /**
   * Parses the triple query line into the mappings at the class level.
   * 
   * @param queryLineMappings The input unparsed query lines
   */
  private void parseQueryLines(Map<String, SparqlQueryLine> queryLineMappings) {
    queryLineMappings.values().forEach(queryLine -> {
      // Parse and generate a query line for the current line
      StringBuilder currentLine = new StringBuilder();
      String jointPredicate = parsePredicate(queryLine.predicate(), queryLine.labelPredicate());
      // Add a final rdfs:label if it is a class to retrieve the label
      if (queryLine.isClazz()) {
        jointPredicate = parsePredicate(jointPredicate, ShaclResource.RDFS_LABEL_PREDICATE);
      }
      // If query line is id with a roundabout loop to target itself
      if (queryLine.property().equals("id") && this.verifySelfTargetIdField(jointPredicate)) {
        // Simply bind the iri as the id
        currentLine.append("BIND(?iri AS ?id)");
      } else {
        StringResource.appendTriple(currentLine, "?iri", jointPredicate,
            // Note to add a _ to the property
            ShaclResource.VARIABLE_MARK + StringResource.parseQueryVariable(queryLine.property()));
      }
      // If this is an instance, add a statement targeting the exact class
      if (queryLine.instanceClass() != null) {
        // Inverse the label predicate if it exist
        String inverseLabelPred = !queryLine.labelPredicate().isEmpty() ? "^(" + queryLine.labelPredicate() + ")/" : "";
        StringResource.appendTriple(currentLine,
            ShaclResource.VARIABLE_MARK + StringResource.parseQueryVariable(queryLine.property()),
            inverseLabelPred + RDF_TYPE, StringResource.parseIriForQuery(queryLine.instanceClass()));
      }
      // Optional lines should be parsed differently
      if (queryLine.isOptional()) {
        // If the value must conform to a specific subject variable,
        // a filter needs to be added directly to the same optional clause
        if (!queryLine.subjectFilter().isEmpty()) {
          Map<String, String> searchCriteria = new HashMap<>();
          searchCriteria.put(queryLine.property(), queryLine.subjectFilter());
          currentLine.append(genSearchCriteria(queryLine.property(), searchCriteria));
        }
        String optionalLine = StringResource.genOptionalClause(currentLine.toString());
        this.queryLines.put(queryLine.property(), optionalLine);
      } else {
        // Non-optional lines does not require special effects
        this.queryLines.put(queryLine.property(), currentLine.toString());
      }
    });
  }

  /**
   * Verifies if the ID field is targeting the IRI.
   * 
   * @param predicate The predicate string of the ID field.
   */
  private boolean verifySelfTargetIdField(String predicate) {
    // Compile the potential patterns to match
    Pattern pattern1 = Pattern.compile(ID_PATTERN_1);
    Pattern pattern2 = Pattern.compile(ID_PATTERN_2);

    // Create matchers for both patterns
    Matcher matcher1 = pattern1.matcher(predicate);
    Matcher matcher2 = pattern2.matcher(predicate);

    // Return true if input matches either pattern 1 or pattern 2
    return matcher1.matches() || matcher2.matches();
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
   * Appends optional lifecycle filter if required based on the specified event.
   * 
   * @param query          Builder for the query template.
   * @param lifecycleEvent Target event for filter.
   */
  private void appendOptionalLifecycleFilters(StringBuilder query, LifecycleEventType lifecycleEvent) {
    if (lifecycleEvent != null) {
      query.append(LifecycleResource.genReadableScheduleQuery());
      switch (lifecycleEvent) {
        case LifecycleEventType.APPROVED:
          LifecycleResource.appendFilterExists(query, false, LifecycleResource.EVENT_APPROVAL);
          break;
        case LifecycleEventType.SERVICE_EXECUTION:
          LifecycleResource.appendFilterExists(query, true, LifecycleResource.EVENT_APPROVAL);
          LifecycleResource.appendArchivedFilterExists(query, false);
          break;
        case LifecycleEventType.ARCHIVE_COMPLETION:
          LifecycleResource.appendArchivedStateQuery(query);
          break;
        default:
          // Do nothing if it doesnt meet the above events
          break;
      }
    }
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
    String formattedVar = StringResource.parseQueryVariable(variable);
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
    String idTripleSubject = this.getFormattedQueryVariable(currentNode.path(ShaclResource.ID_KEY),
        targetId);
    Iterator<Map.Entry<String, JsonNode>> iterator = currentNode.fields();
    while (iterator.hasNext()) {
      Map.Entry<String, JsonNode> field = iterator.next();
      JsonNode fieldNode = field.getValue();
      // Create the following query line for all @type fields
      if (field.getKey().equals(ShaclResource.TYPE_KEY)) {
        String typeTripleObject = this.getFormattedQueryVariable(fieldNode, targetId);
        StringResource.appendTriple(deleteBuilder, idTripleSubject, RDF_TYPE, typeTripleObject);
        StringResource.appendTriple(whereBuilder, idTripleSubject, RDF_TYPE, typeTripleObject);
        // For all @reverse fields
      } else if (field.getKey().equals(ShaclResource.REVERSE_KEY)) {
        if (fieldNode.isArray()) {
          LOGGER.error(
              "Invalid reverse predicate JSON-LD schema for {}! Fields must be stored in an object!",
              idTripleSubject);
          throw new IllegalArgumentException(
              "Invalid reverse predicate JSON-LD schema! Fields must be stored in an object!");
        } else if (fieldNode.isObject()) {
          // Reverse fields must be an object that may contain one or multiple fields
          Iterator<String> fieldIterator = fieldNode.fieldNames();
          while (fieldIterator.hasNext()) {
            String reversePredicate = fieldIterator.next();
            this.parseNestedNode(currentNode.path(ShaclResource.ID_KEY), fieldNode.path(reversePredicate),
                reversePredicate, deleteBuilder, whereBuilder, targetId, true);
          }
        }
        // The @id and @context field should be ignored but continue parsing for
        // everything else
      } else if (!field.getKey().equals(ShaclResource.ID_KEY) && !field.getKey().equals(ShaclResource.CONTEXT_KEY)) {
        this.parseFieldNode(currentNode.path(ShaclResource.ID_KEY), fieldNode, idTripleSubject, field.getKey(),
            deleteBuilder, whereBuilder, targetId);
      }
    }
  }

  /**
   * Retrieves the query variable from the replacement node as either an IRI or
   * query variable.
   * 
   * @param replacementNode Target for retrieval. Node must be an Object or Text
   *                        Node.
   * @param targetId        The target instance IRI.
   */
  private String getFormattedQueryVariable(JsonNode replacementNode, String targetId) {
    // If it is an object, it is definitely a replacement object, and retriving the
    // @replace key is sufficient;
    if (replacementNode.isObject()) {
      String replacementId = replacementNode.path(ShaclResource.REPLACE_KEY).asText();
      String replacementType = replacementNode.path(ShaclResource.TYPE_KEY).asText();
      // Only the id replacement field with prefixes will be returned as an IRI
      if (replacementType.equals("iri") && replacementId.equals("id")) {
        return StringResource.parseIriForQuery(replacementNode.path("prefix").asText() + targetId);
      }
      return ShaclResource.VARIABLE_MARK + StringResource.parseQueryVariable(replacementId);
    } else {
      // Otherwise, default to text
      return StringResource.parseIriForQuery(((TextNode) replacementNode).textValue());
    }
  }

  /**
   * Parses any field node.
   * 
   * @param idNode        The ID node of the current node.
   * @param fieldNode     The field node of the current node.
   * @param subject       The node acting as the subject of the triple.
   * @param predicate     The predicate path of the triple.
   * @param deleteBuilder A query builder for the DELETE clause.
   * @param whereBuilder  A query builder for the WHERE clause.
   * @param targetId      The target instance IRI.
   */
  private void parseFieldNode(JsonNode idNode, JsonNode fieldNode, String subject, String predicate,
      StringBuilder deleteBuilder, StringBuilder whereBuilder, String targetId) {
    // For object field node
    if (fieldNode.isObject()) {
      JsonNode targetTripleObjectNode = fieldNode.has(ShaclResource.REPLACE_KEY)
          ? fieldNode
          : fieldNode.path(ShaclResource.ID_KEY);
      String formattedPredicate = StringResource.parseIriForQuery(predicate);
      String formattedObjVar = this.getFormattedQueryVariable(targetTripleObjectNode, targetId);
      StringResource.appendTriple(deleteBuilder, subject, formattedPredicate, formattedObjVar);
      StringResource.appendTriple(whereBuilder, subject, formattedPredicate, formattedObjVar);
      // Further processing for only pricing replacement object
      if (fieldNode.has(ShaclResource.REPLACE_KEY)
          && fieldNode.path(ShaclResource.REPLACE_KEY).asText().equals("pricing")) {
        this.appendPricingModelStatements(formattedObjVar, deleteBuilder, whereBuilder);
      }
      // No further processing required for objects intended for replacement, @value,
      if (!fieldNode.has(ShaclResource.REPLACE_KEY) && !fieldNode.has(ShaclResource.VAL_KEY) &&
      // or a one line instance link to a TextNode eg: "@id" : "instanceIri"
          !(fieldNode.has(ShaclResource.ID_KEY) && fieldNode.size() == 1
              && fieldNode.path(ShaclResource.ID_KEY).isTextual())) {
        this.recursiveParseNode(deleteBuilder, whereBuilder, (ObjectNode) fieldNode, targetId);
      }
      // For arrays,iterate through each object and parse the nested node
    } else if (fieldNode.isArray()) {
      ArrayNode fieldArray = (ArrayNode) fieldNode;
      for (JsonNode tripleObjNode : fieldArray) {
        this.parseNestedNode(idNode, tripleObjNode, predicate, deleteBuilder, whereBuilder, targetId, false);
      }
    }
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
        // A reverse node indicates that the replacement object should now be the
        // subject and the Id Node should become the object
        if (objectNode.has(ShaclResource.REPLACE_KEY)) {
          String replacementVar = this.getFormattedQueryVariable(objectNode, null);
          this.parseFieldNode(null, idNode, replacementVar, predicatePath,
              deleteBuilder, whereBuilder, targetId);
        } else {
          // A reverse node indicates that the original object should now be the subject
          // And the Id Node should become the object
          ObjectNode nestedReverseNode = (ObjectNode) objectNode;
          nestedReverseNode.set(predicatePath, idNode);
          this.recursiveParseNode(deleteBuilder, whereBuilder, nestedReverseNode, targetId);
        }
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

  /**
   * Append pricing model statements for the DELETE builder.
   * 
   * @param subjectVar    The subject of the pricing model as a variable.
   * @param deleteBuilder A query builder for the DELETE clause.
   * @param whereBuilder  A query builder for the WHERE clause.
   */
  private void appendPricingModelStatements(String subjectVar, StringBuilder deleteBuilder,
      StringBuilder whereBuilder) {
    String feeVar = ShaclResource.VARIABLE_MARK + "fee";
    String anyPredVar = ShaclResource.VARIABLE_MARK + "anypred";
    String anyObjectVar = ShaclResource.VARIABLE_MARK + "object";
    StringResource.appendTriple(deleteBuilder, subjectVar, RDF_TYPE,
        StringResource.parseIriForQuery(LifecycleResource.PRICING_MODEL));
    StringResource.appendTriple(whereBuilder, subjectVar, RDF_TYPE,
        StringResource.parseIriForQuery(LifecycleResource.PRICING_MODEL));
    StringResource.appendTriple(deleteBuilder, subjectVar,
        StringResource.parseIriForQuery(LifecycleResource.HAS_ARGUMENT_RELATIONS), feeVar);
    StringResource.appendTriple(whereBuilder, subjectVar,
        StringResource.parseIriForQuery(LifecycleResource.HAS_ARGUMENT_RELATIONS), feeVar);
    StringResource.appendTriple(deleteBuilder, subjectVar, anyPredVar, anyObjectVar);
    StringResource.appendTriple(whereBuilder, subjectVar, anyPredVar, anyObjectVar);
  }
}
