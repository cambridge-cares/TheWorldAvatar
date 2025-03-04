package com.cmclinnovations.agent.template;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.agent.model.ParentField;
import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.SparqlQueryLine;
import com.cmclinnovations.agent.model.type.LifecycleEventType;
import com.cmclinnovations.agent.service.core.JsonLdService;
import com.cmclinnovations.agent.utils.LifecycleResource;
import com.cmclinnovations.agent.utils.ShaclResource;
import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;

public class QueryTemplateFactory {
  private List<String> sortedVars;
  private Queue<StringBuilder> deleteBranchBuilders;
  private Map<String, String> queryLines;
  private Map<String, List<Integer>> varSequence;
  private Set<String> variables;
  private final LifecycleQueryFactory lifecycleQueryFactory;
  private final JsonLdService jsonLdService;
  public static final String NODE_GROUP_VAR = "nodegroup";
  public static final String NESTED_CLASS_VAR = "nested_class";
  private static final String ID_PATTERN_1 = "<([^>]+)>/\\^<\\1>";
  private static final String ID_PATTERN_2 = "\\^<([^>]+)>/<\\1>";
  private static final String CLAZZ_VAR = "clazz";
  private static final String NAME_VAR = "name";
  private static final String INSTANCE_CLASS_VAR = "instance_clazz";
  private static final String ORDER_VAR = "order";
  private static final String BRANCH_VAR = "branch";
  private static final String IS_OPTIONAL_VAR = "isoptional";
  private static final String IS_CLASS_VAR = "isclass";
  private static final String SUBJECT_VAR = "subject";
  private static final String PATH_PREFIX = "_proppath";
  private static final String MULTIPATH_VAR = "multipath";
  private static final String MULTI_NAME_PATH_VAR = "name_multipath";
  private static final String RDF_TYPE = "rdf:type";
  private static final String REPLACEMENT_PLACEHOLDER = "[replace]";
  private static final Logger LOGGER = LogManager.getLogger(QueryTemplateFactory.class);

  /**
   * Constructs a new query template factory.
   * 
   */
  public QueryTemplateFactory(JsonLdService jsonLdService) {
    this.lifecycleQueryFactory = new LifecycleQueryFactory();
    this.jsonLdService = jsonLdService;
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
   * @param parentField    An optional parent field details if instances must be
   *                       associated.
   * @param lifecycleEvent Optional parameter to dictate the filter for a relevant
   *                       lifecycle event.
   */
  public Queue<String> genGetTemplate(Queue<Queue<SparqlBinding>> bindings, String filterId, ParentField parentField,
      LifecycleEventType lifecycleEvent) {
    // Reset from previous iterations
    this.queryLines = new HashMap<>();
    this.varSequence = new HashMap<>();
    this.sortedVars = new ArrayList<>();
    this.variables = new HashSet<>();
    // Code starts after reset and validation
    LOGGER.info("Generating a query template for getting data...");
    StringBuilder selectVariableBuilder = new StringBuilder();
    StringBuilder whereBuilder = new StringBuilder();
    // Extract the first binding class but it should not be removed from the queue
    String targetClass = bindings.peek().peek().getFieldValue(CLAZZ_VAR);
    this.sortBindings(bindings);

    // Retrieve only the property fields if no sequence of variable is present
    if (this.varSequence.isEmpty()) {
      selectVariableBuilder.append(ShaclResource.VARIABLE_MARK).append(LifecycleResource.IRI_KEY);
      this.variables.stream().forEach(variable -> selectVariableBuilder.append(ShaclResource.WHITE_SPACE)
          .append(variable));
      // Append lifecycle events if available
      if (lifecycleEvent != null) {
        List.of(LifecycleResource.STATUS_KEY, LifecycleResource.SCHEDULE_START_DATE_KEY,
            LifecycleResource.SCHEDULE_END_DATE_KEY, LifecycleResource.SCHEDULE_START_TIME_KEY,
            LifecycleResource.SCHEDULE_END_TIME_KEY, LifecycleResource.SCHEDULE_TYPE_KEY)
            .stream().forEach(lifecycleVar -> selectVariableBuilder.append(ShaclResource.WHITE_SPACE)
                .append(ShaclResource.VARIABLE_MARK)
                .append(StringResource.parseQueryVariable(lifecycleVar)));
      }
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
    this.appendOptionalIdFilters(whereBuilder, filterId, parentField);
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
    this.queryLines = new HashMap<>();
    this.varSequence = new HashMap<>();
    this.sortedVars = new ArrayList<>();
    this.variables = new HashSet<>();
    // Code starts after reset
    LOGGER.info("Generating a query template for getting the data that matches the search criteria...");
    StringBuilder whereBuilder = new StringBuilder();
    StringBuilder filters = new StringBuilder();
    // Extract the first binding class but it should not be removed from the queue
    String targetClass = bindings.peek().peek().getFieldValue(CLAZZ_VAR);
    this.sortBindings(bindings);

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
    this.deleteBranchBuilders = new ArrayDeque<>();
    this.recursiveParseNode(deleteBuilder, whereBuilder, rootNode, targetId, true);
    return this.genDeleteTemplate(deleteBuilder, whereBuilder, this.deleteBranchBuilders);
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
    // For mixed endpoints with Ontop which does not support property paths
    results.offer(
        "SELECT DISTINCT " + selectVariables + " WHERE {?iri a " + iriClass + ShaclResource.FULL_STOP
            + whereClause.replace(REPLACEMENT_PLACEHOLDER, "")
            + "}");
    // For SPARQL endpoints
    results.offer(
        "SELECT DISTINCT " + selectVariables + " WHERE {?iri a/rdfs:subClassOf* " + iriClass + ShaclResource.FULL_STOP
            + whereClause.replace(REPLACEMENT_PLACEHOLDER, "/rdfs:subClassOf*") + "}");
    return results;
  }

  /**
   * Sort and categorise the bindings into the default and optional queues for
   * processing.
   * 
   * @param nestedBindings The bindings queried from SHACL restrictions that
   *                       should be queried in template.
   */
  private void sortBindings(Queue<Queue<SparqlBinding>> nestedBindings) {
    Map<String, SparqlQueryLine> queryLineMappings = new HashMap<>();
    Map<String, SparqlQueryLine> groupQueryLineMappings = new HashMap<>();
    while (!nestedBindings.isEmpty()) {
      Queue<SparqlBinding> bindings = nestedBindings.poll();
      Queue<String> nodeGroups = new ArrayDeque<>();
      while (!bindings.isEmpty()) {
        SparqlBinding binding = bindings.poll();
        String multiPartPredicate = this.getPredicate(binding, MULTIPATH_VAR);
        String multiPartLabelPredicate = this.getPredicate(binding, MULTI_NAME_PATH_VAR);
        this.genQueryLine(binding, multiPartPredicate, multiPartLabelPredicate, nodeGroups, queryLineMappings);
      }
      // If there are any node group, these should be removed from the mappings
      // and place into a dedicated group mapping
      while (!nodeGroups.isEmpty()) {
        String nodeGroup = nodeGroups.poll();
        if (!groupQueryLineMappings.containsKey(nodeGroup)) {
          groupQueryLineMappings.put(nodeGroup, queryLineMappings.get(nodeGroup));
          queryLineMappings.remove(nodeGroup);
          this.varSequence.remove(nodeGroup);
        }
      }
    }
    this.parseQueryLines(queryLineMappings, groupQueryLineMappings);
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
   * @param nodeGroups              Stores any node group property.
   * @param queryLineMappings       The target mappings storing the generated
   *                                query lines.
   */
  private void genQueryLine(SparqlBinding binding, String multiPartPredicate, String multiPartLabelPredicate,
      Queue<String> nodeGroups, Map<String, SparqlQueryLine> queryLineMappings) {
    String shNodeGroupName = binding.getFieldValue(NODE_GROUP_VAR);
    String propertyName = binding.getFieldValue(NAME_VAR);
    // Any nested id properties in sh:node should be ignored
    if (shNodeGroupName != null && propertyName.equals("id")) {
      return;
    }

    boolean isClassVar = Boolean.parseBoolean(binding.getFieldValue(IS_CLASS_VAR));
    String instanceClass = binding.getFieldValue(INSTANCE_CLASS_VAR);
    String branchName = binding.getFieldValue(BRANCH_VAR);
    String mappingKey = ShaclResource.getMappingKey(propertyName, branchName);
    // For existing mappings,
    if (queryLineMappings.containsKey(mappingKey)) {
      SparqlQueryLine currentQueryLine = queryLineMappings.get(mappingKey);
      // Update the mapping with the extended predicates
      queryLineMappings.put(mappingKey,
          new SparqlQueryLine(propertyName, instanceClass, currentQueryLine.nestedClass(), currentQueryLine.subject(),
              parsePredicate(currentQueryLine.predicate(), multiPartPredicate),
              parsePredicate(currentQueryLine.labelPredicate(), multiPartLabelPredicate),
              currentQueryLine.subjectFilter(), currentQueryLine.branch(), currentQueryLine.isOptional(), isClassVar));
    } else {
      // When initialising a new query line
      String subjectVar = binding.containsField(SUBJECT_VAR) ? binding.getFieldValue(SUBJECT_VAR) : "";
      String nestedClass = binding.containsField(NESTED_CLASS_VAR) ? binding.getFieldValue(NESTED_CLASS_VAR) : "";
      boolean isOptional = Boolean.parseBoolean(binding.getFieldValue(IS_OPTIONAL_VAR));
      // Parse ordering only for label query, as we require the heading order in csv
      // Order field will not exist for non-label query
      if (binding.containsField(ORDER_VAR)) {
        int order = Integer.parseInt(binding.getFieldValue(ORDER_VAR));
        List<Integer> orders = new ArrayList<>();
        orders.add(order);
        this.varSequence.put(propertyName, orders);
      }
      String fieldSubject = LifecycleResource.IRI_KEY;
      if (shNodeGroupName != null) {
        fieldSubject = shNodeGroupName;
        // Append branch name for groups as well if required
        nodeGroups.offer(ShaclResource.getMappingKey(shNodeGroupName, branchName));
      }
      queryLineMappings.put(mappingKey,
          new SparqlQueryLine(propertyName, instanceClass, nestedClass, fieldSubject, multiPartPredicate,
              multiPartLabelPredicate, subjectVar, branchName, isOptional, isClassVar));
    }
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
   * @param queryLineMappings      The input unparsed query lines
   * @param groupQueryLineMappings The unparsed query lines for node groups.
   */
  private void parseQueryLines(Map<String, SparqlQueryLine> queryLineMappings,
      Map<String, SparqlQueryLine> groupQueryLineMappings) {
    Map<String, String> branchQueryLines = new HashMap<>();
    Map<String, String> groupQueryLines = new HashMap<>();
    groupQueryLineMappings.entrySet().stream().forEach(entry -> {
      StringBuilder currentLine = new StringBuilder();
      StringResource.appendTriple(currentLine,
          ShaclResource.VARIABLE_MARK + StringResource.parseQueryVariable(entry.getValue().subject()),
          entry.getValue().predicate(),
          ShaclResource.VARIABLE_MARK + StringResource.parseQueryVariable(entry.getValue().property()));
      // If there is a sh:node targetClass property available, append the class
      // restriction
      if (!entry.getValue().nestedClass().isEmpty()) {
        StringResource.appendTriple(currentLine,
            ShaclResource.VARIABLE_MARK + StringResource.parseQueryVariable(entry.getValue().property()),
            RDF_TYPE + REPLACEMENT_PLACEHOLDER,
            StringResource.parseIriForQuery(entry.getValue().nestedClass()));
      }
      groupQueryLines.put(entry.getKey(), currentLine.toString());
    });
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
        StringResource.appendTriple(currentLine,
            ShaclResource.VARIABLE_MARK + StringResource.parseQueryVariable(queryLine.subject()), jointPredicate,
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
      String lineOutput;
      // Optional lines should be parsed differently
      if (queryLine.isOptional()) {
        // If the value must conform to a specific subject variable,
        // a filter needs to be added directly to the same optional clause
        if (!queryLine.subjectFilter().isEmpty()) {
          Map<String, String> searchCriteria = new HashMap<>();
          searchCriteria.put(queryLine.property(), queryLine.subjectFilter());
          currentLine.append(genSearchCriteria(queryLine.property(), searchCriteria));
        }
        lineOutput = StringResource.genOptionalClause(currentLine.toString());
      } else {
        // Non-optional lines does not require special effects
        lineOutput = currentLine.toString();
      }

      // Branches should be added as a separate bunch
      if (queryLine.branch() != null) {
        branchQueryLines.compute(queryLine.branch(), (k, previousLine) -> {
          // Append previous line only if there are previous lines, else, it will be an
          // empty string added
          String prevLine = previousLine == null ? "" : previousLine;
          // For non-iri subjects, append the associated group line once
          if (!queryLine.subject().equals(LifecycleResource.IRI_KEY)) {
            String mappingKey = ShaclResource.getMappingKey(queryLine.subject(), queryLine.branch());
            String groupedOutput = groupQueryLines.getOrDefault(mappingKey, "") + lineOutput;
            groupQueryLines.remove(mappingKey); // Remove to prevent side effects
            return prevLine + groupedOutput;
          }
          return prevLine + lineOutput;
        });
      } else {
        // Non-branch query lines will be added directly
        this.queryLines.put(queryLine.property(), lineOutput);
      }
      // Always append current property for both branches and individual fields
      // Grouped fields are not part of the query line parsing
      this.variables.add(ShaclResource.VARIABLE_MARK + StringResource.parseQueryVariable(queryLine.property()));
    });
    // If there are non-branch group query lines, add them to the query lines
    if (!groupQueryLines.isEmpty()) {
      this.queryLines.putAll(groupQueryLines);
    }
    // Add branch query block if there are any branches
    if (!branchQueryLines.isEmpty()) {
      StringBuilder branchBlock = new StringBuilder();
      branchQueryLines.entrySet().stream().forEach(branch -> {
        // Add a UNION if there is a previous branch
        if (!branchBlock.isEmpty()) {
          branchBlock.append(" UNION ");
        }
        // If there is only one branch, it should be an optional clause instead
        if (branchQueryLines.size() == 1) {
          branchBlock.append(StringResource.genOptionalClause(branch.getValue()));
        } else {
          branchBlock.append("{").append(branch.getValue()).append("}");
        }
      });
      this.queryLines.put(ShaclResource.BRANCH_KEY, branchBlock.toString());
    }
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
   * @param query       Builder for the query template.
   * @param filterId    An optional field to target the query at a specific
   *                    instance.
   * @param parentField An optional parent field to target the query with specific
   *                    parents.
   */
  private void appendOptionalIdFilters(StringBuilder query, String filterId, ParentField parentField) {
    // Add filter clause for a parent field instead if available
    if (parentField != null) {
      query.append("FILTER STRENDS(STR(?")
          .append(StringResource.parseQueryVariable(parentField.name()))
          .append("), \"")
          .append(parentField.id())
          .append("\")");
    } else if (!filterId.isEmpty()) {
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
      query.append(this.lifecycleQueryFactory.getReadableScheduleQuery());
      switch (lifecycleEvent) {
        case LifecycleEventType.APPROVED:
          this.lifecycleQueryFactory.appendFilterExists(query, false, LifecycleResource.EVENT_APPROVAL);
          break;
        case LifecycleEventType.SERVICE_EXECUTION:
          this.lifecycleQueryFactory.appendFilterExists(query, true, LifecycleResource.EVENT_APPROVAL);
          this.lifecycleQueryFactory.appendArchivedFilterExists(query, false);
          break;
        case LifecycleEventType.ARCHIVE_COMPLETION:
          this.lifecycleQueryFactory.appendArchivedStateQuery(query);
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
   * Recursively parses the node to generate the DELETE query contents.
   * 
   * @param deleteBuilder A query builder for the DELETE clause.
   * @param whereBuilder  A query builder for the WHERE clause.
   * @param currentNode   Input contents to perform operation on.
   * @param targetId      The target instance IRI.
   * @param isIdRequired  Indicator to generate an instance ID or an ID variable
   *                      following targetId.
   */
  private void recursiveParseNode(StringBuilder deleteBuilder, StringBuilder whereBuilder, ObjectNode currentNode,
      String targetId, boolean isIdRequired) {
    // First retrieve the ID value as a subject of the triple if required, else
    // default to target it
    String idTripleSubject = isIdRequired ? this.getFormattedQueryVariable(currentNode.path(ShaclResource.ID_KEY),
        targetId) : targetId;
    Iterator<Map.Entry<String, JsonNode>> iterator = currentNode.fields();
    while (iterator.hasNext()) {
      Map.Entry<String, JsonNode> field = iterator.next();
      JsonNode fieldNode = field.getValue();
      // Create the following query line for all @type fields
      if (field.getKey().equals(ShaclResource.TYPE_KEY)) {
        String typeTripleObject = this.getFormattedQueryVariable(fieldNode, targetId);
        StringResource.appendTriple(deleteBuilder, idTripleSubject, RDF_TYPE, typeTripleObject);
        StringResource.appendTriple(whereBuilder, idTripleSubject, RDF_TYPE, typeTripleObject);
        // For any @branch field
      } else if (field.getKey().equals(ShaclResource.BRANCH_KEY)) {
        // Iterate over all possible branches
        ArrayNode branches = this.jsonLdService.getArrayNode(fieldNode);
        for (JsonNode branch : branches) {
          // Generate the required delete template and store the template
          StringBuilder deleteBranchBuilder = new StringBuilder();
          StringBuilder deleteBranchWhereBuilder = new StringBuilder();
          ObjectNode branchNode = this.jsonLdService.getObjectNode(branch);
          // Retain the current ID value
          branchNode.set(ShaclResource.ID_KEY, currentNode.path(ShaclResource.ID_KEY));
          this.recursiveParseNode(deleteBranchBuilder, deleteBranchWhereBuilder, branchNode, targetId, isIdRequired);
          this.deleteBranchBuilders.offer(deleteBranchBuilder);
          this.deleteBranchBuilders.offer(deleteBranchWhereBuilder);
        }
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
                reversePredicate, deleteBuilder, whereBuilder, targetId, true, isIdRequired);
          }
        }
        // The @id and @context field should be ignored but continue parsing for
        // everything else
      } else if (!field.getKey().equals(ShaclResource.ID_KEY) && !field.getKey().equals(ShaclResource.CONTEXT_KEY)) {
        this.parseFieldNode(currentNode.path(ShaclResource.ID_KEY), fieldNode, idTripleSubject, field.getKey(),
            deleteBuilder, whereBuilder, targetId, isIdRequired);
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
      if (replacementType.equals(LifecycleResource.IRI_KEY) && replacementId.equals("id")) {
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
   * @param isIdRequired  Indicator to generate an instance ID or an ID variable
   *                      following targetId.
   */
  private void parseFieldNode(JsonNode idNode, JsonNode fieldNode, String subject, String predicate,
      StringBuilder deleteBuilder, StringBuilder whereBuilder, String targetId, boolean isIdRequired) {
    // For object field node
    if (fieldNode.isObject()) {
      JsonNode targetTripleObjectNode = fieldNode.has(ShaclResource.REPLACE_KEY)
          ? fieldNode
          : fieldNode.path(ShaclResource.ID_KEY);
      String formattedPredicate = StringResource.parseIriForQuery(predicate);
      String formattedObjVar = this.getFormattedQueryVariable(targetTripleObjectNode, targetId);
      String parsedId = targetId;
      // If this is a nested array element, where no id is required but this is an id
      // field, not literal, extend the target id with the predicate
      if (!isIdRequired && formattedObjVar.startsWith("<") && formattedObjVar.endsWith(">")
          && targetId.contains(ShaclResource.VARIABLE_MARK)) {
        parsedId = targetId + StringResource.getLocalName(predicate);
        formattedObjVar = parsedId;
      }
      StringResource.appendTriple(deleteBuilder, subject, formattedPredicate, formattedObjVar);
      if (fieldNode.has(ShaclResource.REPLACE_KEY)
          && fieldNode.path(ShaclResource.TYPE_KEY).asText().equals("literal")) {
        StringBuilder optionalBuilder = new StringBuilder();
        StringResource.appendTriple(optionalBuilder, subject, formattedPredicate, formattedObjVar);
        whereBuilder.append(StringResource.genOptionalClause(optionalBuilder.toString()));
      } else {
        StringResource.appendTriple(whereBuilder, subject, formattedPredicate, formattedObjVar);
      }
      // Further processing for array type
      if (fieldNode.has(ShaclResource.REPLACE_KEY) && fieldNode.path(ShaclResource.TYPE_KEY).asText().equals("array")
          && fieldNode.has(ShaclResource.CONTENTS_KEY)) {
        // This should generate a DELETE query with a variable whenever IDs are detected
        this.recursiveParseNode(deleteBuilder, whereBuilder,
            this.jsonLdService.getObjectNode(fieldNode.path(ShaclResource.CONTENTS_KEY)),
            formattedObjVar, false);
      }
      // No further processing required for objects intended for replacement, @value,
      if (!fieldNode.has(ShaclResource.REPLACE_KEY) && !fieldNode.has(ShaclResource.VAL_KEY) &&
      // or a one line instance link to a TextNode eg: "@id" : "instanceIri"
          !(fieldNode.has(ShaclResource.ID_KEY) && fieldNode.size() == 1
              && fieldNode.path(ShaclResource.ID_KEY).isTextual())) {
        this.recursiveParseNode(deleteBuilder, whereBuilder, (ObjectNode) fieldNode, parsedId, isIdRequired);
      }
      // For arrays,iterate through each object and parse the nested node
    } else if (fieldNode.isArray()) {
      ArrayNode fieldArray = (ArrayNode) fieldNode;
      for (JsonNode tripleObjNode : fieldArray) {
        this.parseNestedNode(idNode, tripleObjNode, predicate, deleteBuilder, whereBuilder, targetId, false,
            isIdRequired);
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
   * @param isIdRequired  Indicator to generate an instance ID or an ID variable
   *                      following targetId.
   */
  private void parseNestedNode(JsonNode idNode, JsonNode objectNode, String predicatePath, StringBuilder deleteBuilder,
      StringBuilder whereBuilder,
      String targetId, boolean isReverse, boolean isIdRequired) {
    if (isReverse) {
      if (objectNode.isObject()) {
        // A reverse node indicates that the replacement object should now be the
        // subject and the Id Node should become the object
        if (objectNode.has(ShaclResource.REPLACE_KEY)) {
          String replacementVar = this.getFormattedQueryVariable(objectNode, targetId);
          this.parseFieldNode(null, idNode, replacementVar, predicatePath,
              deleteBuilder, whereBuilder, targetId, isIdRequired);
        } else {
          // A reverse node indicates that the original object should now be the subject
          // And the Id Node should become the object
          ObjectNode nestedReverseNode = (ObjectNode) objectNode;
          nestedReverseNode.set(predicatePath, idNode);
          this.recursiveParseNode(deleteBuilder, whereBuilder, nestedReverseNode, targetId, isIdRequired);
        }
      } else if (objectNode.isArray()) {
        // For reverse arrays, iterate and recursively parse each object as a reverse
        // node
        ArrayNode objArray = (ArrayNode) objectNode;
        for (JsonNode nestedReverseObjNode : objArray) {
          this.parseNestedNode(idNode, nestedReverseObjNode, predicatePath, deleteBuilder, whereBuilder, targetId, true,
              isIdRequired);
        }
      }
    } else {
      // This aspect is used for parsing arrays without any reversion
      // Creating a new node where the ID node is the parent is sufficient
      ObjectNode nestedNode = this.jsonLdService.genObjectNode();
      nestedNode.set(ShaclResource.ID_KEY, idNode);
      nestedNode.set(predicatePath, objectNode);
      this.recursiveParseNode(deleteBuilder, whereBuilder, nestedNode, targetId, isIdRequired);
    }
  }

  /**
   * Generates a delete template from the delete builder contents.
   * 
   * @param deleteBuilder  A query builder for the DELETE clause.
   * @param whereBuilder   A query builder for the WHERE clause.
   * @param branchBuilders A query builders for any form branches.
   */
  private String genDeleteTemplate(StringBuilder deleteBuilder, StringBuilder whereBuilder,
      Queue<StringBuilder> branchBuilders) {
    while (!branchBuilders.isEmpty()) {
      // Branch builders will have two builders per branch
      // First builder is for the DELETE clause
      deleteBuilder.append(branchBuilders.poll().toString());
      // Second builder is for the WHERE clause, which should also be optional
      whereBuilder.append(StringResource.genOptionalClause(branchBuilders.poll().toString()));
    }
    return "DELETE {" + deleteBuilder.toString() + "} WHERE {" + whereBuilder.toString() + "}";
  }
}
