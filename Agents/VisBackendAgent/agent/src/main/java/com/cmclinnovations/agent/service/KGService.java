package com.cmclinnovations.agent.service;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Map;
import java.util.List;
import java.util.Queue;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.SparqlVariableOrder;
import com.cmclinnovations.agent.template.FormTemplateFactory;
import com.cmclinnovations.agent.template.QueryTemplateFactory;
import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@Service
public class KGService {
  @Value("${NAMESPACE}")
  String namespace;

  private final RestClient client;
  private final ObjectMapper objectMapper;
  private final FormTemplateFactory formTemplateFactory;
  private final QueryTemplateFactory queryTemplateFactory;

  private static final String JSON_MEDIA_TYPE = "application/json";
  private static final String LD_JSON_MEDIA_TYPE = "application/ld+json";
  private static final String SPARQL_MEDIA_TYPE = "application/sparql-query";
  private static final String CSV_MEDIA_TYPE = "text/csv";

  public static final String INVALID_SHACL_ERROR_MSG = "Invalid knowledge model! SHACL restrictions have not been defined/instantiated in the knowledge graph.";

  private static final String RDF_LIST_PATH_PREFIX = "/rdf:rest";

  private static final Logger LOGGER = LogManager.getLogger(KGService.class);

  /**
   * Constructs a new service.
   */
  public KGService() {
    this.client = RestClient.create();
    this.objectMapper = new ObjectMapper();
    this.formTemplateFactory = new FormTemplateFactory();
    this.queryTemplateFactory = new QueryTemplateFactory(this.objectMapper);
  }

  /**
   * Add the target triples in JSON-LD format into the KG.
   * 
   * @param contents the contents to add
   */
  public ResponseEntity<String> add(String contents) {
    return this.client.post()
        .uri(BlazegraphClient.getInstance().getRemoteStoreClient(this.namespace).getQueryEndpoint())
        .accept(MediaType.valueOf(LD_JSON_MEDIA_TYPE))
        .contentType(MediaType.valueOf(LD_JSON_MEDIA_TYPE))
        .body(contents)
        .retrieve()
        .toEntity(String.class);
  }

  /**
   * Executes the query at the target endpoint to retrieve the original-format
   * results.
   * 
   * @param query the query for execution.
   * 
   * @return the query results.
   */
  public Queue<SparqlBinding> query(String query) {
    String results = this.client.post()
        .uri(BlazegraphClient.getInstance().getRemoteStoreClient(this.namespace).getQueryEndpoint())
        .accept(MediaType.valueOf(JSON_MEDIA_TYPE))
        .contentType(MediaType.valueOf(SPARQL_MEDIA_TYPE))
        .body(query)
        .retrieve()
        .body(String.class);
    // Returns an array
    JsonNode jsonResults;
    try {
      jsonResults = this.objectMapper.readValue(results, ObjectNode.class).path("results").path("bindings");
    } catch (JsonProcessingException e) {
      LOGGER.error(e);
      throw new IllegalArgumentException(e);
    }
    if (jsonResults.isArray()) {
      return this.parseResults((ArrayNode) jsonResults);
    }
    return new ArrayDeque<>();
  }

  /**
   * Executes the query at the target endpoint to retrieve results in the CSV
   * format.
   * 
   * @param query the query for execution.
   * 
   * @return the query results in the CSV format.
   */
  public String queryCSV(String query) {
    return this.client.post()
        .uri(BlazegraphClient.getInstance().getRemoteStoreClient(this.namespace).getQueryEndpoint())
        .accept(MediaType.valueOf(CSV_MEDIA_TYPE))
        .contentType(MediaType.valueOf(SPARQL_MEDIA_TYPE))
        .body(query)
        .retrieve()
        .body(String.class);
  }

  /**
   * Executes the query at the target endpoint to retrieve JSON LD results.
   * 
   * @param query the query for execution.
   * 
   * @return the query results as JSON array.
   */
  public ArrayNode queryJsonLd(String query) {
    String results = this.client.post()
        .uri(BlazegraphClient.getInstance().getRemoteStoreClient(this.namespace).getQueryEndpoint())
        .accept(MediaType.valueOf(LD_JSON_MEDIA_TYPE))
        .contentType(MediaType.valueOf(SPARQL_MEDIA_TYPE))
        .body(query)
        .retrieve()
        .body(String.class);
    ArrayNode parsedResults = null;
    try {
      parsedResults = this.objectMapper.readValue(results, ArrayNode.class);
    } catch (JsonProcessingException e) {
      LOGGER.error(e);
      throw new IllegalArgumentException(e);
    }
    return parsedResults;
  }

  /**
   * Queries for the form template.
   * 
   * @param query the query for execution.
   * @return the form template as a JSON object.
   */
  public Map<String, Object> queryForm(String query, Map<String, Object> defaultVals) {
    LOGGER.info("Querying the knowledge graph for the form template...");
    ArrayNode results = this.queryJsonLd(query);
    LOGGER.debug("Query is successfully executed. Parsing the results...");
    return this.formTemplateFactory.genTemplate(results, defaultVals);
  }

  /**
   * Queries for either all instances or a specific instance based on the id.
   * 
   * @param shaclPathQuery The query to retrieve the required predicate paths in
   *                       the SHACL restrictions.
   * @param targetId       An optional field to target the query at a specific
   *                       instance.
   * @param hasParent      Indicates if the query needs to filter out parent
   *                       entities.
   */
  public Queue<SparqlBinding> queryInstances(String shaclPathQuery, String targetId, boolean hasParent) {
    // Initialise a new queue to store all variables and add the first level right
    // away
    Queue<Queue<SparqlBinding>> nestedVariablesAndPropertyPaths = this.queryNestedPredicates(shaclPathQuery);

    LOGGER.debug("Generating the query template from the predicate paths and variables queried...");
    String instanceQuery = this.queryTemplateFactory.genGetTemplate(nestedVariablesAndPropertyPaths, targetId,
        hasParent);
    List<SparqlVariableOrder> varSequence = this.queryTemplateFactory.getSequence();
    LOGGER.debug("Querying the knowledge graph for the instances...");
    Queue<SparqlBinding> instances = this.query(instanceQuery);
    // If there is a variable sequence available, add the sequence to each binding,
    if (!varSequence.isEmpty()) {
      instances.forEach(instance -> instance.addSequence(varSequence));
    }
    return instances;
  }

  /**
   * Queries for all instances in the csv format.
   * 
   * @param shaclPathQuery The query to retrieve the required predicate paths in
   *                       the SHACL restrictions.
   */
  public String queryInstancesInCsv(String shaclPathQuery) {
    LOGGER.debug("Querying the knowledge graph for predicate paths and variables...");
    Queue<Queue<SparqlBinding>> nestedVariablesAndPropertyPaths = this.queryNestedPredicates(shaclPathQuery);
    if (nestedVariablesAndPropertyPaths.isEmpty()) {
      LOGGER.error(INVALID_SHACL_ERROR_MSG);
      throw new IllegalStateException(INVALID_SHACL_ERROR_MSG);
    }
    LOGGER.debug("Generating the query template from the predicate paths and variables queried...");
    String instanceQuery = this.queryTemplateFactory.genGetTemplate(nestedVariablesAndPropertyPaths, null, false);
    LOGGER.debug("Querying the knowledge graph for the instances in csv format...");
    return this.queryCSV(instanceQuery);
  }

  /**
   * Queries for all instances that matches the search criteria.
   * 
   * @param shaclPathQuery The query to retrieve the required predicate paths in
   *                       the SHACL restrictions.
   * @param criterias      All the available search criteria inputs.
   */
  public Queue<SparqlBinding> queryInstancesWithCriteria(String shaclPathQuery, Map<String, String> criterias) {
    LOGGER.debug("Querying the knowledge graph for predicate paths and variables...");
    Queue<Queue<SparqlBinding>> nestedVariablesAndPropertyPaths = this.queryNestedPredicates(shaclPathQuery);
    if (nestedVariablesAndPropertyPaths.isEmpty()) {
      LOGGER.error(INVALID_SHACL_ERROR_MSG);
      throw new IllegalStateException(INVALID_SHACL_ERROR_MSG);
    }
    LOGGER.debug("Generating the query template from the predicate paths and variables queried...");
    String searchQuery = this.queryTemplateFactory.genSearchTemplate(nestedVariablesAndPropertyPaths, criterias);
    LOGGER.debug("Querying the knowledge graph for the matching instances...");
    return this.query(searchQuery);
  }

  /**
   * Executes the update query at the target endpoint.
   * 
   * @param query the query for execution.
   * 
   * @return the status code.
   */
  public int executeUpdate(String query) {
    RemoteStoreClient kgClient = BlazegraphClient.getInstance().getRemoteStoreClient(this.namespace);
    // Execute the request
    try (CloseableHttpResponse response = kgClient.executeUpdateByPost(query)) {
      return response.getStatusLine().getStatusCode();
    } catch (IOException e) {
      LOGGER.error(e);
    }
    return 500;
  }

  /**
   * Deletes the target instance and its associated properties from the KG.
   * 
   * @param addJsonSchema The JSON schema for adding a new instance
   * @param targetId      The target instance IRI.
   */
  public ResponseEntity<String> delete(ObjectNode addJsonSchema, String targetId) {
    // Parse the JSON schema into the corresponding delete query
    String query = this.queryTemplateFactory.genDeleteQueryTemplate(addJsonSchema, targetId);
    LOGGER.debug("Deleting instances...");

    int statusCode = this.executeUpdate(query);
    if (statusCode == 200) {
      LOGGER.info("Instance has been successfully deleted!");
      return new ResponseEntity<>(
          "Instance has been successfully deleted!",
          HttpStatus.OK);
    } else {
      return new ResponseEntity<>(
          "Error deleting instances from the KG. Please read the logs for more information!",
          HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  /**
   * Parses the results into the required data model.
   * 
   * @param results Results retrieved from the knowledge graph.
   */
  private Queue<SparqlBinding> parseResults(ArrayNode results) {
    LOGGER.debug("Parsing the results...");
    return StreamSupport.stream(results.spliterator(), false)
        .filter(JsonNode::isObject) // Ensure they are object node so that we can type cast
        .map(row -> new SparqlBinding((ObjectNode) row))
        .collect(Collectors.toCollection(ArrayDeque::new));
  }

  /**
   * Queries for the nested predicates as a queue of responses based on their
   * current nested level.
   * 
   * @param shaclPathQuery The query to retrieve the required predicate paths in
   *                       the SHACL restrictions.
   */
  private Queue<Queue<SparqlBinding>> queryNestedPredicates(String shaclPathQuery) {
    LOGGER.debug("Querying the knowledge graph for predicate paths and variables...");
    String firstExecutableQuery = shaclPathQuery.replace(FileService.REPLACEMENT_PATH, "");

    LOGGER.debug("Executing for the first level of predicate paths and variables...");
    Queue<SparqlBinding> variablesAndPropertyPaths = this.query(firstExecutableQuery);
    if (variablesAndPropertyPaths.isEmpty()) {
      LOGGER.error(INVALID_SHACL_ERROR_MSG);
      throw new IllegalStateException(INVALID_SHACL_ERROR_MSG);
    }
    // Initialise a new queue to store all variables and add the first level right
    // away
    Queue<Queue<SparqlBinding>> nestedVariablesAndPropertyPaths = new ArrayDeque<>();
    nestedVariablesAndPropertyPaths.offer(variablesAndPropertyPaths);

    LOGGER.debug("Executing for level {} of predicate paths and variables...",
        nestedVariablesAndPropertyPaths.size() + 1);
    String replacementPath = RDF_LIST_PATH_PREFIX + "/rdf:first";
    // Iterating for the next level of predicates if current level exists
    while (!variablesAndPropertyPaths.isEmpty()) {
      // First replace the [path] in the original query with the current replacement
      // path
      String executableQuery = shaclPathQuery.replace(FileService.REPLACEMENT_PATH, replacementPath);
      // Execute and store the current level of predicate paths and variables
      variablesAndPropertyPaths = this.query(executableQuery);
      nestedVariablesAndPropertyPaths.offer(variablesAndPropertyPaths);
      // Extend the replacement path with an /rdf:rest prefix for the next level
      replacementPath = RDF_LIST_PATH_PREFIX + replacementPath;
    }
    return nestedVariablesAndPropertyPaths;
  }
}