package com.cmclinnovations.agent.service;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.Queue;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.federated.repository.FedXRepository;
import org.eclipse.rdf4j.federated.repository.FedXRepositoryConnection;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.resultio.sparqljson.SPARQLResultsJSONWriter;
import org.eclipse.rdf4j.query.resultio.text.csv.SPARQLResultsCSVWriter;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.SparqlEndpointType;
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
  private final FileService fileService;

  private static final String DEFAULT_NAMESPACE = "kb";
  private static final String JSON_MEDIA_TYPE = "application/json";
  private static final String LD_JSON_MEDIA_TYPE = "application/ld+json";
  private static final String SPARQL_MEDIA_TYPE = "application/sparql-query";

  public static final String INVALID_SHACL_ERROR_MSG = "Invalid knowledge model! SHACL restrictions have not been defined/instantiated in the knowledge graph.";

  private static final String RDF_LIST_PATH_PREFIX = "/rdf:rest";

  private static final Logger LOGGER = LogManager.getLogger(KGService.class);

  /**
   * Constructs a new service.
   * 
   * @param fileService File service for accessing file resources.
   */
  public KGService(FileService fileService) {
    this.client = RestClient.create();
    this.objectMapper = new ObjectMapper();
    this.formTemplateFactory = new FormTemplateFactory();
    this.queryTemplateFactory = new QueryTemplateFactory(this.objectMapper);
    this.fileService = fileService;
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
   * Executes the query at the default endpoint `kb` to retrieve the
   * original-format results.
   * 
   * @param query the query for execution.
   * 
   * @return the query results.
   */
  public Queue<SparqlBinding> query(String query) {
    return this.query(query, BlazegraphClient.getInstance().getRemoteStoreClient(DEFAULT_NAMESPACE).getQueryEndpoint());
  }

  /**
   * A method that executes a query at the specified endpoint to retrieve the
   * original-format results.
   * 
   * @param query    the query for execution.
   * @param endpoint the endpoint for execution.
   * 
   * @return the query results.
   */
  public Queue<SparqlBinding> query(String query, String endpoint) {
    String results = this.client.post()
        .uri(endpoint)
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
   * A method that executes a federated query across available endpoints to
   * retrieve results in JSONArray.
   * 
   * @param query        the query for execution.
   * @param endpointType the type of endpoint. Options include Mixed, Blazegraph,
   *                     and Ontop.
   * 
   * @return the query results.
   */
  public JSONArray query(String query, SparqlEndpointType endpointType) {
    List<String> endpoints;
    if (endpointType.equals(SparqlEndpointType.MIXED)) {
      endpoints = this.getEndpoints(SparqlEndpointType.BLAZEGRAPH);
      endpoints.addAll(this.getEndpoints(SparqlEndpointType.ONTOP));
    } else {
      endpoints = this.getEndpoints(endpointType);
    }
    try {
      StringWriter stringWriter = new StringWriter();
      FedXRepository repository = FedXFactory.createSparqlFederation(endpoints);
      try (FedXRepositoryConnection conn = repository.getConnection()) {
        TupleQuery tq = conn.prepareTupleQuery(query);
        // Extend execution time as required
        tq.setMaxExecutionTime(600);
        SPARQLResultsJSONWriter jsonWriter = new SPARQLResultsJSONWriter(stringWriter);
        tq.evaluate(jsonWriter);
        JsonNode bindings = this.objectMapper.readValue(stringWriter.toString(), ObjectNode.class).path("results")
            .path("bindings");
        return new JSONArray(bindings.toString());
      } catch (RepositoryException e) {
        LOGGER.error(e);
      }
    } catch (Exception e) {
      LOGGER.error(e);
    }
    return new JSONArray();
  }

  /**
   * Executes the query at the target endpoint to retrieve results in the CSV
   * format.
   * 
   * @param query        the query for execution.
   * @param endpointType the type of endpoint. Options include Mixed, Blazegraph,
   *                     and Ontop.
   * 
   * @return the query results as CSV rows.
   */
  public String[] queryCSV(String query, SparqlEndpointType endpointType) {
    List<String> endpoints;
    if (endpointType.equals(SparqlEndpointType.MIXED)) {
      endpoints = this.getEndpoints(SparqlEndpointType.BLAZEGRAPH);
      endpoints.addAll(this.getEndpoints(SparqlEndpointType.ONTOP));
    } else {
      endpoints = this.getEndpoints(endpointType);
    }
    try {
      StringWriter stringWriter = new StringWriter();
      FedXRepository repository = FedXFactory.createSparqlFederation(endpoints);
      try (FedXRepositoryConnection conn = repository.getConnection()) {
        TupleQuery tq = conn.prepareTupleQuery(query);
        // Extend execution time as required
        tq.setMaxExecutionTime(600);
        SPARQLResultsCSVWriter csvWriter = new SPARQLResultsCSVWriter(stringWriter);
        tq.evaluate(csvWriter);
        // Results are in csv in one string
        String csvData = stringWriter.toString();
        // Split into rows
        return csvData.split("\\r?\\n");
      } catch (RepositoryException e) {
        LOGGER.error(e);
      }
    } catch (Exception e) {
      LOGGER.error(e);
    }
    return new String[0];
  }

  /**
   * Executes the query at the target endpoint to retrieve JSON LD results.
   * 
   * @param query the query for execution.
   * 
   * @return the query results as JSON array.
   */
  public ArrayNode queryJsonLd(String query, String endpoint) {
    String results = this.client.post()
        // JSON LD queries are used only for generating the form template, and thus,
        // will always be executed on the blazegraph namespace (storing the SHACL
        // restrictions)
        .uri(endpoint)
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
    // SHACL restrictions for generating the form template always stored on a
    // blazegraph namespace
    List<String> endpoints = this.getEndpoints(SparqlEndpointType.BLAZEGRAPH);
    for (String endpoint : endpoints) {
      LOGGER.debug("Querying at the endpoint {}...", endpoint);
      // Execute the query on the current endpoint and get the result
      ArrayNode results = this.queryJsonLd(query, endpoint);
      if (!results.isEmpty()) {
        LOGGER.debug("Query is successfully executed. Parsing the results...");
        return this.formTemplateFactory.genTemplate(results, defaultVals);
      }
    }
    return new HashMap<>();
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
    // Initialise a new queue to store all variables
    // And add the first level right away
    Queue<Queue<SparqlBinding>> nestedVariablesAndPropertyPaths = this.queryNestedPredicates(shaclPathQuery);
    LOGGER.debug("Generating the query template from the predicate paths and variables queried...");

    Queue<String> queries = this.queryTemplateFactory.genGetTemplate(nestedVariablesAndPropertyPaths, targetId,
        hasParent);
    LOGGER.debug("Querying the knowledge graph for the instances...");
    List<SparqlVariableOrder> varSequence = this.queryTemplateFactory.getSequence();
    // Query for direct instances
    JSONArray instances = this.query(queries.poll(), SparqlEndpointType.MIXED);
    // Query for secondary instances ie instances that are subclasses of parent
    JSONArray secondaryInstances = this.query(queries.poll(), SparqlEndpointType.BLAZEGRAPH);
    for (int i = 0; i < secondaryInstances.length(); i++) {
      instances.put(secondaryInstances.getJSONObject(i));
    }

    Queue<SparqlBinding> parsedInstances = new ArrayDeque<>();
    for (int i = 0; i < instances.length(); i++) {
      JSONObject currentInstance = instances.getJSONObject(i); // Get each JSONObject
      SparqlBinding currentInstanceRow = new SparqlBinding(currentInstance);
      currentInstanceRow.addSequence(varSequence);
      parsedInstances.offer(currentInstanceRow);
    }
    return parsedInstances;
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
    Queue<String> queries = this.queryTemplateFactory.genGetTemplate(nestedVariablesAndPropertyPaths, null, false);
    LOGGER.debug("Querying the knowledge graph for the instances in csv format...");
    // Query for direct instances
    String[] resultRows = this.queryCSV(queries.poll(), SparqlEndpointType.MIXED);
    // Query for secondary instances ie instances that are subclasses of parent
    String[] secondaryResultRows = this.queryCSV(queries.poll(), SparqlEndpointType.BLAZEGRAPH);
    StringBuilder results = new StringBuilder();
    // First row will always be column names and should be appended
    for (String row : resultRows) {
      results.append(row).append(System.getProperty("line.separator"));
    }
    // Ignore first row of secondary results as these are duplicate column names
    if (secondaryResultRows.length > 1) {
      for (int i = 1; i < secondaryResultRows.length; i++) {
        results.append(secondaryResultRows[i]).append(System.getProperty("line.separator"));
      }
    }
    return results.toString();
  }

  /**
   * Queries for all instances that matches the search criteria.
   * 
   * @param shaclPathQuery The query to retrieve the required predicate paths in
   *                       the SHACL restrictions.
   * @param criterias      All the available search criteria inputs.
   */
  public JSONArray queryInstancesWithCriteria(String shaclPathQuery, Map<String, String> criterias) {
    LOGGER.debug("Querying the knowledge graph for predicate paths and variables...");
    Queue<Queue<SparqlBinding>> nestedVariablesAndPropertyPaths = this.queryNestedPredicates(shaclPathQuery);
    if (nestedVariablesAndPropertyPaths.isEmpty()) {
      LOGGER.error(INVALID_SHACL_ERROR_MSG);
      throw new IllegalStateException(INVALID_SHACL_ERROR_MSG);
    }
    LOGGER.debug("Generating the query template from the predicate paths and variables queried...");
    Queue<String> searchQuery = this.queryTemplateFactory.genSearchTemplate(nestedVariablesAndPropertyPaths, criterias);
    LOGGER.debug("Querying the knowledge graph for the matching instances...");
    // Query for direct instances
    JSONArray instances = this.query(searchQuery.poll(), SparqlEndpointType.MIXED);
    // Query for secondary instances ie instances that are subclasses of parent
    JSONArray secondaryInstances = this.query(searchQuery.poll(), SparqlEndpointType.BLAZEGRAPH);
    for (int i = 0; i < secondaryInstances.length(); i++) {
      instances.put(secondaryInstances.getJSONObject(i));
    }
    return instances;
  }

  /**
   * Gets all available internal SPARQL endpoints within the stack of the
   * specified type.
   * 
   * @param endpointType The required endpoint type. Can be either mixed,
   *                     blazegraph, or ontop.
   */
  private List<String> getEndpoints(SparqlEndpointType endpointType) {
    LOGGER.debug("Retrieving available endpoints...");
    String serviceClass = "";
    if (endpointType.equals(SparqlEndpointType.BLAZEGRAPH)) {
      serviceClass = "ser:Blazegraph";
    } else if (endpointType.equals(SparqlEndpointType.ONTOP)) {
      serviceClass = "ser:Ontop";
    }
    String query = this.fileService.getContentsWithReplacement(FileService.ENDPOINT_QUERY_RESOURCE, serviceClass);
    Queue<SparqlBinding> results = this.query(query);
    return results.stream()
        .map(binding -> binding.getFieldValue("endpoint"))
        .collect(Collectors.toList());
  }

  /**
   * Executes the update query at the target endpoint.
   * 
   * @param query the query for execution.
   * 
   * @return the status code.
   */
  private int executeUpdate(String query) {
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
    List<String> endpoints = this.getEndpoints(SparqlEndpointType.BLAZEGRAPH);
    LOGGER.debug("Querying the knowledge graph for predicate paths and variables...");
    String firstExecutableQuery = shaclPathQuery.replace(FileService.REPLACEMENT_PATH, "");
    // SHACL restrictions are only found within one endpoint, and can be queried
    // without using FedX
    Queue<Queue<SparqlBinding>> results = new ArrayDeque<>();
    endpoints.forEach(endpoint -> {
      // Stop all iteration once there are already results
      if (!results.isEmpty()) {
        return;
      }
      LOGGER.debug("Querying at the endpoint {}...", endpoint);
      LOGGER.debug("Executing for the first level of predicate paths and variables...");
      Queue<SparqlBinding> variablesAndPropertyPaths = this.query(firstExecutableQuery, endpoint);
      // Break this iteration if no restrictions are found
      if (variablesAndPropertyPaths.isEmpty()) {
        LOGGER.debug("No relevant SHACL restrictions found...");
        return;
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
        variablesAndPropertyPaths = this.query(executableQuery, endpoint);
        nestedVariablesAndPropertyPaths.offer(variablesAndPropertyPaths);
        // Extend the replacement path with an /rdf:rest prefix for the next level
        replacementPath = RDF_LIST_PATH_PREFIX + replacementPath;
      }
      results.addAll(nestedVariablesAndPropertyPaths);
      return;
    });
    if (results.isEmpty()) {
      LOGGER.error(INVALID_SHACL_ERROR_MSG);
      throw new IllegalStateException(INVALID_SHACL_ERROR_MSG);
    }
    return results;
  }
}