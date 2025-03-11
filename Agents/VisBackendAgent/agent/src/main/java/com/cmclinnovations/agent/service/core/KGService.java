package com.cmclinnovations.agent.service.core;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;

import com.cmclinnovations.agent.model.ParentField;
import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.type.LifecycleEventType;
import com.cmclinnovations.agent.model.type.SparqlEndpointType;
import com.cmclinnovations.agent.template.FormTemplateFactory;
import com.cmclinnovations.agent.template.QueryTemplateFactory;
import com.cmclinnovations.agent.utils.LifecycleResource;
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
  private static final String SUB_SHAPE_PATH = "sh:node/sh:property";

  private static final Logger LOGGER = LogManager.getLogger(KGService.class);

  /**
   * Constructs a new service.
   * 
   * @param fileService File service for accessing file resources.
   */
  public KGService(FileService fileService, JsonLdService jsonLdService) {
    this.client = RestClient.create();
    this.objectMapper = new ObjectMapper();
    this.formTemplateFactory = new FormTemplateFactory();
    this.queryTemplateFactory = new QueryTemplateFactory(jsonLdService);
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
    return this.query(query,
        BlazegraphClient.getInstance().getRemoteStoreClient(DEFAULT_NAMESPACE).getQueryEndpoint());
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
  public Queue<SparqlBinding> query(String query, SparqlEndpointType endpointType) {
    List<String> endpoints = this.getEndpoints(endpointType);
    try {
      StringWriter stringWriter = new StringWriter();
      FedXRepository repository = FedXFactory.createSparqlFederation(endpoints);
      try (FedXRepositoryConnection conn = repository.getConnection()) {
        TupleQuery tq = conn.prepareTupleQuery(query);
        // Extend execution time as required
        tq.setMaxExecutionTime(600);
        SPARQLResultsJSONWriter jsonWriter = new SPARQLResultsJSONWriter(stringWriter);
        tq.evaluate(jsonWriter);
        JsonNode bindings = this.objectMapper.readValue(stringWriter.toString(), ObjectNode.class)
            .path("results")
            .path("bindings");
        if (bindings.isArray()) {
          return this.parseResults((ArrayNode) bindings);
        }
      } catch (RepositoryException e) {
        LOGGER.error(e);
      } catch (JsonProcessingException e) {
        LOGGER.error(e);
        throw new IllegalArgumentException(e);
      }
    } catch (Exception e) {
      LOGGER.error(e);
    }
    return new ArrayDeque<>();
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
    List<String> endpoints = this.getEndpoints(endpointType);
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
   * Queries for all instance(s) associated with the target id.
   * 
   * @param shaclPathQuery The query to retrieve the required predicate paths in
   *                       the SHACL restrictions.
   * @param targetId       Target ID of interest.
   */
  public Queue<SparqlBinding> queryInstances(String shaclPathQuery, String targetId) {
    return this.queryInstances(shaclPathQuery, targetId, null, null);
  }

  /**
   * Queries for all instances that may or may not be associated with a parent
   * field.
   * 
   * @param shaclPathQuery The query to retrieve the required predicate paths in
   *                       the SHACL restrictions.
   * @param parentField    Optional parent field.
   */
  public Queue<SparqlBinding> queryInstances(String shaclPathQuery, ParentField parentField) {
    return this.queryInstances(shaclPathQuery, "", parentField, null);
  }

  /**
   * Queries for all instances associated with a lifecycle event.
   * 
   * @param shaclPathQuery The query to retrieve the required predicate paths in
   *                       the SHACL restrictions.
   * @param lifecycleEvent Optional parameter to dictate the filter for a relevant
   *                       lifecycle event.
   */
  public Queue<SparqlBinding> queryInstances(String shaclPathQuery, LifecycleEventType lifecycleEvent) {
    return this.queryInstances(shaclPathQuery, "", null, lifecycleEvent);
  }

  /**
   * Queries for either all instances or a specific instance based on the id.
   * 
   * @param shaclPathQuery The query to retrieve the required predicate paths in
   *                       the SHACL restrictions.
   * @param targetId       An optional field to target the query at a specific
   *                       instance.
   * @param parentField    Optional parent field.
   * @param lifecycleEvent Optional parameter to dictate the filter for a relevant
   *                       lifecycle event.
   */
  public Queue<SparqlBinding> queryInstances(String shaclPathQuery, String targetId, ParentField parentField,
      LifecycleEventType lifecycleEvent) {
    // Initialise a new queue to store all variables
    // And add the first level right away
    Queue<Queue<SparqlBinding>> nestedVariablesAndPropertyPaths = this.queryNestedPredicates(shaclPathQuery);
    LOGGER.debug("Generating the query template from the predicate paths and variables queried...");
    Queue<String> queries = this.queryTemplateFactory.genGetTemplate(nestedVariablesAndPropertyPaths, targetId,
        parentField, lifecycleEvent);
    LOGGER.debug("Querying the knowledge graph for the instances...");
    List<String> varSequence = this.queryTemplateFactory.getSequence();
    // Query for direct instances
    Queue<SparqlBinding> instances = this.query(queries.poll(), SparqlEndpointType.MIXED);
    // Query for secondary instances ie instances that are subclasses of parent
    Queue<SparqlBinding> secondaryInstances = this.query(queries.poll(), SparqlEndpointType.BLAZEGRAPH);
    instances = this.combineBindingQueue(instances, secondaryInstances);
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
    Queue<String> queries = this.queryTemplateFactory.genGetTemplate(nestedVariablesAndPropertyPaths, null, null,
        null);
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
  public Queue<SparqlBinding> queryInstancesWithCriteria(String shaclPathQuery, Map<String, String> criterias) {
    LOGGER.debug("Querying the knowledge graph for predicate paths and variables...");
    Queue<Queue<SparqlBinding>> nestedVariablesAndPropertyPaths = this.queryNestedPredicates(shaclPathQuery);
    if (nestedVariablesAndPropertyPaths.isEmpty()) {
      LOGGER.error(INVALID_SHACL_ERROR_MSG);
      throw new IllegalStateException(INVALID_SHACL_ERROR_MSG);
    }
    LOGGER.debug("Generating the query template from the predicate paths and variables queried...");
    Queue<String> searchQuery = this.queryTemplateFactory.genSearchTemplate(nestedVariablesAndPropertyPaths,
        criterias);
    LOGGER.debug("Querying the knowledge graph for the matching instances...");
    // Query for direct instances
    Queue<SparqlBinding> instances = this.query(searchQuery.poll(), SparqlEndpointType.MIXED);
    // Query for secondary instances ie instances that are subclasses of parent
    Queue<SparqlBinding> secondaryInstances = this.query(searchQuery.poll(), SparqlEndpointType.BLAZEGRAPH);
    instances.addAll(secondaryInstances);
    return instances;
  }

  /**
   * A method to retrieve the result binding. Note that this method is only
   * applicable for one result sparql binding and will return an error otherwise.
   * 
   * @param results Results to parse.
   * @param field   Field name
   */
  public SparqlBinding getSingleInstance(Queue<SparqlBinding> results) {
    if (results.size() > 1) {
      // When there is more than one results, verify if they can be grouped
      // as results might contain an array of different values for the same instance
      String firstId = results.peek().getFieldValue(LifecycleResource.IRI_KEY);
      boolean isGroup = results.stream().allMatch(binding -> {
        String currentId = binding.getFieldValue(LifecycleResource.IRI_KEY);
        return currentId != null && currentId.equals(firstId);
      });
      if (!isGroup) {
        LOGGER.error("Detected multiple instances: Data model is invalid!");
        throw new IllegalStateException("Detected multiple instances: Data model is invalid!");
      }
      // Removes the first instance from results as the core instance
      SparqlBinding firstInstance = results.poll();
      // Iterate over each result binding to append arrays if required
      results.stream().forEach(firstInstance::addFieldArray);
      return firstInstance;
    }
    if (results.size() == 1) {
      return results.poll();
    }
    if (results.isEmpty()) {
      LOGGER.error("No valid instance found!");
      throw new NullPointerException("No valid instance found!");
    }
    LOGGER.error("Data model is invalid!");
    throw new IllegalStateException("Data model is invalid!");

  }

  /**
   * Gets all available internal SPARQL endpoints within the stack of the
   * specified type.
   * 
   * @param endpointType The required endpoint type. Can be either mixed,
   *                     blazegraph, or ontop.
   * @return List of endpoints of type `endpointType`
   */
  public List<String> getEndpoints(SparqlEndpointType endpointType) {
    LOGGER.debug("Retrieving available endpoints...");
    String query = this.fileService.getContentsWithReplacement(FileService.ENDPOINT_QUERY_RESOURCE,
        endpointType.getIri());
    Queue<SparqlBinding> results = this.query(query);
    return results.stream()
        .map(binding -> binding.getFieldValue("endpoint"))
        .toList();
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
    LOGGER.debug("Querying the knowledge graph for predicate paths and variables...");
    // Retrieve all endpoints once
    List<String> endpoints = this.getEndpoints(SparqlEndpointType.BLAZEGRAPH);
    // Initialise a queue to store all values
    Queue<Queue<SparqlBinding>> results = new ArrayDeque<>();
    String replacementShapePath = ""; // Initial replacement string
    boolean continueLoop = true; // Initialise a continue indicator
    // Iterate to get predicates at the hierarchy of shapes enforced via sh:node
    while (continueLoop) {
      boolean isFirstIteration = true;
      boolean hasResults = false;
      String replacementPath = "";
      Queue<SparqlBinding> variablesAndPropertyPaths = new ArrayDeque<>();
      // Iterate through the depth to retrieve all associated predicate paths for this
      // property in this shape
      while (isFirstIteration || hasResults) {
        hasResults = false; // Reset and verify if there are results for this iteration
        // Replace the [subproperty] and [path] with the respective values
        String executableQuery = shaclPathQuery
            .replace(FileService.REPLACEMENT_SHAPE, replacementShapePath)
            .replace(FileService.REPLACEMENT_PATH, replacementPath);
        // SHACL restrictions are only found within one Blazegraph endpoint, and can be
        // queried without using FedX
        for (String endpoint : endpoints) {
          LOGGER.debug("Querying at the endpoint {}...", endpoint);
          Queue<SparqlBinding> queryResults = this.query(executableQuery, endpoint);
          if (!queryResults.isEmpty()) {
            LOGGER.debug("Found data at the endpoint {}...", endpoint);
            variablesAndPropertyPaths.addAll(queryResults);
            // Indicator should be marked if results are found
            hasResults = true;
          }
        }
        if (hasResults) {
          // Extend replacement path based on the current level
          replacementPath = replacementPath.isEmpty() ? RDF_LIST_PATH_PREFIX + "/rdf:first" // first level
              : RDF_LIST_PATH_PREFIX + replacementPath; // for the second level onwards
        }
        if (isFirstIteration) {
          // If there are results in the first iteration of the retrieval for this shape,
          // iterate to check if there are any nested shapes with predicates
          continueLoop = hasResults;
          isFirstIteration = false;
        }
      }
      if (!variablesAndPropertyPaths.isEmpty()) {
        results.offer(variablesAndPropertyPaths);
      }
      // Extend to get the next level of shape if any
      replacementShapePath = replacementShapePath.isEmpty() ? " ?nestedshape." +
          "?nestedshape sh:name ?" + QueryTemplateFactory.NODE_GROUP_VAR + ";sh:node/sh:targetClass ?"
          + QueryTemplateFactory.NESTED_CLASS_VAR + ";" + SUB_SHAPE_PATH
          : "/" + SUB_SHAPE_PATH + replacementShapePath;
    }
    if (results.isEmpty()) {
      LOGGER.error(INVALID_SHACL_ERROR_MSG);
      throw new IllegalStateException(INVALID_SHACL_ERROR_MSG);
    }
    return results;
  }

  /**
   * Combine two queues containing SparlBinding objects. The method also removes
   * duplicates in the combined queue.
   * 
   * @param firstQueue The first target queue.
   * @param secQueue   The second target queue.
   */
  private Queue<SparqlBinding> combineBindingQueue(Queue<SparqlBinding> firstQueue, Queue<SparqlBinding> secQueue) {
    if (firstQueue.isEmpty() && secQueue.isEmpty()) {
      return new ArrayDeque<>();
    }
    Queue<SparqlBinding> result = new ArrayDeque<>();
    // Group them by the IRI key
    Map<String, List<SparqlBinding>> groupedBindings = Stream.concat(firstQueue.stream(), secQueue.stream())
        .distinct()
        .collect(Collectors.groupingBy(binding -> binding.containsField(LifecycleResource.IRI_KEY)
            ? binding.getFieldValue(LifecycleResource.IRI_KEY)
            : binding.getFieldValue("id")));
    // For the same IRI, combine them using the add field array method
    groupedBindings.values().forEach(groupedBinding -> {
      if (groupedBinding.isEmpty()) {
        return;
      }
      SparqlBinding firstBinding = groupedBinding.get(0);
      if (groupedBinding.size() > 1) {
        for (int i = 1; i < groupedBinding.size(); i++) {
          firstBinding.addFieldArray(groupedBinding.get(i));
        }
      }
      result.offer(firstBinding);
    });
    return result;
  }
}