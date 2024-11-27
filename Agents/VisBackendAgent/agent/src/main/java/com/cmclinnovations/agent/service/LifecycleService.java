package com.cmclinnovations.agent.service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.SparqlResponseField;
import com.cmclinnovations.agent.model.response.ApiResponse;
import com.cmclinnovations.agent.model.type.LifecycleEventType;
import com.cmclinnovations.agent.model.type.SparqlEndpointType;
import com.cmclinnovations.agent.utils.LifecycleResource;
import com.cmclinnovations.agent.utils.StringResource;

@Service
public class LifecycleService {
  private final AddService addService;
  private final KGService kgService;
  private final FileService fileService;

  private static final Logger LOGGER = LogManager.getLogger(LifecycleService.class);

  /**
   * Constructs a new service with the following dependencies.
   * 
   * @param kgService KG service for performing the query.
   */
  public LifecycleService(AddService addService, KGService kgService, FileService fileService) {
    this.addService = addService;
    this.kgService = kgService;
    this.fileService = fileService;
  }

  /**
   * Add the required stage instance into the request parameters.
   * 
   * @param params    The target parameters to update.
   * @param eventType The target event type to retrieve.
   */
  public void addStageInstanceToParams(Map<String, Object> params, LifecycleEventType eventType) {
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    LOGGER.debug("Adding stage parameters for {}...", contractId);
    String stage = this.getStageInstance(contractId, eventType);
    params.put(LifecycleResource.STAGE_KEY, stage);
  }

  /**
   * Populate the remaining occurrence parameters into the request parameters.
   * 
   * @param params    The target parameters to update.
   * @param eventType The target event type to retrieve.
   */
  public void addOccurrenceParams(Map<String, Object> params, LifecycleEventType eventType) {
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    LOGGER.debug("Adding occurrence parameters for {}...", contractId);
    String stage = this.getStageInstance(contractId, eventType);
    params.put("id", StringResource.getPrefix(stage) + "/" + LifecycleResource.getEventIdentifier(eventType) + "/"
        + UUID.randomUUID());
    params.put(LifecycleResource.STAGE_KEY, stage);
    params.put(LifecycleResource.EVENT_KEY, LifecycleResource.getEventClass(eventType));
    // Only retrieve the current date if no date input is given
    params.putIfAbsent(LifecycleResource.DATE_KEY, LifecycleResource.getCurrentDate());
  }

  /**
   * Retrieve the status of the contract.
   * 
   * @param contract The target contract id.
   */
  public ResponseEntity<ApiResponse> getStatus(String contract) {
    LOGGER.debug("Retrieving the status of the contract...");
    String query = LifecycleResource.genServiceStatusQuery(contract);
    Queue<SparqlBinding> results = this.kgService.query(query, SparqlEndpointType.BLAZEGRAPH);
    SparqlBinding result = this.kgService.getSingleInstance(results);
    return new ResponseEntity<>(
        new ApiResponse(result.getFieldValue(LifecycleResource.STATUS_KEY),
            result.getFieldValue(LifecycleResource.IRI_KEY)),
        HttpStatus.OK);
  }

  /**
   * Retrieve the schedule details of the contract.
   * 
   * @param contract The target contract id.
   */
  public ResponseEntity<Map<String, SparqlResponseField>> getSchedule(String contract) {
    LOGGER.debug("Retrieving the schedule details of the contract...");
    String query = LifecycleResource.genServiceScheduleQuery(contract);
    Queue<SparqlBinding> results = this.kgService.query(query, SparqlEndpointType.BLAZEGRAPH);
    SparqlBinding result = this.kgService.getSingleInstance(results);
    return new ResponseEntity<>(result.get(), HttpStatus.OK);
  }

  /**
   * Retrieve all the contract instances and their information based on the
   * resource ID.
   * 
   * @param resourceID The target resource identifier for the instance class.
   * @param eventType  The target event type to retrieve.
   */
  public ResponseEntity<?> getContracts(String resourceID, boolean requireLabel, LifecycleEventType eventType) {
    LOGGER.debug("Retrieving all contracts...");
    ResponseEntity<String> iriResponse = this.fileService.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    // Only use the label query if required due to the slower query performance
    String queryPath = requireLabel ? FileService.SHACL_PATH_LABEL_QUERY_RESOURCE
        : FileService.SHACL_PATH_QUERY_RESOURCE;

    String query = this.fileService.getContentsWithReplacement(queryPath, iriResponse.getBody());
    Queue<SparqlBinding> results = this.kgService.queryInstances(query, null, false, eventType);
    return new ResponseEntity<>(results.stream().map(SparqlBinding::get).collect(Collectors.toList()), HttpStatus.OK);
  }

  /**
   * Retrieve all service related occurrences in the lifecycle for the specified
   * date.
   * 
   * @param date Target date in YYYY-MM-DD format.
   */
  public ResponseEntity<List<Map<String, SparqlResponseField>>> getOccurrences(String date) {
    String activeServiceQuery = LifecycleResource.genServiceTasksQuery(date);
    Queue<SparqlBinding> results = this.kgService.query(activeServiceQuery, SparqlEndpointType.BLAZEGRAPH);
    return new ResponseEntity<>(
        results.stream()
            .map(SparqlBinding::get)
            .collect(Collectors.toList()),
        HttpStatus.OK);
  }

  /**
   * Retrieve the specific service occurence in the lifecycle for the specified
   * contract and date.
   * 
   * @param contract Target contract iri.
   * @param date     Target date in YYYY-MM-DD format.
   */
  public String getActiveServiceOccurrence(String contract, String date) {
    String activeServiceQuery = LifecycleResource.genActiveServiceTaskQuery(contract, date);
    Queue<SparqlBinding> results = this.kgService.query(activeServiceQuery, SparqlEndpointType.BLAZEGRAPH);
    return this.kgService.getSingleInstance(results).getFieldValue(LifecycleResource.IRI_KEY);
  }

  /**
   * Generate occurrences for all active services that should be scheduled for
   * today.
   */
  public ResponseEntity<ApiResponse> genActiveServiceOccurrences() {
    LOGGER.info("Generating today's tasks for active services...");
    boolean hasError = false;
    // Iterate through all possible endpoints to find the current day of week
    List<String> endpoints = this.kgService.getEndpoints(SparqlEndpointType.BLAZEGRAPH);
    int index = 0;
    String dayOfWeekInstance = "";
    // Iterate through loop until day of week is found
    LOGGER.info("Querying for a matching day of week for today...");
    while (index < endpoints.size() && dayOfWeekInstance.isEmpty()) {
      Queue<SparqlBinding> results = this.kgService.query(LifecycleResource.genTodayDayOfWeekQuery(),
          endpoints.get(index));
      try {
        dayOfWeekInstance = this.kgService.getSingleInstance(results).getFieldValue(LifecycleResource.IRI_KEY);
      } catch (Exception e) {
        // It is possible that there is no day of week instance at some endpoints and
        // this should be ignored
      } finally {
        index++;
      }
    }
    if (dayOfWeekInstance.isEmpty()) {
      LOGGER.error("Invalid data: Unable to find a valid day of week instance!");
      return new ResponseEntity<>(new ApiResponse("Invalid data: Unable to find a valid day of week instance!"),
          HttpStatus.INTERNAL_SERVER_ERROR);
    } else {
      LOGGER.info("Detected a matching day of week! Continue execution...");
      String activeServiceQuery = LifecycleResource.genActiveServiceQuery(dayOfWeekInstance);
      Queue<SparqlBinding> results = this.kgService.query(activeServiceQuery, SparqlEndpointType.BLAZEGRAPH);
      Map<String, Object> params = new HashMap<>();
      // While there are active services to be instantiated
      while (!results.isEmpty()) {
        params.clear();
        // retrieve the contract IRI to instantiate a new occurrence
        String contract = results.poll().getFieldValue(LifecycleResource.IRI_KEY);
        params.put(LifecycleResource.CONTRACT_KEY, contract);
        params.put(LifecycleResource.REMARKS_KEY, ""); // Empty remarks
        this.addOccurrenceParams(params, LifecycleEventType.SERVICE_EXECUTION);
        ResponseEntity<ApiResponse> response = this.addService.instantiate(
            LifecycleResource.OCCURRENCE_INSTANT_RESOURCE, params);
        // Logs to provide details on the generation status of each task
        if (response.getStatusCode() == HttpStatus.CREATED) {
          LOGGER.info("Task for {} has been created!", contract);
        } else {
          LOGGER.error("Error encountered while creating task for {}! Read error message for more details: {}",
              contract, response.getBody().getMessage());
          hasError = true;
        }
      }
      String responseMessage = hasError
          ? "Some tasks have failed to be generated. Please read logs for more information."
          : "All tasks has been successfully generated!";
      HttpStatusCode status = hasError ? HttpStatus.INTERNAL_SERVER_ERROR : HttpStatus.OK;
      return new ResponseEntity<>(new ApiResponse(responseMessage), status);
    }
  }

  /**
   * Retrieve the stage occurrence instance associated with the target contract.
   * 
   * @param query     The query to execute.
   * @param eventType The target event type to retrieve.
   */
  private String getStageInstance(String contractId, LifecycleEventType eventType) {
    LOGGER.debug("Retrieving stage instance for {}...", contractId);
    String query = "PREFIX fibo-fnd-arr-lif: <https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/>" +
        "PREFIX fibo-fnd-rel-rel: <https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/>" +
        "SELECT DISTINCT ?iri WHERE {" +
        "<" + contractId + "> fibo-fnd-arr-lif:hasLifecycle ?lifecycle ." +
        "?lifecycle fibo-fnd-arr-lif:hasStage ?iri ." +
        "?iri fibo-fnd-rel-rel:exemplifies <" + LifecycleResource.getStageClass(eventType) + "> ." +
        "}";
    Queue<SparqlBinding> results = this.kgService.query(query, SparqlEndpointType.BLAZEGRAPH);
    return this.kgService.getSingleInstance(results).getFieldValue(LifecycleResource.IRI_KEY);
  }
}