package com.cmclinnovations.agent.service;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
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
  private final DateTimeService dateTimeService;
  private final DeleteService deleteService;
  private final GetService getService;
  private final KGService kgService;
  private final FileService fileService;

  private static final String ORDER_INITIALISE_MESSAGE = "Order received and is being processed.";
  private static final String ORDER_DISPATCH_MESSAGE = "Order has been assigned and is awaiting execution.";
  private static final Logger LOGGER = LogManager.getLogger(LifecycleService.class);

  /**
   * Constructs a new service with the following dependencies.
   * 
   * @param kgService KG service for performing the query.
   */
  public LifecycleService(AddService addService, DateTimeService dateTimeService, DeleteService deleteService,
      GetService getService, KGService kgService, FileService fileService) {
    this.addService = addService;
    this.dateTimeService = dateTimeService;
    this.deleteService = deleteService;
    this.getService = getService;
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
   * Defaults to the current date as date is not supplied.
   * 
   * @param params    The target parameters to update.
   * @param eventType The target event type to retrieve.
   */
  public void addOccurrenceParams(Map<String, Object> params, LifecycleEventType eventType) {
    addOccurrenceParams(params, eventType, this.dateTimeService.getCurrentDate());
  }

  /**
   * Populate the remaining occurrence parameters into the request parameters.
   * 
   * @param params    The target parameters to update.
   * @param eventType The target event type to retrieve.
   * @param date      Date in YYYY-MM-DD format.
   */
  public void addOccurrenceParams(Map<String, Object> params, LifecycleEventType eventType, String date) {
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    LOGGER.debug("Adding occurrence parameters for {}...", contractId);
    String stage = this.getStageInstance(contractId, eventType);
    params.putIfAbsent("id",
        StringResource.getPrefix(stage) + "/" + LifecycleResource.getEventIdentifier(eventType) + "/"
            + UUID.randomUUID());
    params.put(LifecycleResource.STAGE_KEY, stage);
    params.put(LifecycleResource.EVENT_KEY, LifecycleResource.getEventClass(eventType));
    // Only update the date field if there is no pre-existing field
    params.putIfAbsent(LifecycleResource.DATE_KEY, date);
  }

  /**
   * Retrieve the status of the contract.
   * 
   * @param contract The target contract id.
   */
  public ResponseEntity<ApiResponse> getContractStatus(String contract) {
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
   * @param timestamp Timestamp in UNIX format.
   */
  public ResponseEntity<List<Map<String, SparqlResponseField>>> getOccurrences(long timestamp) {
    // Get date from timestamp
    String targetDate = this.dateTimeService.getDateFromTimestamp(timestamp);
    String activeServiceQuery = LifecycleResource.genServiceTasksQuery(targetDate);
    Queue<SparqlBinding> results = this.kgService.query(activeServiceQuery, SparqlEndpointType.BLAZEGRAPH);
    Map<String, SparqlBinding> resultMapping = results.stream()
        .collect(Collectors.toMap(
            binding -> binding.getFieldValue(LifecycleResource.CONTRACT_KEY), // Key mapper
            binding -> binding, // Value mapper
            (existing, replacement) -> // Merge function to keep the higher order
            Integer.parseInt(existing.getFieldValue(LifecycleResource.ORDER_KEY)) > Integer
                .parseInt(replacement.getFieldValue(LifecycleResource.ORDER_KEY))
                    ? existing
                    : replacement));
    return new ResponseEntity<>(
        resultMapping.values().stream()
            .map(SparqlBinding::get)
            .toList(),
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
   * Generate occurrences for the order received event of a specified contract.
   * 
   * @param contract Target contract.
   * @return boolean indicating if the occurrences have been generated
   *         successfully.
   */
  public boolean genOrderReceivedOccurrences(String contract) {
    LOGGER.info("Generating all orders for the active contract {}...", contract);
    // Retrieve schedule information for the specific contract
    String query = LifecycleResource.genServiceScheduleQuery(contract);
    Queue<SparqlBinding> results = this.kgService.query(query, SparqlEndpointType.BLAZEGRAPH);
    SparqlBinding bindings = this.kgService.getSingleInstance(results);
    // Extract specific schedule info
    String startDate = bindings
        .getFieldValue(StringResource.parseQueryVariable(LifecycleResource.SCHEDULE_START_DATE_KEY));
    String endDate = bindings.getFieldValue(StringResource.parseQueryVariable(LifecycleResource.SCHEDULE_END_DATE_KEY));
    String recurrence = bindings
        .getFieldValue(StringResource.parseQueryVariable(LifecycleResource.SCHEDULE_RECURRENCE_KEY));
    Queue<String> occurrences = new ArrayDeque<>();
    // Extract date of occurrences based on the schedule information
    // For single time schedules, simply add the start date
    if (recurrence.equals("P1D")) {
      occurrences.offer(startDate);
    } else if (recurrence.equals("P2D")) {
      // Alternate day recurrence should have dual interval
      occurrences = this.dateTimeService.getOccurrenceDates(startDate, endDate, 2);
    } else {
      // Note that this may run for other intervals like P3D but
      // an error will be thrown in the following method unless the recurrence is in
      // intervals of 7
      int weeklyInterval = this.dateTimeService.getWeeklyInterval(recurrence);
      occurrences = this.dateTimeService.getOccurrenceDates(startDate, endDate, bindings, weeklyInterval);
    }
    // Add parameter template
    Map<String, Object> params = new HashMap<>();
    params.put(LifecycleResource.CONTRACT_KEY, contract);
    params.put(LifecycleResource.REMARKS_KEY, ORDER_INITIALISE_MESSAGE);
    this.addOccurrenceParams(params, LifecycleEventType.SERVICE_ORDER_RECEIVED);
    String orderPrefix = StringResource.getPrefix(params.get(LifecycleResource.STAGE_KEY).toString()) + "/"
        + LifecycleResource.getEventIdentifier(LifecycleEventType.SERVICE_ORDER_RECEIVED) + "/";
    // Instantiate each occurrence
    boolean hasError = false;
    while (!occurrences.isEmpty()) {
      // Retrieve and update the date of occurrence
      String occurrenceDate = occurrences.poll();
      // set new id each time
      params.put("id", orderPrefix + UUID.randomUUID());
      params.put(LifecycleResource.DATE_KEY, occurrenceDate);
      ResponseEntity<ApiResponse> response = this.addService.instantiate(
          LifecycleResource.OCCURRENCE_INSTANT_RESOURCE, params);
      // Error logs for any specified occurrence
      if (response.getStatusCode() != HttpStatus.CREATED) {
        LOGGER.error("Error encountered while creating order for {} on {}! Read error message for more details: {}",
            contract, occurrenceDate, response.getBody().getMessage());
        hasError = true;
      }
    }
    return hasError;
  }

  /**
   * Generate an occurrence for the order dispatch event of a specified contract.
   * 
   * @param params Required parameters with configurable parameters to instantiate
   *               the occurrence.
   */
  public ResponseEntity<ApiResponse> genDispatchOccurrence(Map<String, Object> params) {
    params.put(LifecycleResource.REMARKS_KEY, ORDER_DISPATCH_MESSAGE);
    this.addOccurrenceParams(params, LifecycleEventType.SERVICE_ORDER_DISPATCHED);
    // Attempt to delete any existing dispatch occurrence before any updates
    ResponseEntity<ApiResponse> response = this.deleteService.delete(
        LifecycleResource.getEventIdentifier(LifecycleEventType.SERVICE_ORDER_DISPATCHED), params.get("id").toString());
    // Request will return ok even if no related occurrence exists
    if (response.getStatusCode().equals(HttpStatus.OK)) {
      // Ensure that the event identifier mapped directly to the jsonLd file name
      response = this.addService.instantiate(
          LifecycleResource.getEventIdentifier(LifecycleEventType.SERVICE_ORDER_DISPATCHED), params);
      if (response.getStatusCode() != HttpStatus.CREATED) {
        LOGGER.error(
            "Error encountered while dispatching details for the order: {}! Read error message for more details: {}",
            params.get(LifecycleResource.ORDER_KEY), response.getBody().getMessage());
      }
    }
    return response;
  }

  /**
   * Retrieves the form template for the specified event type.
   * 
   * @param eventType The target event type.
   * @param targetId  The target instance IRI.
   */
  public ResponseEntity<?> getForm(LifecycleEventType eventType, String targetId) {
    // Ensure that there is a specific event type target
    String replacementQueryLine = "<https://spec.edmcouncil.org/fibo/ontology/FBC/ProductsAndServices/FinancialProductsAndServices/ContractLifecycleEventOccurrence>;"
        + "sh:property/sh:hasValue " + StringResource.parseIriForQuery(LifecycleResource.getEventClass(eventType));
    Map<String, Object> currentEntity = new HashMap<>();
    if (targetId != null) {
      LOGGER.debug("Detected specific entity ID! Retrieving relevant entity information for occurrence of {} ...",
          eventType);
      ResponseEntity<?> currentEntityResponse = this.getService.getInstance("occurrence", targetId,
          replacementQueryLine);
      if (currentEntityResponse.getStatusCode() == HttpStatus.OK) {
        currentEntity = (Map<String, Object>) currentEntityResponse.getBody();
      }
    }
    String query = this.fileService.getContentsWithReplacement(FileService.FORM_QUERY_RESOURCE, replacementQueryLine);
    Map<String, Object> results = this.kgService.queryForm(query, currentEntity);
    if (results.isEmpty()) {
      LOGGER.error(KGService.INVALID_SHACL_ERROR_MSG);
      return new ResponseEntity<>(
          KGService.INVALID_SHACL_ERROR_MSG,
          HttpStatus.INTERNAL_SERVER_ERROR);
    } else {
      LOGGER.info("Request has been completed successfully!");
      return new ResponseEntity<>(
          results,
          HttpStatus.OK);
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