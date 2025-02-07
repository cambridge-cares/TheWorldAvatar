package com.cmclinnovations.agent;

import java.text.MessageFormat;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.cmclinnovations.agent.model.SparqlResponseField;
import com.cmclinnovations.agent.model.response.ApiResponse;
import com.cmclinnovations.agent.model.type.LifecycleEventType;
import com.cmclinnovations.agent.service.AddService;
import com.cmclinnovations.agent.service.DeleteService;
import com.cmclinnovations.agent.service.application.LifecycleReportService;
import com.cmclinnovations.agent.service.application.LifecycleService;
import com.cmclinnovations.agent.service.core.DateTimeService;
import com.cmclinnovations.agent.utils.LifecycleResource;
import com.fasterxml.jackson.databind.JsonNode;

@RestController
public class LifecycleController {
  private final AddService addService;
  private final DateTimeService dateTimeService;
  private final DeleteService deleteService;
  private final LifecycleService lifecycleService;
  private final LifecycleReportService lifecycleReportService;

  private static final String MISSING_FIELD_MSG_TEMPLATE = "Missing `{0}` field in request parameters!";
  private static final String SOME_ORDER_GEN_FAIL_MSG = "Some orders have failed to be generated. Please read logs for more information.";

  private static final Logger LOGGER = LogManager.getLogger(LifecycleController.class);

  public LifecycleController(AddService addService, DateTimeService dateTimeService, DeleteService deleteService,
      LifecycleService lifecycleService, LifecycleReportService lifecycleReportService) {
    this.addService = addService;
    this.dateTimeService = dateTimeService;
    this.deleteService = deleteService;
    this.lifecycleService = lifecycleService;
    this.lifecycleReportService = lifecycleReportService;
  }

  /**
   * Create a contract lifecycle (stages and events) for the specified contract,
   * and set it in draft state ie awaiting approval.
   */
  @PostMapping("/contracts/draft")
  public ResponseEntity<ApiResponse> genContractLifecycle(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params, LifecycleResource.CONTRACT_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.CONTRACT_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    // Add current date into parameters
    params.put(LifecycleResource.CURRENT_DATE_KEY, this.dateTimeService.getCurrentDate());
    LOGGER.info("Received request to generate a new lifecycle for contract <{}>...", contractId);
    ResponseEntity<ApiResponse> response = this.addService.instantiate(LifecycleResource.LIFECYCLE_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.CREATED) {
      LOGGER.info("The lifecycle of the contract has been successfully drafted!");
      // Execute request for schedule as well
      ResponseEntity<ApiResponse> scheduleResponse = this.genContractSchedule(params);
      if (scheduleResponse.getStatusCode() == HttpStatus.CREATED) {
        LOGGER.info("Contract has been successfully drafted!");
        return new ResponseEntity<>(
            new ApiResponse("Contract has been successfully drafted", scheduleResponse.getBody().getIri()),
            HttpStatus.CREATED);
      }
      return scheduleResponse;
    } else {
      return response;
    }
  }

  /**
   * Create an upcoming schedule for the specified contract.
   */
  @PostMapping("/contracts/schedule")
  public ResponseEntity<ApiResponse> genContractSchedule(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params, LifecycleResource.CONTRACT_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.CONTRACT_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to generate the schedule details for contract...");
    this.lifecycleService.addStageInstanceToParams(params, LifecycleEventType.SERVICE_EXECUTION);
    ResponseEntity<ApiResponse> response = this.addService.instantiate(LifecycleResource.SCHEDULE_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.CREATED) {
      LOGGER.info("Schedule has been successfully drafted for contract!");
      return new ResponseEntity<>(
          new ApiResponse("Schedule has been successfully drafted for contract", response.getBody().getIri()),
          HttpStatus.CREATED);
    } else {
      return response;
    }
  }

  /**
   * Signal the commencement of the services for the specified contract.
   */
  @PostMapping("/contracts/service/commence")
  public ResponseEntity<?> commenceContract(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params, LifecycleResource.CONTRACT_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.CONTRACT_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to commence the services for a contract...");
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    boolean hasError = this.lifecycleService.genOrderReceivedOccurrences(contractId);
    if (hasError) {
      LOGGER.error(SOME_ORDER_GEN_FAIL_MSG);
      return new ResponseEntity<>(SOME_ORDER_GEN_FAIL_MSG, HttpStatus.INTERNAL_SERVER_ERROR);
    } else {
      LOGGER.info("All orders has been successfully received!");
      JsonNode report = this.lifecycleReportService.genReportInstance(contractId);
      ResponseEntity<ApiResponse> response = this.addService.instantiateJsonLd(report,
          "Report has been successfully instantiated!");
      if (response.getStatusCode() != HttpStatus.CREATED) {
        return response;
      }
      this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.APPROVED);
      response = this.addService.instantiate(
          LifecycleResource.OCCURRENCE_INSTANT_RESOURCE,
          params);
      if (response.getStatusCode() == HttpStatus.CREATED) {
        LOGGER.info("Contract has been approved for service execution!");
        return new ResponseEntity<>("Contract has been approved for service execution!", HttpStatus.OK);
      } else {
        return response;
      }
    }
  }

  /**
   * Assign dispatch details for the specified event
   */
  @PutMapping("/contracts/service/dispatch")
  public ResponseEntity<ApiResponse> assignDispatchDetails(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params, LifecycleResource.CONTRACT_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.CONTRACT_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to assign the dispatch details for a service order...");
    return this.lifecycleService.genDispatchOccurrence(params);
  }

  /**
   * Completes a specific service order
   */
  @PostMapping("/contracts/service/complete")
  public ResponseEntity<ApiResponse> completeServiceOrder(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params, LifecycleResource.CONTRACT_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.CONTRACT_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to complete a service order with completion details...");
    return this.lifecycleService.genDeliveryOccurrence(params);
  }

  /**
   * Reports any unfulfilled service delivery.
   */
  @PostMapping("/contracts/service/report")
  public ResponseEntity<ApiResponse> reportService(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params, LifecycleResource.CONTRACT_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.CONTRACT_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    if (this.isInvalidParams(params, LifecycleResource.DATE_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.DATE_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    String date = params.get(LifecycleResource.DATE_KEY).toString();
    // Invalidate request if the report is being lodged for future dates
    if (LifecycleResource.checkDate(date, false)) {
      String errorMsg = "Invalid Date: Reports cannot be lodged for future dates. Please select today's date or any date in the past.";
      LOGGER.error(errorMsg);
      return new ResponseEntity<>(new ApiResponse(errorMsg), HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to report an unfulfilled service...");
    this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.SERVICE_INCIDENT_REPORT);
    ResponseEntity<ApiResponse> response = this.addService.instantiate(
        LifecycleResource.OCCURRENCE_INSTANT_RESOURCE, params);
    if (response.getStatusCode() == HttpStatus.CREATED) {
      // If successfully added, remove the active service occurrence
      String occurrenceIri = this.lifecycleService
          .getActiveServiceOccurrence(params.get(LifecycleResource.CONTRACT_KEY).toString(), date);
      response = this.deleteService.delete(LifecycleResource.OCCURRENCE_INSTANT_RESOURCE, occurrenceIri);
      if (response.getStatusCode() == HttpStatus.OK) {
        LOGGER.info("Report for an unfulfilled service has been successfully lodged!");
        return new ResponseEntity<>(new ApiResponse("Report for an unfulfilled service has been successfully lodged!"),
            HttpStatus.OK);
      }
    }
    return response;
  }

  /**
   * Cancel any upcoming service.
   */
  @PostMapping("/contracts/service/cancel")
  public ResponseEntity<ApiResponse> cancelService(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params, LifecycleResource.CONTRACT_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.CONTRACT_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    if (this.isInvalidParams(params, LifecycleResource.DATE_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.DATE_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    // Invalidate request if the cancellation is targeted at past services
    String date = params.get(LifecycleResource.DATE_KEY).toString();
    if (LifecycleResource.checkDate(date, true)) {
      String errorMsg = "Invalid Date: Services can only be cancelled for today or future dates. Cancellation of past services is not allowed.";
      LOGGER.error(errorMsg);
      return new ResponseEntity<>(new ApiResponse(errorMsg), HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to cancel the upcoming service...");
    this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.SERVICE_CANCELLATION);
    ResponseEntity<ApiResponse> response = this.addService.instantiate(
        LifecycleResource.OCCURRENCE_INSTANT_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.CREATED) {
      // If successfully added, remove the active service occurrence
      String occurrenceIri = this.lifecycleService
          .getActiveServiceOccurrence(params.get(LifecycleResource.CONTRACT_KEY).toString(), date);
      response = this.deleteService.delete(LifecycleResource.OCCURRENCE_INSTANT_RESOURCE, occurrenceIri);
      if (response.getStatusCode() == HttpStatus.OK) {
        LOGGER.info("Service has been successfully cancelled!");
        return new ResponseEntity<>(new ApiResponse("Service has been successfully cancelled!"), HttpStatus.OK);
      }
    }
    return response;
  }

  /**
   * Rescind the ongoing contract specified.
   */
  @PostMapping("/contracts/archive/rescind")
  public ResponseEntity<ApiResponse> rescindContract(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params, LifecycleResource.CONTRACT_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.CONTRACT_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to rescind the contract...");
    this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.ARCHIVE_RESCINDMENT);
    ResponseEntity<ApiResponse> response = this.addService.instantiate(
        LifecycleResource.OCCURRENCE_INSTANT_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.CREATED) {
      LOGGER.info("Contract has been successfully rescinded!");
      return new ResponseEntity<>(new ApiResponse("Contract has been successfully rescinded!"), HttpStatus.OK);
    } else {
      return response;
    }
  }

  /**
   * Terminate the ongoing contract specified.
   */
  @PostMapping("/contracts/archive/terminate")
  public ResponseEntity<ApiResponse> terminateContract(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params, LifecycleResource.CONTRACT_KEY)) {
      return new ResponseEntity<>(
          new ApiResponse(MessageFormat.format(MISSING_FIELD_MSG_TEMPLATE, LifecycleResource.CONTRACT_KEY)),
          HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to terminate the contract...");
    this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.ARCHIVE_TERMINATION);
    ResponseEntity<ApiResponse> response = this.addService.instantiate(
        LifecycleResource.OCCURRENCE_INSTANT_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.CREATED) {
      LOGGER.info("Contract has been successfully terminated!");
      return new ResponseEntity<>(new ApiResponse("Contract has been successfully terminated!"), HttpStatus.OK);
    } else {
      return response;
    }
  }

  /**
   * Update the draft contract's lifecycle details in the knowledge graph.
   */
  @PutMapping("/contracts/draft")
  public ResponseEntity<ApiResponse> updateDraftContract(@RequestBody Map<String, Object> params) {
    LOGGER.info("Received request to update draft contract...");
    String targetId = params.get("id").toString();
    ResponseEntity<ApiResponse> deleteResponse = this.deleteService.delete(LifecycleResource.LIFECYCLE_RESOURCE,
        targetId);
    if (deleteResponse.getStatusCode().equals(HttpStatus.OK)) {
      // Add current date into parameters
      params.put(LifecycleResource.CURRENT_DATE_KEY, this.dateTimeService.getCurrentDate());
      ResponseEntity<ApiResponse> addResponse = this.addService.instantiate(
          LifecycleResource.LIFECYCLE_RESOURCE, targetId,
          params);
      if (addResponse.getStatusCode() == HttpStatus.CREATED) {
        LOGGER.info("The lifecycle of the contract has been successfully updated!");
        // Execute request for schedule as well
        ResponseEntity<ApiResponse> scheduleResponse = this.updateContractSchedule(params);
        if (scheduleResponse.getStatusCode() == HttpStatus.OK) {
          LOGGER.info("Draft contract has been successfully updated!");
          return new ResponseEntity<>(
              new ApiResponse("Draft contract has been successfully updated!", scheduleResponse.getBody().getIri()),
              HttpStatus.OK);
        }
        return scheduleResponse;
      } else {
        return addResponse;
      }
    } else {
      return deleteResponse;
    }
  }

  /**
   * Update the draft schedule details in the knowledge graph.
   */
  @PutMapping("/contracts/schedule")
  public ResponseEntity<ApiResponse> updateContractSchedule(@RequestBody Map<String, Object> params) {
    LOGGER.info("Received request to update a draft schedule...");
    this.lifecycleService.addStageInstanceToParams(params, LifecycleEventType.SERVICE_EXECUTION);
    String targetId = params.get("id").toString();
    ResponseEntity<ApiResponse> deleteResponse = this.deleteService.delete(LifecycleResource.SCHEDULE_RESOURCE,
        targetId);
    if (deleteResponse.getStatusCode().equals(HttpStatus.OK)) {
      ResponseEntity<ApiResponse> addResponse = this.addService.instantiate(LifecycleResource.SCHEDULE_RESOURCE,
          targetId,
          params);
      if (addResponse.getStatusCode() == HttpStatus.CREATED) {
        LOGGER.info("Draft schedule has been successfully updated!");
        return new ResponseEntity<>(
            new ApiResponse("Draft schedule has been successfully updated!", addResponse.getBody().getIri()),
            HttpStatus.OK);
      } else {
        return addResponse;
      }
    } else {
      return deleteResponse;
    }
  }

  /**
   * Retrieve the status of the contract
   */
  @GetMapping("/contracts/status/{id}")
  public ResponseEntity<ApiResponse> getContractStatus(@PathVariable String id) {
    LOGGER.info("Received request to retrieve the status for the contract: {}...", id);
    return this.lifecycleService.getContractStatus(id);
  }

  /**
   * Retrieve the schedule details for the specified contract to populate the form
   * template
   */
  @GetMapping("/contracts/schedule/{id}")
  public ResponseEntity<Map<String, SparqlResponseField>> getSchedule(@PathVariable String id) {
    LOGGER.info("Received request to retrieve the schedule for the contract: {}...", id);
    return this.lifecycleService.getSchedule(id);
  }

  /**
   * Retrieve all draft contracts ie awaiting approval.
   */
  @GetMapping("/contracts/draft")
  public ResponseEntity<?> getDraftContracts(
      @RequestParam(required = true) String type,
      @RequestParam(defaultValue = "false") boolean label) {
    LOGGER.info("Received request to retrieve draft contracts...");
    return this.lifecycleService.getContracts(type, label, LifecycleEventType.APPROVED);
  }

  /**
   * Retrieve all contracts that are currently in progress.
   */
  @GetMapping("/contracts/service")
  public ResponseEntity<?> getInProgressContracts(
      @RequestParam(required = true) String type,
      @RequestParam(defaultValue = "false") boolean label) {
    LOGGER.info("Received request to retrieve contracts in progress...");
    return this.lifecycleService.getContracts(type, label, LifecycleEventType.SERVICE_EXECUTION);
  }

  /**
   * Retrieve all archived contracts.
   */
  @GetMapping("/contracts/archive")
  public ResponseEntity<?> getArchivedContracts(
      @RequestParam(required = true) String type,
      @RequestParam(defaultValue = "false") boolean label) {
    LOGGER.info("Received request to retrieve archived contracts...");
    return this.lifecycleService.getContracts(type, label, LifecycleEventType.ARCHIVE_COMPLETION);
  }

  /**
   * Retrieve all tasks for the specified contract or date in UNIX timestamp.
   */
  @GetMapping("/contracts/service/{task}")
  public ResponseEntity<?> getAllTasksOnTimestamp(
      @PathVariable(name = "task") String input) {
    // Attempt to parse the input into a long, indicating this is a timestamp
    try {
      long timestamp = Long.parseLong(input);
      LOGGER.info("Received request to retrieve services in progress for a specified date...");
      return this.lifecycleService.getOccurrences(timestamp);
    } catch (NumberFormatException e) {
      // Any parsing errors will indicate that this is an identifier for the contract
      LOGGER.info("Received request to retrieve services in progress for a specified contract...");
      return this.lifecycleService.getOccurrences(input);
    }
  }

  /**
   * Retrieves the form template for the dispatch order from the knowledge graph.
   */
  @GetMapping("/contracts/service/dispatch/form")
  public ResponseEntity<?> getDispatchForm() {
    LOGGER.info("Received request to get form template for order dispatch...");
    return this.lifecycleService.getForm(LifecycleEventType.SERVICE_ORDER_DISPATCHED, null);
  }

  /**
   * Retrieve dispatch details for the specified event
   */
  @GetMapping("/contracts/service/dispatch/{id}")
  public ResponseEntity<?> getDispatchDetails(@PathVariable String id) {
    LOGGER.info("Received request to get form template with order dispatch details...");
    return this.lifecycleService.getForm(LifecycleEventType.SERVICE_ORDER_DISPATCHED, id);
  }

  /**
   * Retrieves the form template to complete an order from the knowledge graph.
   */
  @GetMapping("/contracts/service/complete/form")
  public ResponseEntity<?> getOrderCompleteForm() {
    LOGGER.info("Received request to get form template for order completion...");
    return this.lifecycleService.getForm(LifecycleEventType.SERVICE_EXECUTION, null);
  }

  /**
   * Retrieves the form template to report the order from the knowledge graph.
   */
  @GetMapping("/contracts/service/report/form")
  public ResponseEntity<?> getOrderReportForm() {
    LOGGER.info("Received request to get form template to report the order...");
    return this.lifecycleService.getForm(LifecycleEventType.SERVICE_INCIDENT_REPORT, null);
  }

  /**
   * Retrieves the form template to cancel the order from the knowledge graph.
   */
  @GetMapping("/contracts/service/cancel/form")
  public ResponseEntity<?> getOrderCancellationForm() {
    LOGGER.info("Received request to get form template to cancel the order...");
    return this.lifecycleService.getForm(LifecycleEventType.SERVICE_CANCELLATION, null);
  }

  /**
   * Retrieves the form template to rescind the contract from the knowledge graph.
   */
  @GetMapping("/contracts/archive/rescind/form")
  public ResponseEntity<?> getContractRescissionForm() {
    LOGGER.info("Received request to get form template to rescind the contract...");
    return this.lifecycleService.getForm(LifecycleEventType.ARCHIVE_RESCINDMENT, null);
  }

  /**
   * Retrieves the form template to terminate the contract from the knowledge
   * graph.
   */
  @GetMapping("/contracts/archive/terminate/form")
  public ResponseEntity<?> getContractTerminationForm() {
    LOGGER.info("Received request to get form template to terminate the contract...");
    return this.lifecycleService.getForm(LifecycleEventType.ARCHIVE_TERMINATION, null);
  }

  /**
   * Validate if the request parameters are invalid or not. Returns true if
   * invalid.
   */
  private boolean isInvalidParams(Map<String, Object> params, String field) {
    // Checks for contract
    if (!params.containsKey(field)) {
      LOGGER.error("Missing `{}` field in request parameters!", field);
      return true;
    }
    return false;
  }
}