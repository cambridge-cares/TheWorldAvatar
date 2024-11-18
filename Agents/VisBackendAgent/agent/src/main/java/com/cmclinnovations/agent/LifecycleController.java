package com.cmclinnovations.agent;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.cmclinnovations.agent.model.response.ApiResponse;
import com.cmclinnovations.agent.model.type.LifecycleEventType;
import com.cmclinnovations.agent.service.AddService;
import com.cmclinnovations.agent.service.DeleteService;
import com.cmclinnovations.agent.service.LifecycleService;
import com.cmclinnovations.agent.utils.LifecycleResource;

@RestController
public class LifecycleController {
  private final AddService addService;
  private final DeleteService deleteService;
  private final LifecycleService lifecycleService;

  private static final String INVALID_CONTRACT_PARAMS_MSG = "Invalid request parameters! Contract IRI must be passed.";
  private static final String MISSING_DATE_MSG = "Missing `date` request parameters!";

  private static final Logger LOGGER = LogManager.getLogger(LifecycleController.class);

  public LifecycleController(AddService addService, DeleteService deleteService, LifecycleService lifecycleService) {
    this.addService = addService;
    this.deleteService = deleteService;
    this.lifecycleService = lifecycleService;
  }

  /**
   * Create a contract lifecycle (stages and events) for the specified contract,
   * and set it in draft state ie awaiting approval.
   */
  @PostMapping("/contracts/draft")
  public ResponseEntity<ApiResponse> genContractLifecycle(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(new ApiResponse(INVALID_CONTRACT_PARAMS_MSG), HttpStatus.BAD_REQUEST);
    }
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    // Add current date into parameters
    params.put(LifecycleResource.CURRENT_DATE_KEY, LifecycleResource.getCurrentDate());
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
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(
          new ApiResponse(INVALID_CONTRACT_PARAMS_MSG), HttpStatus.BAD_REQUEST);
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
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(
          new ApiResponse(INVALID_CONTRACT_PARAMS_MSG), HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to commence the services for a contract...");
    this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.APPROVED);
    ResponseEntity<ApiResponse> response = this.addService.instantiate(
        LifecycleResource.OCCURRENCE_INSTANT_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.CREATED) {
      LOGGER.info("Contract has been approved for service execution!");
      return new ResponseEntity<>("Contract has been approved for service execution!", HttpStatus.OK);
    } else {
      return response;
    }
  }

  /**
   * Reports any unfulfilled service delivery.
   */
  @PostMapping("/contracts/service/report")
  public ResponseEntity<ApiResponse> reportService(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(
          new ApiResponse(INVALID_CONTRACT_PARAMS_MSG), HttpStatus.BAD_REQUEST);
    }
    if (!params.containsKey(LifecycleResource.DATE_KEY)) {
      LOGGER.error(MISSING_DATE_MSG);
      return new ResponseEntity<>(new ApiResponse(MISSING_DATE_MSG), HttpStatus.BAD_REQUEST);
    }
    // Invalidate request if the report is being lodged for future dates
    if (LifecycleResource.checkDate(params.get(LifecycleResource.DATE_KEY).toString(), false)) {
      String errorMsg = "Invalid Date: Reports cannot be lodged for future dates. Please select today's date or any date in the past.";
      LOGGER.error(errorMsg);
      return new ResponseEntity<>(new ApiResponse(errorMsg), HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to report an unfulfilled service...");
    this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.SERVICE_MISS_REPORT);
    ResponseEntity<ApiResponse> response = this.addService.instantiate(
        LifecycleResource.OCCURRENCE_INSTANT_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.CREATED) {
      LOGGER.info("Report for an unfulfilled service has been successfully lodged!");
      return new ResponseEntity<>(new ApiResponse("Report for an unfulfilled service has been successfully lodged!"),
          HttpStatus.OK);
    } else {
      return response;
    }
  }

  /**
   * Cancel any upcoming service.
   */
  @PostMapping("/contracts/service/cancel")
  public ResponseEntity<ApiResponse> cancelService(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(
          new ApiResponse(INVALID_CONTRACT_PARAMS_MSG), HttpStatus.BAD_REQUEST);
    }
    if (!params.containsKey(LifecycleResource.DATE_KEY)) {
      LOGGER.error(MISSING_DATE_MSG);
      return new ResponseEntity<>(new ApiResponse(MISSING_DATE_MSG), HttpStatus.BAD_REQUEST);
    }
    // Invalidate request if the cancellation is targeted at past services
    if (LifecycleResource.checkDate(params.get(LifecycleResource.DATE_KEY).toString(), true)) {
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
      LOGGER.info("Service has been successfully cancelled!");
      return new ResponseEntity<>(new ApiResponse("Service has been successfully cancelled!"), HttpStatus.OK);
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
      params.put(LifecycleResource.CURRENT_DATE_KEY, LifecycleResource.getCurrentDate());
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
   * Validate if the request parameters are invalid or not. Returns true if
   * invalid.
   */
  private boolean isInvalidParams(Map<String, Object> params) {
    // Checks for contract
    if (!params.containsKey(LifecycleResource.CONTRACT_KEY)) {
      LOGGER.error(INVALID_CONTRACT_PARAMS_MSG);
      return true;
    }
    return false;
  }
}