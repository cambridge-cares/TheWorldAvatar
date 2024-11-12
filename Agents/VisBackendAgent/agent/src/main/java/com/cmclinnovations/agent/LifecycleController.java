package com.cmclinnovations.agent;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

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
  public ResponseEntity<String> genContractLifecycle(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(INVALID_CONTRACT_PARAMS_MSG, HttpStatus.BAD_REQUEST);
    }
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    // Add current date into parameters
    params.put(LifecycleResource.CURRENT_DATE_KEY, LifecycleResource.getCurrentDate());
    LOGGER.info("Received request to generate a new lifecycle for contract <{}>...", contractId);
    ResponseEntity<String> response = this.addService.instantiate(LifecycleResource.LIFECYCLE_RESOURCE, params);
    if (response.getStatusCode() == HttpStatus.OK) {
      LOGGER.info("The lifecycle of the contract has been successfully drafted!");
      // Execute request for schedule as well
      ResponseEntity<String> scheduleResponse = this.genContractSchedule(params);
      if (scheduleResponse.getStatusCode() == HttpStatus.OK) {
        LOGGER.info("Contract has been successfully drafted!");
        return new ResponseEntity<>("Contract has been successfully drafted", HttpStatus.CREATED);
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
  public ResponseEntity<String> genContractSchedule(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(INVALID_CONTRACT_PARAMS_MSG, HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to generate the schedule details for contract...");
    this.lifecycleService.addEventInstance(params, LifecycleEventType.SERVICE_EXECUTION);
    ResponseEntity<String> response = this.addService.instantiate(LifecycleResource.SCHEDULE_RESOURCE, params);
    if (response.getStatusCode() == HttpStatus.CREATED) {
      LOGGER.info("Schedule has been successfully drafted for contract!");
      return new ResponseEntity<>("Schedule has been successfully drafted for contract", HttpStatus.CREATED);
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
      return new ResponseEntity<>(INVALID_CONTRACT_PARAMS_MSG, HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to commence the services for a contract...");
    this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.APPROVED);
    ResponseEntity<String> response = this.addService.instantiate(LifecycleResource.OCCURRENCE_INSTANT_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.OK) {
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
  public ResponseEntity<?> reportService(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(INVALID_CONTRACT_PARAMS_MSG, HttpStatus.BAD_REQUEST);
    }
    if (!params.containsKey(LifecycleResource.DATE_KEY)) {
      LOGGER.error(MISSING_DATE_MSG);
      return new ResponseEntity<>(MISSING_DATE_MSG, HttpStatus.BAD_REQUEST);
    }
    // Invalidate request if the report is being lodged for future dates
    if (LifecycleResource.checkDate(params.get(LifecycleResource.DATE_KEY).toString(), false)) {
      String errorMsg = "Invalid Date: Reports cannot be lodged for future dates. Please select today's date or any date in the past.";
      LOGGER.error(errorMsg);
      return new ResponseEntity<>(errorMsg, HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to report an unfulfilled service...");
    this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.SERVICE_MISS_REPORT);
    ResponseEntity<String> response = this.addService.instantiate(LifecycleResource.OCCURRENCE_INSTANT_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.OK) {
      LOGGER.info("Report for an unfulfilled service has been successfully lodged!");
      return new ResponseEntity<>("Report for an unfulfilled service has been successfully lodged!", HttpStatus.OK);
    } else {
      return response;
    }
  }

  /**
   * Cancel any upcoming service.
   */
  @PostMapping("/contracts/service/cancel")
  public ResponseEntity<?> cancelService(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(INVALID_CONTRACT_PARAMS_MSG, HttpStatus.BAD_REQUEST);
    }
    if (!params.containsKey(LifecycleResource.DATE_KEY)) {
      LOGGER.error(MISSING_DATE_MSG);
      return new ResponseEntity<>(MISSING_DATE_MSG, HttpStatus.BAD_REQUEST);
    }
    // Invalidate request if the cancellation is targeted at past services
    if (LifecycleResource.checkDate(params.get(LifecycleResource.DATE_KEY).toString(), true)) {
      String errorMsg = "Invalid Date: Services can only be cancelled for today or future dates. Cancellation of past services is not allowed.";
      LOGGER.error(errorMsg);
      return new ResponseEntity<>(errorMsg, HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to cancel the upcoming service...");
    this.lifecycleService.addOccurrenceParams(params, LifecycleEventType.SERVICE_CANCELLATION);
    ResponseEntity<String> response = this.addService.instantiate(LifecycleResource.OCCURRENCE_INSTANT_RESOURCE,
        params);
    if (response.getStatusCode() == HttpStatus.OK) {
      LOGGER.info("Service has been successfully cancelled!");
      return new ResponseEntity<>("Service has been successfully cancelled!", HttpStatus.OK);
    } else {
      return response;
    }
  }

  /**
   * Update the draft contract's lifecycle details in the knowledge graph.
   */
  @PutMapping("/contracts/draft")
  public ResponseEntity<String> updateDraftContract(@RequestBody Map<String, Object> params) {
    LOGGER.info("Received request to update draft contract...");
    String targetId = params.get("id").toString();
    ResponseEntity<String> deleteResponse = this.deleteService.delete(LifecycleResource.LIFECYCLE_RESOURCE, targetId);
    if (deleteResponse.getStatusCode().equals(HttpStatus.OK)) {
      // Add current date into parameters
      params.put(LifecycleResource.CURRENT_DATE_KEY, LifecycleResource.getCurrentDate());
      ResponseEntity<String> addResponse = this.addService.instantiate(LifecycleResource.LIFECYCLE_RESOURCE, targetId,
          params);
      if (addResponse.getStatusCode() == HttpStatus.OK) {
        LOGGER.info("The lifecycle of the contract has been successfully updated!");
        // Execute request for schedule as well
        ResponseEntity<String> scheduleResponse = this.updateContractSchedule(params);
        if (scheduleResponse.getStatusCode() == HttpStatus.OK) {
          LOGGER.info("Draft contract has been successfully updated!");
          return new ResponseEntity<>("Draft contract has been successfully updated!", HttpStatus.OK);
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
  public ResponseEntity<String> updateContractSchedule(@RequestBody Map<String, Object> params) {
    LOGGER.info("Received request to update a draft schedule...");
    this.lifecycleService.addEventInstance(params, LifecycleEventType.SERVICE_EXECUTION);
    String targetId = params.get("id").toString();
    ResponseEntity<String> deleteResponse = this.deleteService.delete(LifecycleResource.SCHEDULE_RESOURCE, targetId);
    if (deleteResponse.getStatusCode().equals(HttpStatus.OK)) {
      ResponseEntity<String> addResponse = this.addService.instantiate(LifecycleResource.SCHEDULE_RESOURCE, targetId,
          params);
      if (addResponse.getStatusCode() == HttpStatus.OK) {
        LOGGER.info("Draft schedule has been successfully updated!");
        return new ResponseEntity<>("Draft schedule has been successfully updated!", HttpStatus.OK);
      } else {
        return addResponse;
      }
    } else {
      return deleteResponse;
    }
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