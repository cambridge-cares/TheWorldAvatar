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
  public ResponseEntity<?> genContractLifecycle(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(INVALID_CONTRACT_PARAMS_MSG, HttpStatus.BAD_REQUEST);
    }
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    // Add current date into parameters
    params.put(LifecycleResource.CURRENT_DATE_KEY, LifecycleResource.getCurrentDate());
    LOGGER.info("Received request to generate a new lifecycle for contract <{}>...", contractId);
    ResponseEntity<String> response = this.addService.instantiate(LifecycleResource.LIFECYCLE_RESOURCE, params);
    if (response.getStatusCode() == HttpStatus.OK) {
      LOGGER.info("Contract has been successfully drafted!");
      return new ResponseEntity<>("Contract has been successfully drafted", HttpStatus.CREATED);
    } else {
      return response;
    }
  }

  /**
   * Create an upcoming schedule for the specified contract.
   */
  @PostMapping("/contracts/schedule")
  public ResponseEntity<?> genContractSchedule(@RequestBody Map<String, Object> params) {
    if (this.isInvalidParams(params)) {
      return new ResponseEntity<>(INVALID_CONTRACT_PARAMS_MSG, HttpStatus.BAD_REQUEST);
    }
    LOGGER.info("Received request to generate the schedule details for contract...");
    this.lifecycleService.addEventInstance(params, LifecycleEventType.SERVICE_EXECUTION);
    ResponseEntity<String> response = this.addService.instantiate(LifecycleResource.SCHEDULE_RESOURCE, params);
    if (response.getStatusCode() == HttpStatus.OK) {
      LOGGER.info("Schedule has been successfully drafted for contract!");
      return new ResponseEntity<>("Schedule has been successfully drafted for contract", HttpStatus.CREATED);
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
        LOGGER.info("Draft contract has been successfully updated!");
        return new ResponseEntity<>("Draft contract has been successfully updated!", HttpStatus.CREATED);
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
        return new ResponseEntity<>("Draft schedule has been successfully updated!", HttpStatus.CREATED);
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