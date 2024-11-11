package com.cmclinnovations.agent;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.cmclinnovations.agent.service.AddService;
import com.cmclinnovations.agent.utils.LifecycleResource;

@RestController
public class LifecycleController {
  private final AddService addService;
  private static final Logger LOGGER = LogManager.getLogger(LifecycleController.class);

  public LifecycleController(AddService addService) {
    this.addService = addService;
  }

  /**
   * Create a contract lifecycle (stages and events) for the specified contract,
   * and set it in draft state ie awaiting approval.
   */
  @PostMapping("/contracts/draft")
  public ResponseEntity<?> genContractLifecycle(@RequestBody Map<String, Object> params) {
    if (params.containsKey(LifecycleResource.CONTRACT_KEY)) {
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
    } else {
      String errorMsg = "Invalid request parameters! Contract IRI must be passed.";
      LOGGER.error(errorMsg);
      return new ResponseEntity<>(errorMsg, HttpStatus.BAD_REQUEST);
    }
  }
}