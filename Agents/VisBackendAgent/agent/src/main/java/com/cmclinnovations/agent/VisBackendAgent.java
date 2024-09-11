package com.cmclinnovations.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import com.cmclinnovations.agent.service.GetService;

@RestController
public class VisBackendAgent {
  private final GetService getService;

  private static final Logger LOGGER = LogManager.getLogger(VisBackendAgent.class);

  public VisBackendAgent(GetService getService) {
    this.getService = getService;
  }

  @GetMapping("/status")
  public ResponseEntity<String> getStatus() {
    LOGGER.info("Detected request to get agent status...");
    return new ResponseEntity<>(
        "Agent is ready to receive requests.",
        HttpStatus.OK);
  }

  /**
   * Retrieves the form template for the specified type from the knowledge graph.
   */
  @GetMapping("/form/{type}")
  public ResponseEntity<?> getFormTemplate(@PathVariable(name = "type") String type) {
    LOGGER.info("Received request to get the form template for {}...", type);
    return this.getService.getForm(type);
  }

  /**
   * Retrieve the available instances alongside their label and description for
   * the specified type in the knowledge graph.
   */
  @GetMapping("/type/{type}")
  public ResponseEntity<?> getInstances(@PathVariable(name = "type") String type) {
    LOGGER.info("Received request to get instances for {}...", type);
    return this.getService.getInstances(type);
  }
}