package com.cmclinnovations.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class VisBackendAgent {
  private static final Logger LOGGER = LogManager.getLogger(VisBackendAgent.class);

  public VisBackendAgent() {
  }

  @GetMapping("/status")
  public ResponseEntity<String> getStatus() {
    LOGGER.info("Detected request to get agent status...");
    return new ResponseEntity<>(
        "Agent is ready to receive requests.",
        HttpStatus.OK);
  }
}