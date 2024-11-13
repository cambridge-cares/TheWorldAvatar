package com.cmclinnovations.agent;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.cmclinnovations.agent.model.response.ApiResponse;
import com.cmclinnovations.agent.service.AddService;
import com.cmclinnovations.agent.service.DeleteService;
import com.cmclinnovations.agent.service.GeocodingService;
import com.cmclinnovations.agent.service.GetService;

@RestController
public class VisBackendAgent {
  private final AddService addService;
  private final DeleteService deleteService;
  private final GetService getService;
  private final GeocodingService geocodingService;
  private static final Logger LOGGER = LogManager.getLogger(VisBackendAgent.class);

  public VisBackendAgent(AddService addService, DeleteService deleteService, GetService getService,
      GeocodingService geocodingService) {
    this.addService = addService;
    this.deleteService = deleteService;
    this.getService = getService;
    this.geocodingService = geocodingService;
  }

  @GetMapping("/status")
  public ResponseEntity<String> getStatus() {
    LOGGER.info("Detected request to get agent status...");
    return new ResponseEntity<>(
        "Agent is ready to receive requests.",
        HttpStatus.OK);
  }

  @GetMapping("/location/geocode")
  public ResponseEntity<?> getGeoCoordinates(
      @RequestParam(required = false) String block,
      @RequestParam(required = false) String street,
      @RequestParam(required = false) String city,
      @RequestParam(required = false) String country,
      @RequestParam(required = false) String postal_code) {
    LOGGER.info("Received geocoding request...");
    if (block != null && street == null) {
      String errorMsg = "Invalid geocoding parameters! Detected a block number but no street is provided!";
      LOGGER.error(errorMsg);
      return new ResponseEntity<>(errorMsg, HttpStatus.BAD_REQUEST);
    }

    return this.geocodingService.getCoordinates(block, street, city, country, postal_code);
  }

  @GetMapping("/location/addresses")
  public ResponseEntity<?> getAddress(@RequestParam(required = true) String postal_code) {
    LOGGER.info("Received request to search for address...");
    return this.geocodingService.getAddress(postal_code);
  }

  /**
   * Retrieves all instances belonging to the specified type in the knowledge
   * graph.
   */
  @GetMapping("/{type}")
  public ResponseEntity<?> getAllInstances(
      @PathVariable(name = "type") String type) {
    LOGGER.info("Received request to get all instances for {}...", type);
    // This route does not require further restriction on parent instances
    return this.getService.getAllInstances(type, null, false);
  }

  /**
   * Retrieves all instances belonging to the specified type in the knowledge
   * graph, and include human readable labels for all properties.
   */
  @GetMapping("/{type}/label")
  public ResponseEntity<?> getAllInstancesWithLabel(
      @PathVariable(name = "type") String type) {
    LOGGER.info("Received request to get all instances with labels for {}...", type);
    // This route does not require further restriction on parent instances
    return this.getService.getAllInstances(type, null, true);
  }

  /**
   * Retrieves all instances belonging to the specified type in the knowledge
   * graph.
   */
  @GetMapping("/{parent}/{id}/{type}")
  public ResponseEntity<?> getAllInstancesWithParent(@PathVariable(name = "parent") String parent,
      @PathVariable(name = "id") String id,
      @PathVariable(name = "type") String type) {
    LOGGER.info("Received request to get all instances of target {} associated with the parent type {}...", type,
        parent);
    return this.getService.getAllInstances(type, id, false);
  }

  /**
   * Retrieve the target instance of the specified type in the knowledge graph.
   */
  @GetMapping("/{type}/{id}")
  public ResponseEntity<?> getInstance(@PathVariable String type, @PathVariable String id) {
    LOGGER.info("Received request to get a specific instance of {}...", type);
    return this.getService.getInstance(type, id);
  }

  /**
   * Retrieve the instances that matches the search criterias.
   */
  @PostMapping("/{type}/search")
  public ResponseEntity<?> getMatchingInstances(@PathVariable String type, @RequestBody Map<String, String> criterias) {
    LOGGER.info("Received request to get matching instances of {}...", type);
    return this.getService.getMatchingInstances(type, criterias);
  }

  /**
   * Retrieves all instances belonging to the specified type in the knowledge
   * graph in the csv format.
   */
  @GetMapping("/csv/{type}")
  public ResponseEntity<String> getAllInstancesInCSV(@PathVariable(name = "type") String type) {
    LOGGER.info("Received request to get all instances of {} type in the CSV format...", type);
    return this.getService.getAllInstancesInCSV(type);
  }

  /**
   * Retrieves the form template for the specified type from the knowledge graph.
   */
  @GetMapping("/form/{type}")
  public ResponseEntity<?> getFormTemplate(@PathVariable(name = "type") String type) {
    LOGGER.info("Received request to get the form template for {}...", type);
    return this.getService.getForm(type, null);
  }

  /**
   * Retrieves the form template for the target entity of the specified type from
   * the knowledge graph.
   */
  @GetMapping("/form/{type}/{id}")
  public ResponseEntity<?> retrieveFormTemplate(@PathVariable String type, @PathVariable String id) {
    LOGGER.info("Received request to get specific form template for {} ...", type);
    return this.getService.getForm(type, id);
  }

  /**
   * Retrieve the metadata (IRI, label, and description) of the concept associated
   * with the specified type in the knowledge graph.
   */
  @GetMapping("/type/{type}")
  public ResponseEntity<?> getConceptMetadata(@PathVariable(name = "type") String type) {
    LOGGER.info("Received request to get the metadata for the concept: {}...", type);
    return this.getService.getConceptMetadata(type);
  }

  /**
   * Instantiates a new instance in the knowledge graph.
   */
  @PostMapping("/{type}")
  public ResponseEntity<ApiResponse> addInstance(@PathVariable String type,
      @RequestBody Map<String, Object> instance) {
    LOGGER.info("Received request to add one {}...", type);
    return this.addService.instantiate(type, instance);
  }

  /**
   * Removes the specified instance from the knowledge graph.
   */
  @DeleteMapping("/{type}/{id}")
  public ResponseEntity<ApiResponse> removeEntity(@PathVariable String type, @PathVariable String id) {
    LOGGER.info("Received request to delete {}...", type);
    return this.deleteService.delete(type, id);
  }

  /**
   * Update the target instance in the knowledge graph.
   */
  @PutMapping("/{type}/{id}")
  public ResponseEntity<ApiResponse> updateEntity(@PathVariable String type, @PathVariable String id,
      @RequestBody Map<String, Object> updatedEntity) {
    LOGGER.info("Received request to update {}...", type);
    ResponseEntity<ApiResponse> deleteResponse = this.deleteService.delete(type, id);
    if (deleteResponse.getStatusCode().equals(HttpStatus.OK)) {
      ResponseEntity<ApiResponse> addResponse = this.addService.instantiate(type, id, updatedEntity);
      if (addResponse.getStatusCode() == HttpStatus.OK) {
        LOGGER.info("{} has been successfully updated for {}", type, id);
        return new ResponseEntity<>(
            new ApiResponse(type + " has been successfully updated for " + id,
                addResponse.getBody().getIri()),
            HttpStatus.CREATED);
      } else {
        return addResponse;
      }
    } else {
      return deleteResponse;
    }
  }
}