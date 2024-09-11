package com.cmclinnovations.agent.service;

import java.text.MessageFormat;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.SparqlBinding;

@Service
public class GetService {
  private final KGService kgService;
  private final FileService fileService;

  private static final Logger LOGGER = LogManager.getLogger(GetService.class);

  /**
   * Constructs a new service with the following dependencies.
   * 
   * @param kgService   KG service for performing the query.
   * @param fileService File service for accessing file resources.
   */
  public GetService(KGService kgService, FileService fileService) {
    this.kgService = kgService;
    this.fileService = fileService;
  }

  /**
   * Retrieve the form template for the target entity and its information.
   * 
   * @param targetType The target entity type.
   */
  public ResponseEntity<?> getForm(String targetType) {
    LOGGER.debug("Retrieving the form template for {} ...", targetType);
    ResponseEntity<String> iriResponse = this.getTargetIri(targetType);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    String query = this.fileService.getContentsWithReplacement(FileService.FORM_QUERY_RESOURCE, iriResponse.getBody());
    Map<String, Object> results = this.kgService.queryForm(query);
    if (results.isEmpty()) {
      LOGGER.error(
          "Invalid knowledge model! SHACL restrictions have not been defined/instantiated in the knowledge graph.");
      throw new IllegalStateException(
          "Invalid knowledge model! SHACL restrictions have not been defined/instantiated in the knowledge graph.");
    } else {
      LOGGER.info("Request has been completed successfully!");
      return new ResponseEntity<>(
          results,
          HttpStatus.OK);
    }
  }

  /**
   * Retrieve the instances for the target entity alongside its label and
   * description.
   * 
   * @param targetType The target entity type.
   */
  public ResponseEntity<?> getInstances(String targetType) {
    LOGGER.debug("Retrieving the instances for {} ...", targetType);
    ResponseEntity<String> iriResponse = this.getTargetIri(targetType);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    String query = this.fileService.getContentsWithReplacement(FileService.INSTANCE_QUERY_RESOURCE,
        iriResponse.getBody());
    List<SparqlBinding> results = this.kgService.query(query);
    if (results.isEmpty()) {
      LOGGER.info(
          "Request has been completed successfully with no results!");
    } else {
      LOGGER.info("Request has been completed successfully!");
    }
    return new ResponseEntity<>(
        results.stream()
            .map(SparqlBinding::get)
            .collect(Collectors.toList()),
        HttpStatus.OK);
  }

  /**
   * Gets the target IRI as a response entity if there is an associated identifier
   * in the file resource. This function also validates if the route is enabled
   * depending on if the user has set an identifier.
   * 
   * @param targetType The target class type.
   */
  private ResponseEntity<String> getTargetIri(String targetType) {
    LOGGER.debug("Retrieving the instances for {} ...", targetType);
    String targetClass = this.fileService.getTargetClass(targetType,
        FileService.SPRING_FILE_PATH_PREFIX + FileService.APPLICATION_FORM_RESOURCE);
    // Handle invalid target type
    if (targetClass.isEmpty()) {
      return new ResponseEntity<>(MessageFormat.format("Route is invalid at /{0}! If this route is intended to be enabled, please contact your technical team for assistance.", targetType),
          HttpStatus.BAD_REQUEST);
    }
    // For valid target type, return the associated target class
    return new ResponseEntity<>("<" + targetClass + ">", HttpStatus.OK);
  }
}