package com.cmclinnovations.agent.service;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public class GetService {
  private final KGService kgService;
  private final FileService fileService;

  private static final Logger LOGGER = LogManager.getLogger(GetService.class);

  /**
   * Constructs a new service with the following dependencies.
   * 
   * @param resourceLoader ResourceLoader instance for loading file resources.
   * @param kgService      KG service for performing the query.
   */
  public GetService(KGService kgService, FileService fileService) {
    this.kgService = kgService;
    this.fileService = fileService;
  }

  /**
   * Retrieve the form template for the target entity and its information.
   * 
   * @param targetType The target entity type. Valid types include bin, client,
   *                   contract, and employee.
   * @param targetId   The target entity identifier.
   */
  public ResponseEntity<?> getForm(String targetType) {
    LOGGER.debug("Retrieving the form template for {} ...", targetType);
    String replacementIri = "<"
        + this.fileService.getTargetClass(targetType,
            FileService.SPRING_FILE_PATH_PREFIX + FileService.APPLICATION_FORM_RESOURCE)
        + ">";
    String query = this.fileService.getContentsWithReplacement(FileService.FORM_QUERY_RESOURCE, replacementIri);
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
}