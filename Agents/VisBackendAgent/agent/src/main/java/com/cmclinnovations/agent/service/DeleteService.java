package com.cmclinnovations.agent.service;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.response.ApiResponse;
import com.cmclinnovations.agent.utils.LifecycleResource;
import com.cmclinnovations.agent.utils.ShaclResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

@Service
public class DeleteService {
  private final KGService kgService;
  private final FileService fileService;

  private static final Logger LOGGER = LogManager.getLogger(DeleteService.class);

  /**
   * Constructs a new service with the following dependencies.
   * 
   * @param kgService   KG service for performing the query.
   * @param fileService File service for accessing file resources.
   */
  public DeleteService(KGService kgService, FileService fileService) {
    this.kgService = kgService;
    this.fileService = fileService;
  }

  /**
   * Delete the instance associated with the target identifier.
   * 
   * @param resourceID The target resource identifier for the instance.
   * @param targetId   The target instance IRI.
   */
  public ResponseEntity<ApiResponse> delete(String resourceID, String targetId) {
    LOGGER.debug("Deleting {} instance of {} ...", resourceID, targetId);
    String filePath = LifecycleResource.getLifecycleResourceFilePath(resourceID);
    // Default to the file name in application-service if it not a lifecycle route
    if (filePath == null) {
      ResponseEntity<String> fileNameResponse = this.fileService.getTargetFileName(resourceID);
      // Return the BAD REQUEST response directly if the file is invalid
      if (fileNameResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
        return new ResponseEntity<>(
            new ApiResponse(fileNameResponse),
            fileNameResponse.getStatusCode());
      }
      filePath = FileService.SPRING_FILE_PATH_PREFIX + FileService.JSON_LD_DIR + fileNameResponse.getBody() + ".jsonld";
    }
    // Retrieve the instantiation JSON schema
    JsonNode addJsonSchema = this.fileService.getJsonContents(filePath);
    if (!addJsonSchema.isObject()) {
      LOGGER.info("Invalid JSON-LD format! Please ensure the file starts with an JSON object.");
      return new ResponseEntity<>(
          new ApiResponse("Invalid JSON-LD format! Please contact your technical team for assistance."),
          HttpStatus.INTERNAL_SERVER_ERROR);
    }
    String instanceIri = addJsonSchema.path(ShaclResource.ID_KEY).asText();
    ResponseEntity<String> response = this.kgService.delete((ObjectNode) addJsonSchema, targetId);
    return new ResponseEntity<>(
        new ApiResponse(response.getBody(), instanceIri),
        response.getStatusCode());
  }
}
