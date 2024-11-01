package com.cmclinnovations.agent.service;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.SparqlEndpointType;
import com.cmclinnovations.agent.utils.StringResource;

@Service
public class GetService {
  private final KGService kgService;
  private final FileService fileService;

  private static final String SUCCESSFUL_REQUEST_MSG = "Request has been completed successfully!";
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
   * Retrieve all the target instances and their information. This method can also
   * retrieve instances associated with a specific parent instance if declared.
   * 
   * @param resourceID       The target resource identifier for the instance
   *                         class.
   * @param parentInstanceId Optional parent instance identifier.
   * @param requireLabel     Indicates if labels should be returned for all the
   *                         fields that are IRIs.
   */
  public ResponseEntity<?> getAllInstances(String resourceID, String parentInstanceId, boolean requireLabel) {
    LOGGER.debug("Retrieving all instances of {} ...", resourceID);
    ResponseEntity<String> iriResponse = this.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    String queryPath = FileService.SHACL_PATH_QUERY_RESOURCE;
    String parentId = parentInstanceId;
    // If no parent instance ID is supplied, hasParent should be true
    boolean hasParent = parentInstanceId != null;
    if (requireLabel) {
      // Only use the label query if required due to the associated slower query
      // performance
      queryPath = FileService.SHACL_PATH_LABEL_QUERY_RESOURCE;
      // Parent related parameters should be disabled
      parentId = null;
      hasParent = false;
    }
    String query = this.fileService.getContentsWithReplacement(queryPath, iriResponse.getBody());
    Queue<SparqlBinding> results = this.kgService.queryInstances(query, parentId, hasParent);
    return new ResponseEntity<>(
        results.stream()
            .map(SparqlBinding::get)
            .collect(Collectors.toList()),
        HttpStatus.OK);
  }

  /**
   * Retrieve all target instances and their information in the CSV format.
   * 
   * @param resourceID The target resource identifier for the instance
   *                   class.
   */
  public ResponseEntity<String> getAllInstancesInCSV(String resourceID) {
    LOGGER.info("Retrieving all instances of {} in csv...", resourceID);
    ResponseEntity<String> iriResponse = this.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    String query = this.fileService.getContentsWithReplacement(FileService.SHACL_PATH_LABEL_QUERY_RESOURCE,
        iriResponse.getBody());
    return new ResponseEntity<>(
        this.kgService.queryInstancesInCsv(query),
        HttpStatus.OK);
  }

  /**
   * Retrieve only the specific instance and its information.
   * 
   * @param resourceID The target resource identifier for the instance class.
   * @param targetId   The target instance IRI.
   */
  public ResponseEntity<?> getInstance(String resourceID, String targetId) {
    LOGGER.debug("Retrieving an instance of {} ...", resourceID);
    ResponseEntity<String> iriResponse = this.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    String query = this.fileService.getContentsWithReplacement(FileService.SHACL_PATH_QUERY_RESOURCE,
        iriResponse.getBody());
    Queue<SparqlBinding> results = this.kgService.queryInstances(query, targetId, false);
    if (results.size() == 1) {
      return new ResponseEntity<>(
          results.poll().get(),
          HttpStatus.OK);
    } else if (results.isEmpty()) {
      return new ResponseEntity<>(
          "Invalid ID! There is no entity associated with this id in the knowledge graph.",
          HttpStatus.NOT_FOUND);
    } else {
      return new ResponseEntity<>(
          "Invalid knowledge model! Detected multiple entities with this id.",
          HttpStatus.CONFLICT);
    }
  }

  /**
   * Retrieve the form template for the target entity and its information.
   * 
   * @param resourceID The target resource identifier for the instance class.
   * @param targetId   The target instance IRI.
   */
  public ResponseEntity<?> getForm(String resourceID, String targetId) {
    LOGGER.debug("Retrieving the form template for {} ...", resourceID);
    ResponseEntity<String> iriResponse = this.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    Map<String, Object> currentEntity = new HashMap<>();
    if (targetId != null) {
      LOGGER.debug("Detected specific entity ID! Retrieving relevant entity information for {} ...", resourceID);
      ResponseEntity<?> currentEntityResponse = this.getInstance(resourceID, targetId);
      if (currentEntityResponse.getStatusCode() == HttpStatus.OK) {
        currentEntity = (Map<String, Object>) currentEntityResponse.getBody();
      }
    }
    String query = this.fileService.getContentsWithReplacement(FileService.FORM_QUERY_RESOURCE, iriResponse.getBody());
    Map<String, Object> results = this.kgService.queryForm(query, currentEntity);
    if (results.isEmpty()) {
      LOGGER.error(KGService.INVALID_SHACL_ERROR_MSG);
      throw new IllegalStateException(KGService.INVALID_SHACL_ERROR_MSG);
    } else {
      LOGGER.info(SUCCESSFUL_REQUEST_MSG);
      return new ResponseEntity<>(
          results,
          HttpStatus.OK);
    }
  }

  /**
   * Retrieve the metadata (IRI, label, and description) of the concept associated
   * with the target resource. This will return their current or sub-classes.
   * 
   * @param resourceID The target resource identifier for the instance class.
   */
  public ResponseEntity<?> getConceptMetadata(String resourceID) {
    LOGGER.debug("Retrieving the instances for {} ...", resourceID);
    ResponseEntity<String> iriResponse = this.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    String query = this.fileService.getContentsWithReplacement(FileService.INSTANCE_QUERY_RESOURCE,
        iriResponse.getBody());
    // Note that all concept metadata will never be stored in Ontop and will require
    // the special property paths
    Queue<SparqlBinding> results = this.kgService.query(query, SparqlEndpointType.BLAZEGRAPH);
    if (results.isEmpty()) {
      LOGGER.info(
          "Request has been completed successfully with no results!");
    } else {
      LOGGER.info(SUCCESSFUL_REQUEST_MSG);
    }
    return new ResponseEntity<>(
        results.stream()
            .map(SparqlBinding::get)
            .collect(Collectors.toList()),
        HttpStatus.OK);
  }

  /**
   * Retrieve the matching instances of the search criterias.
   * 
   * @param resourceID The target resource identifier for the instance class.
   * @param criterias  All the available search criteria inputs.
   */
  public ResponseEntity<?> getMatchingInstances(String resourceID, Map<String, String> criterias) {
    LOGGER.debug("Retrieving the form template for {} ...", resourceID);
    ResponseEntity<String> iriResponse = this.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    String query = this.fileService.getContentsWithReplacement(FileService.SHACL_PATH_QUERY_RESOURCE,
        iriResponse.getBody());
    Queue<SparqlBinding> results = this.kgService.queryInstancesWithCriteria(query, criterias);
    LOGGER.info(SUCCESSFUL_REQUEST_MSG);
    return new ResponseEntity<>(
        results.stream()
            .map(binding -> binding.getFieldValue("iri"))
            .collect(Collectors.toList()),
        HttpStatus.OK);
  }

  /**
   * Gets the target IRI as a response entity if there is an associated identifier
   * in the file resource. This function also validates if the route is enabled
   * depending on if the user has set an identifier.
   * 
   * @param resourceID The target resource identifier for the instance class.
   */
  private ResponseEntity<String> getTargetIri(String resourceID) {
    LOGGER.debug("Retrieving the target class associated with the resource identifier: {} ...", resourceID);
    String targetClass = this.fileService.getResourceTarget(resourceID,
        FileService.SPRING_FILE_PATH_PREFIX + FileService.APPLICATION_FORM_RESOURCE);
    // Handle invalid target type
    if (targetClass.isEmpty()) {
      return new ResponseEntity<>(MessageFormat.format(
          "Route is invalid at /{0}! If this route is intended to be enabled, please contact your technical team for assistance.",
          resourceID),
          HttpStatus.BAD_REQUEST);
    }
    // For valid target type, return the associated target class
    return new ResponseEntity<>(StringResource.parseIriForQuery(targetClass), HttpStatus.OK);
  }
}