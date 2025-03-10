package com.cmclinnovations.agent.service;

import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.ParentField;
import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.type.SparqlEndpointType;
import com.cmclinnovations.agent.service.core.FileService;
import com.cmclinnovations.agent.service.core.KGService;
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
   * @param resourceID   The target resource identifier for the instance
   *                     class.
   * @param parentField  Optional parent field containing its id and name.
   * @param requireLabel Indicates if labels should be returned for all the
   *                     fields that are IRIs.
   */
  public ResponseEntity<?> getAllInstances(String resourceID, ParentField parentField, boolean requireLabel) {
    LOGGER.debug("Retrieving all instances of {} ...", resourceID);
    ResponseEntity<String> iriResponse = this.fileService.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    String queryPath = FileService.SHACL_PATH_QUERY_RESOURCE;
    if (requireLabel) {
      // Only use the label query if required due to the associated slower query
      // performance
      queryPath = FileService.SHACL_PATH_LABEL_QUERY_RESOURCE;
      // Parent related parameters should be disabled
      parentField = null;
    }
    String query = this.fileService.getContentsWithReplacement(queryPath, iriResponse.getBody());
    Queue<SparqlBinding> results = this.kgService.queryInstances(query, parentField);
    return new ResponseEntity<>(
        results.stream()
            .map(SparqlBinding::get)
            .toList(),
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
    ResponseEntity<String> iriResponse = this.fileService.getTargetIri(resourceID);
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
   * Retrieve only the specific instance and its information. This overloaded
   * method will retrieve the replacement value required from the resource ID.
   * 
   * @param resourceID   The target resource identifier for the instance class.
   * @param targetId     The target instance IRI.
   * @param requireLabel Indicates if labels should be returned for all the
   *                     fields that are IRIs.
   */
  public ResponseEntity<?> getInstance(String resourceID, String targetId, boolean requireLabel) {
    ResponseEntity<String> iriResponse = this.fileService.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    return getInstance(resourceID, targetId, iriResponse.getBody(), requireLabel);
  }

  /**
   * Retrieve only the specific instance and its information.
   * 
   * @param resourceID   The target resource identifier for the instance class.
   * @param targetId     The target instance IRI.
   * @param replacement  The replacement value required.
   * @param requireLabel Indicates if labels should be returned for all the
   *                     fields that are IRIs.
   */
  public ResponseEntity<?> getInstance(String resourceID, String targetId, String replacement, boolean requireLabel) {
    LOGGER.debug("Retrieving an instance of {} ...", resourceID);
    String queryPath = requireLabel ? FileService.SHACL_PATH_LABEL_QUERY_RESOURCE
        : FileService.SHACL_PATH_QUERY_RESOURCE;
    String query = this.fileService.getContentsWithReplacement(queryPath, replacement);
    Queue<SparqlBinding> results = this.kgService.queryInstances(query, targetId);
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
   * Retrieve only the specific instance based on the query. The query must have
   * iri as its variable.
   * 
   * @param query Query for execution.
   */
  public SparqlBinding getInstance(String query) {
    LOGGER.debug("Retrieving an instance...");
    Queue<SparqlBinding> results = this.kgService.query(query, SparqlEndpointType.BLAZEGRAPH);
    return this.kgService.getSingleInstance(results);
  }

  /**
   * Retrieve the form template for the target entity and its information.
   * 
   * @param resourceID The target resource identifier for the instance class.
   * @param targetId   The target instance IRI.
   */
  public ResponseEntity<?> getForm(String resourceID, String targetId) {
    LOGGER.debug("Retrieving the form template for {} ...", resourceID);
    ResponseEntity<String> iriResponse = this.fileService.getTargetIri(resourceID);
    // Return the BAD REQUEST response directly if IRI is invalid
    if (iriResponse.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
      return iriResponse;
    }
    Map<String, Object> currentEntity = new HashMap<>();
    if (targetId != null) {
      LOGGER.debug("Detected specific entity ID! Retrieving relevant entity information for {} ...", resourceID);
      ResponseEntity<?> currentEntityResponse = this.getInstance(resourceID, targetId, false);
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
   * @param conceptClass The target class details to retrieved.
   */
  public ResponseEntity<?> getConceptMetadata(String conceptClass) {
    LOGGER.debug("Retrieving the instances for {} ...", conceptClass);
    String query = this.fileService.getContentsWithReplacement(FileService.INSTANCE_QUERY_RESOURCE,
        StringResource.parseIriForQuery(conceptClass));
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
            .toList(),
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
    ResponseEntity<String> iriResponse = this.fileService.getTargetIri(resourceID);
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
            .toList(),
        HttpStatus.OK);
  }
}