package com.cmclinnovations.agent.service;

import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.Map;
import java.util.Queue;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.response.ApiResponse;
import com.cmclinnovations.agent.service.application.LifecycleReportService;
import com.cmclinnovations.agent.service.core.FileService;
import com.cmclinnovations.agent.service.core.JsonLdService;
import com.cmclinnovations.agent.service.core.KGService;
import com.cmclinnovations.agent.utils.LifecycleResource;
import com.cmclinnovations.agent.utils.ShaclResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

@Service
public class AddService {
  private final KGService kgService;
  private final FileService fileService;
  private final JsonLdService jsonLdService;
  private final LifecycleReportService lifecycleReportService;
  private final ObjectMapper objectMapper;

  private static final Logger LOGGER = LogManager.getLogger(AddService.class);

  /**
   * Constructs a new service with the following dependencies.
   * 
   * @param kgService              KG service for performing the query.
   * @param fileService            File service for accessing file resources.
   * @param jsonLdService          A service for interactions with JSON LD.
   * @param lifecycleReportService A service for reporting lifecycle matters such
   *                               as calculation instances.
   * @param objectMapper           The JSON object mapper.
   */
  public AddService(KGService kgService, FileService fileService, JsonLdService jsonLdService,
      LifecycleReportService lifecycleReportService, ObjectMapper objectMapper) {
    this.kgService = kgService;
    this.fileService = fileService;
    this.jsonLdService = jsonLdService;
    this.lifecycleReportService = lifecycleReportService;
    this.objectMapper = objectMapper;
  }

  /**
   * Overloaded method to instantiate the target instance following the input
   * parameters. ID field will default to a random UUID if no id parameter is
   * sent.
   * 
   * @param resourceID The target resource identifier for the instance.
   * @param param      Request parameters.
   */
  public ResponseEntity<ApiResponse> instantiate(String resourceID, Map<String, Object> param) {
    String id = param.getOrDefault("id", UUID.randomUUID()).toString();
    return instantiate(resourceID, id, param);
  }

  /**
   * Instantiates the target instance following the input parameters and the
   * target ID.
   * 
   * @param resourceID The target resource identifier for the instance.
   * @param targetId   The target instance IRI.
   * @param param      Request parameters.
   */
  public ResponseEntity<ApiResponse> instantiate(String resourceID, String targetId,
      Map<String, Object> param) {
    LOGGER.info("Instantiating an instance of {} ...", resourceID);
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
    // Update ID value to target ID
    param.put("id", targetId);
    // Retrieve the instantiation JSON schema
    JsonNode addJsonSchema = this.fileService.getJsonContents(filePath);
    // Attempt to replace all placeholders in the JSON schema
    if (addJsonSchema.isObject()) {
      this.recursiveReplacePlaceholders((ObjectNode) addJsonSchema, null, null, param);
    } else {
      LOGGER.info("Invalid JSON-LD format for replacement!");
      return new ResponseEntity<>(
          new ApiResponse("Invalid JSON-LD format for replacement! Please contact your technical team for assistance."),
          HttpStatus.INTERNAL_SERVER_ERROR);
    }

    return this.instantiateJsonLd(addJsonSchema, resourceID + " has been successfully instantiated!");
  }

  /**
   * Instantiate an instance based on a jsonLD object.
   * 
   * @param jsonLdSchema The target json LD object to instantiate.
   * @param message      Successful message.
   */
  public ResponseEntity<ApiResponse> instantiateJsonLd(JsonNode jsonLdSchema, String message) {
    LOGGER.debug("Adding instance to endpoint...");
    String instanceIri = jsonLdSchema.path(ShaclResource.ID_KEY).asText();
    String jsonString = jsonLdSchema.toString();
    ResponseEntity<String> response = this.kgService.add(jsonString);
    if (response.getStatusCode() == HttpStatus.OK) {
      LOGGER.info(message);
      return new ResponseEntity<>(
          new ApiResponse(message, instanceIri),
          HttpStatus.CREATED);
    } else {
      return new ResponseEntity<>(
          new ApiResponse(response),
          response.getStatusCode());
    }
  }

  /**
   * Replace the placeholders in the current node and recursively for its children
   * nodes based on the corresponding value in the replacement mappings if
   * available.
   * 
   * @param currentNode  Input contents to perform operation on.
   * @param parentNode   Parent node holding the current node, which must be an
   *                     object node. Arrays should be excluded.
   * @param parentField  Field name of the parent containing the current node.
   * @param replacements Mappings of the replacement value with their
   *                     corresponding node.
   */
  private void recursiveReplacePlaceholders(ObjectNode currentNode, ObjectNode parentNode, String parentField,
      Map<String, Object> replacements) {
    // If the current node is a replacement object, replace current node with
    // replacement value
    if (currentNode.has(ShaclResource.REPLACE_KEY)) {
      if (parentNode != null) {
        // Add a different interaction for schedule types
        if (currentNode.path(ShaclResource.TYPE_KEY).asText().equals("schedule")) {
          this.replaceDayOfWeekSchedule(parentNode, parentField, replacements);
          // Add a different interaction for calculations
        } else if (currentNode.path(ShaclResource.REPLACE_KEY).asText().equals("calculation")) {
          this.lifecycleReportService.appendCalculationRecord(parentNode, currentNode, replacements);
          // Parse literal with data types differently
        } else if (currentNode.path(ShaclResource.TYPE_KEY).asText().equals("literal")
            && currentNode.has(ShaclResource.DATA_TYPE_PROPERTY)) {
          ObjectNode literalNode = this.jsonLdService.genLiteral(
              this.jsonLdService.getReplacementValue(currentNode, replacements),
              currentNode.path(ShaclResource.DATA_TYPE_PROPERTY).asText());
          parentNode.set(parentField, literalNode);
          // IRIs that are not assigned to @id or @type should belong within a nested @id
          // object
        } else if (currentNode.path(ShaclResource.TYPE_KEY).asText().equals("iri")
            && !(parentField.equals(ShaclResource.ID_KEY) || parentField.equals(ShaclResource.TYPE_KEY))) {
          ObjectNode newIriNode = this.objectMapper.createObjectNode();
          newIriNode.put(ShaclResource.ID_KEY, this.jsonLdService.getReplacementValue(currentNode, replacements));
          parentNode.set(parentField, newIriNode);
        } else {
          // For IRIs and literal with no other pattern, simply replace the value
          parentNode.put(parentField, this.jsonLdService.getReplacementValue(currentNode, replacements));
        }
      } else {
        LOGGER.error("Invalid parent node for replacement!");
        throw new IllegalArgumentException("Invalid parent node for replacement!");
      }
    } else {
      // Else recursively go deeper into the JSON object to find other replacements
      Iterator<String> fieldNames = currentNode.fieldNames();
      while (fieldNames.hasNext()) {
        String fieldName = fieldNames.next();
        JsonNode childNode = currentNode.get(fieldName);
        // If the child node is an object, recurse deeper
        if (childNode.isObject()) {
          recursiveReplacePlaceholders((ObjectNode) childNode, currentNode, fieldName, replacements);
        } else if (childNode.isArray()) {
          // If the child node contains an array, recursively parse through each object
          ArrayNode childrenNodes = (ArrayNode) childNode;
          for (int i = 0; i < childrenNodes.size(); i++) {
            // Assumes that the nodes in the array are object node
            recursiveReplacePlaceholders((ObjectNode) childrenNodes.get(i), currentNode, fieldName, replacements);
          }
        }
      }
    }
  }

  /**
   * Replaces the json model for the day of week schedules depending on the
   * indication in the request parameters.
   * 
   * @param parentNode   Parent node holding the day of week schedule
   *                     representation.
   * @param parentField  Field name of the parent containing the current node.
   * @param replacements Mappings of the replacement value with their
   *                     corresponding node.
   */
  private void replaceDayOfWeekSchedule(ObjectNode parentNode, String parentField, Map<String, Object> replacements) {
    // Note that this method assumes that the explicit recurrence interval will
    // always contain an array of items
    ArrayNode results = this.objectMapper.createArrayNode(); // Empty array to store values
    // First iterate through the schedule array and retrieve all items that are not
    // the replacement object
    JsonNode scheduleNode = parentNode.path(parentField);
    if (scheduleNode.isArray()) {
      ArrayNode nodes = (ArrayNode) scheduleNode;
      for (int i = 0; i < nodes.size(); i++) {
        JsonNode currentScheduleNode = nodes.get(i);
        if (currentScheduleNode.isObject() && !((ObjectNode) currentScheduleNode).has(ShaclResource.REPLACE_KEY)) {
          results.add(currentScheduleNode);
        }
      }
    }

    // Generate a queue of days of week
    Queue<String> daysOfWeek = new ArrayDeque<>();
    daysOfWeek.offer("Monday");
    daysOfWeek.offer("Tuesday");
    daysOfWeek.offer("Wednesday");
    daysOfWeek.offer("Thursday");
    daysOfWeek.offer("Friday");
    daysOfWeek.offer("Saturday");
    daysOfWeek.offer("Sunday");

    // Iterate over the queue
    while (!daysOfWeek.isEmpty()) {
      String currentDay = daysOfWeek.poll();
      // Parameter name is in lowercase based on the frontend
      if (replacements.containsKey(currentDay.toLowerCase()) && (boolean) replacements.get(currentDay.toLowerCase())) {
        // Only include the selected day if it has been selected on the frontend
        ObjectNode currentDayNode = this.objectMapper.createObjectNode();
        currentDayNode.put(ShaclResource.ID_KEY,
            "https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/"
                + currentDay + "");
        results.add(currentDayNode);
      }
    }
    parentNode.set(parentField, results);
  }
}
