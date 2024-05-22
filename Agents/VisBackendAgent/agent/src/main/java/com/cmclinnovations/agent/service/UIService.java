package com.cmclinnovations.agent.service;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.ControlsResource;
import com.cmclinnovations.agent.model.InputResource;
import com.cmclinnovations.agent.model.UIResource;
import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;

/**
 * Service class for loading and parsing UI component configurations from the
 * controls.json.
 */
@Service
public class UIService {
  private final ObjectMapper objectMapper;
  private final ResourceLoader resourceLoader;
  private static final Logger LOGGER = LogManager.getLogger(UIService.class);

  /**
   * Constructs a new UIControlService instance.
   * 
   * @param resourceLoader ResourceLoader instance for loading file resources.
   * @param objectMapper   ObjectMapper instance for JSON parsing.
   */
  public UIService(ResourceLoader resourceLoader, ObjectMapper objectMapper) {
    this.resourceLoader = resourceLoader;
    this.objectMapper = objectMapper;
  }

  /**
   * Retrieves the UI component configurations based on the provided identifier.
   * 
   * @param target the target resource.
   * @return the JSON string representing all required UI component
   *         configurations.
   */
  public String getControls(String target) throws IOException {
    ControlsResource targetResource = this.findTargetResource(target);
    List<UIResource> controlConfigs = this.loadConfig(targetResource);
    return this.getControlConfigs(controlConfigs);
  }

  /**
   * Finds the target resource set in the controls.json.
   * 
   * @param target the target resource.
   * @return the target resource data.
   */
  private ControlsResource findTargetResource(String target) throws IOException {
    LOGGER.info("Finding target {} in controls.json...", target);
    Resource resource = this.resourceLoader.getResource(StringResource.CONFIG_DIR + "controls.json");
    try (InputStream inputStream = resource.getInputStream()) {
      JsonNode resourceNode = this.objectMapper.readTree(inputStream).path(target);
      if (resourceNode.isMissingNode()) {
        LOGGER.error("Missing node! Please ensure you have included the identifier for {} in controls.json.",
            target);
        throw new IllegalArgumentException(
            "Missing node! Please ensure you have included the identifier for " + target + " in controls.json.");
      }
      return this.objectMapper.treeToValue(resourceNode, ControlsResource.class);
    } catch (IOException e) {
      LOGGER.info(e);
      throw e;
    }
  }

  /**
   * Load the target resource's config into the required mappings.
   * 
   * @param targetResource the target resource configuration.
   * @return the list of UI element resources.
   */
  private List<UIResource> loadConfig(ControlsResource targetResource)
      throws IOException {
    // Assumes file will always be in the `CONFIG_DIR` directory
    Resource resource = this.resourceLoader.getResource(StringResource.CONFIG_DIR + targetResource.url());
    try (InputStream inputStream = resource.getInputStream()) {
      JsonNode uiNodes = this.objectMapper.readTree(inputStream);

      String targetResourceType = targetResource.type();
      LOGGER.info("Loading configurations for {}...", targetResourceType);
      if (targetResourceType.equals("filter")) {
        return loadFilterConfig(uiNodes);
      } else {
        LOGGER.error("Invalid type: {} . Only filter is permitted!", targetResource.type());
        throw new IllegalArgumentException(
            "Invalid type: " + targetResource.type() + " . Only filter is permitted!");
      }
    }
  }

  /**
   * Load the filter panel configuration based on the target resource's config.
   * 
   * @param uiNodes the array of UI elements' configuration.
   * @return the list of UI element resources.
   */
  private List<UIResource> loadFilterConfig(JsonNode uiNodes) {
    List<UIResource> uiResources = new ArrayList<>();
    if (uiNodes.isArray()) {
      // Iterate each JSON node in the resource and append their configuration
      for (JsonNode uiNode : uiNodes) {
        String type = uiNode.path("type").asText();
        String label = uiNode.path("label").asText();
        LOGGER.debug("Loading initial configuration for {} of type {}...", label, type);
        UIResource currentResource;
        switch (type) {
          case InputResource.CONFIG_TYPE:
            currentResource = new InputResource(label);
            break;
          default:
            LOGGER.error("Invalid type: {} . Only input is permitted!", type);
            throw new IllegalArgumentException(
                "Invalid type: " + type + " . Only input is permitted!");
        }
        // Parse the query field with its own array
        JsonNode queryNodes = uiNode.path("query");
        for (JsonNode queryNode : queryNodes) {
          currentResource.addQueryConfig(queryNode);
        }
        uiResources.add(currentResource);
      }
    } else {
      LOGGER.error("Invalid JSON format. It must be an array!");
      throw new IllegalArgumentException("Invalid JSON format. It must be an array!");
    }
    return uiResources;
  }

  /**
   * Get the required control configurations based on the configuration loaded.
   * 
   * @param uiResources required configuration for retrieving the UI control
   *                    configs.
   * @return the JSON string representing all required UI component
   *         configurations.
   */
  private String getControlConfigs(List<UIResource> uiResources) throws IOException {
    LOGGER.info("Retrieving UI control configurations...");
    ArrayNode output = this.objectMapper.createArrayNode();
    for (UIResource resource : uiResources) {
      LOGGER.debug("Getting {} configurations...", resource.getLabel());
      switch (resource.getType()) {
        case InputResource.CONFIG_TYPE:
          output.add(resource.toJsonResponse());
          break;
        default:
          LOGGER.error("Invalid type: {} . Only input is permitted!", resource.getType());
          throw new IllegalArgumentException(
              "Invalid type: " + resource.getType() + " . Only input is permitted!");
      }

    }
    // Transform the output into string
    try {
      return this.objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(output);
    } catch (Exception e) {
      e.printStackTrace();
      return "[]"; // Return empty JSON array if there's an error
    }
  }
}
