package com.cmclinnovations.agent.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.Before;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.ResourceLoader;

import com.cmclinnovations.agent.TestUtils;
import com.cmclinnovations.agent.model.DropdownResource;
import com.cmclinnovations.agent.model.DropdownResourceTest;
import com.cmclinnovations.agent.model.InputResource;
import com.cmclinnovations.agent.model.InputResourceTest;
import com.cmclinnovations.agent.repo.KGRepository;
import com.cmclinnovations.agent.service.testdouble.FakeKGRepository;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class UIServiceTest {
  private KGRepository fakeKGService;
  private UIService subject;
  private File controls;

  private static final String SAMPLE_RESOURCE_IDENTIFIER = "resource";
  private static final ResourceLoader LOADER = new DefaultResourceLoader();
  private static final ObjectMapper MAPPER = new ObjectMapper();

  @BeforeEach
  void setUp() {
    this.fakeKGService = new FakeKGRepository();
    this.subject = new UIService(LOADER, MAPPER, this.fakeKGService);
    this.controls = genControlsJson();
  }

  @AfterEach
  void cleanUp() {
    this.controls.delete();
  }

  @Test
  void testGetControls_InputResource() throws IOException {
    // Arrange
    File resource = genInputJson(SAMPLE_RESOURCE_IDENTIFIER);
    try {
      // Act
      String results = this.subject.getControls(SAMPLE_RESOURCE_IDENTIFIER);

      // Assert
      JsonNode resultNode = MAPPER.readTree(results);
      assertEquals("filter", resultNode.path("type").asText());

      JsonNode configNodes = resultNode.path("config");
      assertTrue(configNodes.isArray());
      assertEquals(1, configNodes.size());
      // Verify resource node is expected
      JsonNode inputNode = configNodes.get(0);
      assertEquals(InputResourceTest.SAMPLE_LABEL, inputNode.path("label").asText());
      assertEquals(InputResource.CONFIG_TYPE, inputNode.path("type").asText());
      assertTrue(inputNode.path("options").isArray());
      assertEquals(1, inputNode.path("options").size());
    } finally {
      resource.delete();
    }
  }

  @Test
  void testGetControls_DropdownResource() throws IOException {
    // Arrange
    File resource = genDropdownJson(SAMPLE_RESOURCE_IDENTIFIER);
    File sparqlResource = TestUtils
        .genSparqlFile("/usr/local/tomcat/resources/query/" + DropdownResourceTest.SAMPLE_FILE_LOCATION, "");

    try {
      // Act
      String results = this.subject.getControls(SAMPLE_RESOURCE_IDENTIFIER);

      // Assert
      JsonNode resultNode = MAPPER.readTree(results);
      assertEquals("filter", resultNode.path("type").asText());

      JsonNode configNodes = resultNode.path("config");
      assertTrue(configNodes.isArray());
      assertEquals(1, configNodes.size());
      // Verify resource node is expected
      JsonNode dropdownNode = configNodes.get(0);
      assertEquals(DropdownResourceTest.SAMPLE_LABEL, dropdownNode.path("label").asText());
      assertEquals(DropdownResourceTest.SAMPLE_IS_REQUIRED, dropdownNode.path("isRequired").asBoolean());
      assertEquals(DropdownResource.CONFIG_TYPE, dropdownNode.path("type").asText());
      assertTrue(dropdownNode.path("options").isArray());
      assertEquals(1, dropdownNode.path("options").size());
    } finally {
      resource.delete();
      sparqlResource.delete();
    }
  }

  private static File genControlsJson() {
    ObjectNode rootNode = MAPPER.createObjectNode();
    ObjectNode resourceNode = MAPPER.createObjectNode();
    resourceNode.put("url", SAMPLE_RESOURCE_IDENTIFIER + ".json");
    resourceNode.put("type", "filter");
    rootNode.set(SAMPLE_RESOURCE_IDENTIFIER, resourceNode);
    return TestUtils.genJsonFile("/usr/local/tomcat/resources/config/controls.json", rootNode, MAPPER);
  }

  private static File genInputJson(String resourceName) {
    ArrayNode uiNodes = MAPPER.createArrayNode();
    ObjectNode inputNode = MAPPER.createObjectNode();
    ArrayNode queryNodes = MAPPER.createArrayNode();
    ObjectNode queryNode = MAPPER.createObjectNode();
    queryNode.put("label", InputResourceTest.SAMPLE_INPUT_LABEL);
    queryNode.put("filter", InputResourceTest.SAMPLE_INPUT_FILTER);
    queryNode.put("type", InputResourceTest.SAMPLE_INPUT_TYPE);
    queryNodes.add(queryNode);
    inputNode.put("label", InputResourceTest.SAMPLE_LABEL);
    inputNode.put("type", InputResource.CONFIG_TYPE);
    inputNode.set("query", queryNodes);
    uiNodes.add(inputNode);
    return TestUtils.genJsonFile("/usr/local/tomcat/resources/config/" + resourceName + ".json", uiNodes, MAPPER);
  }

  private static File genDropdownJson(String resourceName) {
    ArrayNode uiNodes = MAPPER.createArrayNode();
    ObjectNode inputNode = MAPPER.createObjectNode();
    ArrayNode queryNodes = MAPPER.createArrayNode();
    ObjectNode queryNode = MAPPER.createObjectNode();
    queryNode.put("namespace", DropdownResourceTest.SAMPLE_NAMESPACE);
    queryNode.put("file", DropdownResourceTest.SAMPLE_FILE_LOCATION);
    queryNodes.add(queryNode);
    inputNode.put("label", DropdownResourceTest.SAMPLE_LABEL);
    inputNode.put("type", DropdownResource.CONFIG_TYPE);
    inputNode.put("required", DropdownResourceTest.SAMPLE_IS_REQUIRED);
    inputNode.set("query", queryNodes);
    uiNodes.add(inputNode);
    return TestUtils.genJsonFile("/usr/local/tomcat/resources/config/" + resourceName + ".json", uiNodes, MAPPER);
  }
}
