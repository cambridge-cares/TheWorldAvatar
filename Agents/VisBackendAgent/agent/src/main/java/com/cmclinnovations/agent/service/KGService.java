package com.cmclinnovations.agent.service;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;

import com.cmclinnovations.agent.template.FormTemplateFactory;
import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;

@Service
public class KGService {
  @Value("${NAMESPACE}")
  String namespace;

  private final RestClient client;
  private final ObjectMapper objectMapper;
  private final FormTemplateFactory formTemplateFactory;

  private static final String LD_JSON_MEDIA_TYPE = "application/ld+json";
  private static final String SPARQL_MEDIA_TYPE = "application/sparql-query";

  private static final Logger LOGGER = LogManager.getLogger(KGService.class);

  /**
   * Constructs a new service.
   */
  public KGService() {
    this.client = RestClient.create();
    this.objectMapper = new ObjectMapper();
    this.formTemplateFactory = new FormTemplateFactory();
  }

  /**
   * Executes the query at the target endpoint to retrieve JSON LD results.
   * 
   * @param query the query for execution.
   * 
   * @return the query results as JSON array.
   */
  public ArrayNode queryJsonLd(String query) {
    String results = this.client.post()
        .uri(BlazegraphClient.getInstance().getRemoteStoreClient(this.namespace).getQueryEndpoint())
        .accept(MediaType.valueOf(LD_JSON_MEDIA_TYPE))
        .contentType(MediaType.valueOf(SPARQL_MEDIA_TYPE))
        .body(query)
        .retrieve()
        .body(String.class);
    ArrayNode parsedResults = null;
    try {
      parsedResults = this.objectMapper.readValue(results, ArrayNode.class);
    } catch (JsonProcessingException e) {
      LOGGER.error(e);
      throw new IllegalArgumentException(e);
    }
    return parsedResults;
  }

  /**
   * Queries for the form template.
   * 
   * @param query the query for execution.
   * @return the form template as a JSON object.
   */
  public Map<String, Object> queryForm(String query) {
    LOGGER.info("Querying the knowledge graph for the form template...");
    ArrayNode results = queryJsonLd(query);
    LOGGER.debug("Query is successfully executed. Parsing the results...");
    return this.formTemplateFactory.genTemplate(results);
  }
}