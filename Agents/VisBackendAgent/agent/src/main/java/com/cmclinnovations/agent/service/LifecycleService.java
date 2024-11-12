package com.cmclinnovations.agent.service;

import java.util.Map;
import java.util.Queue;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.type.LifecycleEventType;
import com.cmclinnovations.agent.model.type.SparqlEndpointType;
import com.cmclinnovations.agent.utils.LifecycleResource;
import com.cmclinnovations.agent.utils.StringResource;

@Service
public class LifecycleService {
  private final KGService kgService;

  private static final Logger LOGGER = LogManager.getLogger(LifecycleService.class);

  /**
   * Constructs a new service with the following dependencies.
   * 
   * @param kgService KG service for performing the query.
   */
  public LifecycleService(KGService kgService) {
    this.kgService = kgService;
  }

  /**
   * Add the required event instance into the parameters.
   * 
   * @param params    The target parameters to update.
   * @param eventType The target event type to retrieve.
   */
  public void addEventInstance(Map<String, Object> params, LifecycleEventType eventType) {
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    String event = this.getEventInstance(contractId, eventType);
    params.put(LifecycleResource.EVENT_KEY, event);
  }

  /**
   * Populate the remaining occurrence parameters into the request parameters.
   * 
   * @param params    The target parameters to update.
   * @param eventType The target event type to retrieve.
   */
  public void addOccurrenceParams(Map<String, Object> params, LifecycleEventType eventType) {
    String contractId = params.get(LifecycleResource.CONTRACT_KEY).toString();
    String event = this.getEventInstance(contractId, eventType);
    params.put("id", StringResource.getPrefix(event) + "/occurrence/" + UUID.randomUUID());
    params.put(LifecycleResource.EVENT_KEY, event);
    // Only retrieve the current date if no date input is given
    params.putIfAbsent(LifecycleResource.DATE_KEY, LifecycleResource.getCurrentDate());
  }

  /**
   * Retrieve the event instance associated with the target contract.
   * 
   * @param contractId The identifier for the target contract.
   * @param eventType  The target event type to retrieve.
   */
  public String getEventInstance(String contractId, LifecycleEventType eventType) {
    LOGGER.debug("Retrieving event instance for {}...", contractId);
    String query = "PREFIX fibo-fnd-arr-lif: <https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/>" +
        "PREFIX cmns-col: <https://www.omg.org/spec/Commons/Collections/>" +
        "SELECT DISTINCT ?event WHERE {" +
        "?lifecycle fibo-fnd-arr-lif:isLifecycleOf <" + contractId + ">;" +
        "fibo-fnd-arr-lif:hasStage/cmns-col:comprises ?event." +
        "?event rdf:type <" + LifecycleResource.getEventClass(eventType) + ">" +
        "}";
    Queue<SparqlBinding> results = this.kgService.query(query, SparqlEndpointType.BLAZEGRAPH);
    if (results.size() == 1) {
      return results.poll().getFieldValue("event");
    } else if (results.isEmpty()) {
      LOGGER.error("No valid event instance found!");
      throw new NullPointerException("No valid event instance found!");
    } else {
      LOGGER.error("Detected multiple instances: Data model is invalid!");
      throw new IllegalStateException("Detected multiple instances: Data model is invalid!");
    }
  }
}