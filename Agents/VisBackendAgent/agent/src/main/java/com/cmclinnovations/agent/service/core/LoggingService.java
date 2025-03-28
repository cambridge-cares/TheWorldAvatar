package com.cmclinnovations.agent.service.core;

import java.text.MessageFormat;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

@Service
public class LoggingService {
  /**
   * Constructs a new service to log and throw any exception.
   */
  public LoggingService() {
    // No dependencies required
  }

  /**
   * Checks if an object node contains the target key. If not, log and throw
   * exception.
   * 
   * @param node   The target node.
   * @param key    The key name of interest.
   * @param objectName    The name of the object to be stated in the logging statement name of interest.
   * @param logger The logger associated with the respective class.
   */
  public void checkObjectKey(JsonNode node, String key, String objectName, Logger logger) throws JSONException {
    if (!node.has(key)) {
      logger.error("Missing {} key in {}!", key, objectName);
      throw new JSONException(MessageFormat.format("Missing {0} key in {1}!", key, objectName));
    }
  }
}