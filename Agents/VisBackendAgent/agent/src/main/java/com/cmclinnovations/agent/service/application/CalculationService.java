package com.cmclinnovations.agent.service.application;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.type.CalculationType;
import com.cmclinnovations.agent.utils.ShaclResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

@Service
public class CalculationService {
  private static final Logger LOGGER = LogManager.getLogger(CalculationService.class);

  /**
   * Constructs a new service for performing calculations.
   */
  public CalculationService() {
    // No dependencies required
  }

  /**
   * Performs a calculation based on the object and parameters
   * 
   * @param calculationObject The target calculation configuration.
   * @param params            The mappings containing the values of the required
   *                          variables.
   */
  public double calculate(ObjectNode calculationObject, Map<String, Object> params) {
    String calculationType = calculationObject.get(ShaclResource.TYPE_KEY).asText().toUpperCase();
    ArrayNode variables = calculationObject.get(ShaclResource.VARIABLE_KEY).deepCopy();
    switch (CalculationType.valueOf(calculationType)) {
      case CalculationType.DIFFERENCE:
        return this.computeDifference(variables, params);
      case CalculationType.TOTAL:
        return this.computeTotal(variables, params);
      default:
        LOGGER.error("Invalid Type: Calculation type is invalid in @type key!");
        throw new IllegalArgumentException("Invalid Type: Calculation type is invalid in @type key!");
    }
  }

  /**
   * Add all the values of the variables together to get the total value.
   * 
   * @param variables The name of variables for computation.
   * @param params    The mappings containing the values of the required
   *                  variables.
   */
  private double computeTotal(ArrayNode variables, Map<String, Object> params) {
    LOGGER.info("Computing the total value...");
    double total = 0.000;
    for (JsonNode quantity : variables) {
      String quantityName = quantity.get(ShaclResource.ID_KEY).asText();
      double value = Double.parseDouble(params.get(quantityName).toString());
      total += value;
    }
    return total;
  }

  /**
   * Deduct the second and subsequent values from the first variable's value to
   * find the difference.
   * 
   * @param variables The name of variables for computation.
   * @param params    The mappings containing the values of the required
   *                  variables.
   */
  private double computeDifference(ArrayNode variables, Map<String, Object> params) {
    LOGGER.info("Computing the difference in values...");
    double difference = Double.NaN;
    for (JsonNode quantity : variables) {
      String quantityName = quantity.get(ShaclResource.ID_KEY).asText();
      double value = Double.parseDouble(params.get(quantityName).toString());
      // First value should be the subject for further subtraction
      if (Double.isNaN(difference)) {
        difference = value;
      } else {
        difference -= value;
      }
    }
    return difference;
  }
}
