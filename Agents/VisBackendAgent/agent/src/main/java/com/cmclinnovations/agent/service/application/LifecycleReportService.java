package com.cmclinnovations.agent.service.application;

import java.text.MessageFormat;
import java.util.ArrayDeque;
import java.util.Map;
import java.util.Queue;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.type.CalculationType;
import com.cmclinnovations.agent.service.core.DateTimeService;
import com.cmclinnovations.agent.service.core.JsonLdService;
import com.cmclinnovations.agent.service.core.LoggingService;
import com.cmclinnovations.agent.utils.LifecycleResource;
import com.cmclinnovations.agent.utils.ShaclResource;
import com.cmclinnovations.agent.utils.StringResource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

@Service
public class LifecycleReportService {
  private final CalculationService calculationService;
  private final DateTimeService dateTimeService;
  private final JsonLdService jsonLdService;
  private final LoggingService loggingService;
  private final ObjectMapper objectMapper;

  private static final String LIFECYCLE_RECORD_PREFIX = "https://www.theworldavatar.io/kg/lifecycle/record/";

  private static final Logger LOGGER = LogManager.getLogger(LifecycleReportService.class);

  /**
   * Constructs a new service to interact with JSON-LD objects with the following
   * dependencies.
   * 
   * @param calculationService A service to perform any calculations.
   * @param dateTimeService    A service to handle date and times.
   * @param jsonLdService      A service to handle all JSON-LD related
   *                           transformation.
   * @param loggingService     A service to handle all reusable logging
   *                           statements.
   * @param objectMapper       The JSON object mapper.
   */
  public LifecycleReportService(CalculationService calculationService, DateTimeService dateTimeService,
      JsonLdService jsonLdService, LoggingService loggingService, ObjectMapper objectMapper) {
    this.calculationService = calculationService;
    this.dateTimeService = dateTimeService;
    this.jsonLdService = jsonLdService;
    this.loggingService = loggingService;
    this.objectMapper = objectMapper;
  }

  /**
   * Appends a calculation and record to the parent node, which should be a
   * reverse node.
   * 
   * @param parentNode        Parent node holding the calculation object.
   * @param calculationObject Target JSON configuration object.
   * @param params            Mappings of the parameters and their values.
   */
  public void appendCalculationRecord(ObjectNode parentNode, ObjectNode calculationObject,
      Map<String, Object> params) {
    ObjectNode calculationInstance = this.genCalculationInstance(calculationObject,
        params);
    parentNode.set(LifecycleResource.SUCCEEDS_RELATIONS, calculationInstance);
    // Generate record instance
    ObjectNode recordInstance = this.genRecordInstance();
    // Retrieve and associate output value in calculation
    recordInstance.set(LifecycleResource.RECORDS_RELATIONS,
        calculationInstance.get(LifecycleResource.HAS_QTY_VAL_RELATIONS));
    // Parent node should be a reverse node
    parentNode.set(LifecycleResource.IS_ABOUT_RELATIONS, recordInstance);
  }

  /**
   * Generates a record instance.
   */
  private ObjectNode genRecordInstance() {
    return this.jsonLdService.genInstance(LIFECYCLE_RECORD_PREFIX, LifecycleResource.LIFECYCLE_RECORD);
  }

  /**
   * Generates a calculation instance from the custom input configs.
   * 
   * @param calculationObject Target JSON configuration object.
   * @param params            Mappings of the parameters and their values.
   */
  private ObjectNode genCalculationInstance(ObjectNode calculationObject, Map<String, Object> params) {
    String stage = params.get(LifecycleResource.STAGE_KEY).toString();
    String prefix = StringResource.getPrefix(stage) + "/record/";
    LOGGER.info("Validating the inputs...");
    calculationObject = this.validateCalculationNodeInput(calculationObject);
    LOGGER.info("Calculating the output value...");
    double outputValue = this.calculationService.calculate(calculationObject, params);
    LOGGER.info("Generating template for calculation...");
    String calculationType = calculationObject.get(ShaclResource.TYPE_KEY).asText().toUpperCase();
    ObjectNode occurrence = this.genCalculationTemplate(prefix, params);
    this.appendCalculationExpression(occurrence, prefix, calculationObject.get(ShaclResource.VARIABLE_KEY).deepCopy(),
        params, CalculationType.valueOf(calculationType));
    this.appendCalculationOutput(occurrence, prefix, calculationObject.get(ShaclResource.OUTPUT_KEY).deepCopy(),
        outputValue);
    return occurrence;
  }

  /**
   * Parses the calculation node input to ensure it conforms to format.
   * 
   * @param node The target node.
   */
  private ObjectNode validateCalculationNodeInput(ObjectNode node) {
    // Validate calculation type
    this.loggingService.checkObjectKey(node, ShaclResource.TYPE_KEY, "calculation object", LOGGER);

    // Validate output field
    this.loggingService.checkObjectKey(node, ShaclResource.OUTPUT_KEY, "calculation object", LOGGER);
    ObjectNode outputNode = this.parseScalarQuantityNode(node.get(ShaclResource.OUTPUT_KEY));
    node.set(ShaclResource.OUTPUT_KEY, outputNode);

    // Validate variable field
    this.loggingService.checkObjectKey(node, ShaclResource.VARIABLE_KEY, "calculation object", LOGGER);
    if (node.get(ShaclResource.VARIABLE_KEY).isArray()) {
      ArrayNode variableNodes = this.objectMapper.createArrayNode();
      for (JsonNode quantity : node.get(ShaclResource.VARIABLE_KEY)) {
        ObjectNode variableNode = this.parseScalarQuantityNode(quantity);
        variableNodes.add(variableNode);
      }
      node.set(ShaclResource.VARIABLE_KEY, variableNodes);
    }
    return node;
  }

  /**
   * Parses the scalar quantity node to ensure it conforms to format.
   * 
   * @param scalarQuantity The target node.
   */
  private ObjectNode parseScalarQuantityNode(JsonNode scalarQuantity) {
    this.loggingService.checkObjectKey(scalarQuantity, ShaclResource.ID_KEY, "scalar quantity defined", LOGGER);
    LOGGER.debug("Parsing scalar quantity for {}...", scalarQuantity.get(ShaclResource.ID_KEY));
    ObjectNode result = scalarQuantity.deepCopy();
    if (!scalarQuantity.has(ShaclResource.TYPE_KEY)) {
      LOGGER.debug("No @type field detected! Defaulting to default value...");
      result.put(ShaclResource.TYPE_KEY,
          "https://www.omg.org/spec/Commons/QuantitiesAndUnits/ScalarQuantityValue");
    }

    if (!scalarQuantity.has(ShaclResource.UNIT_KEY)) {
      LOGGER.debug("No unit field detected! Defaulting to default value...");
      result.put(ShaclResource.UNIT_KEY,
          "https://www.omg.org/spec/Commons/QuantitiesAndUnits/MeasurementUnit");
    }
    return result;
  }

  /**
   * Generates an calculation instance template in JSON-LD format.
   * 
   * @param params Mappings containing values of interest.
   */
  private ObjectNode genCalculationTemplate(String prefix, Map<String, Object> params) {
    ObjectNode occurrence = this.objectMapper.createObjectNode();
    occurrence.put(ShaclResource.ID_KEY, prefix + UUID.randomUUID());
    occurrence.put(ShaclResource.TYPE_KEY,
        "https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/Calculation");
    // Set event date time
    occurrence.set("https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/hasEventDate",
        this.jsonLdService.genLiteral(this.dateTimeService.getCurrentDateTime(),
            "http://www.w3.org/2001/XMLSchema#dateTime"));

    // Set exemplifies relation
    ObjectNode exemplifiesNode = objectMapper.createObjectNode();
    exemplifiesNode.put(ShaclResource.ID_KEY,
        "https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/CalculationEvent");
    occurrence.set(LifecycleResource.EXEMPLIFIES_RELATIONS, exemplifiesNode);

    // Set the parent stage relation
    ObjectNode reverseNode = objectMapper.createObjectNode();
    ObjectNode comprisesNode = objectMapper.createObjectNode();
    comprisesNode.put(ShaclResource.ID_KEY, params.get(LifecycleResource.STAGE_KEY).toString());
    reverseNode.set("https://www.omg.org/spec/Commons/Collections/comprises", comprisesNode);
    occurrence.set(ShaclResource.REVERSE_KEY, reverseNode);
    return occurrence;
  }

  /**
   * Appends an instance of the calculation expression as a JSON-LD node to the
   * occurrence.
   * 
   * @param occurrence              The target occurrence object node to be
   *                                appended to.
   * @param prefix                  The prefix for an IRI.
   * @param variableQuantityConfigs An array of the variable quantity
   *                                configuration in JSON containing @id, @type,
   *                                and unit keys.
   * @param params                  Mappings containing values of interest.
   * @param calculationType         The type of calculation required.
   */
  private ObjectNode appendCalculationExpression(ObjectNode occurrence, String prefix,
      ArrayNode variableQuantityConfigs, Map<String, Object> params, CalculationType calculationType) {
    ObjectNode expressionNode = objectMapper.createObjectNode();
    expressionNode.put(ShaclResource.ID_KEY, prefix + "expression/" + UUID.randomUUID());
    expressionNode.put(ShaclResource.TYPE_KEY, LifecycleResource.getExpressionClass(calculationType));

    for (JsonNode config : variableQuantityConfigs) {
      String paramName = config.get(ShaclResource.ID_KEY).asText();
      double value = Double.parseDouble(params.get(paramName).toString());
      ObjectNode currentQuantity = this.genScalarQuantityNode(prefix, config.deepCopy(), value);
      switch (calculationType) {
        case CalculationType.DIFFERENCE:
          if (expressionNode.has(LifecycleResource.HAS_MINUEND_RELATIONS)) {
            expressionNode.set(LifecycleResource.HAS_SUBTRAHEND_RELATIONS, currentQuantity);
          } else {
            expressionNode.set(LifecycleResource.HAS_MINUEND_RELATIONS, currentQuantity);
          }
          break;
        case CalculationType.TOTAL:
          expressionNode.set(LifecycleResource.HAS_ARGUMENT_RELATIONS, currentQuantity);
          break;
        default:
          LOGGER.warn("Invalid Calculation Type!");
          break;
      }
    }
    occurrence.set("https://www.omg.org/spec/Commons/QuantitiesAndUnits/hasExpression", expressionNode);
    return occurrence;
  }

  /**
   * Appends an instance of the calculation output as a JSON-LD node to the
   * occurrence.
   * 
   * @param occurrence           The target occurrence object node to be appended
   *                             to.
   * @param prefix               The prefix for an IRI.
   * @param outputQuantityConfig The scalar quantity configuration in JSON
   *                             containing @id, @type, and unit keys.
   * @param value                The value of the quantity.
   */
  private ObjectNode appendCalculationOutput(ObjectNode occurrence, String prefix, ObjectNode outputQuantityConfig,
      double value) {
    ObjectNode outputQuantity = this.genScalarQuantityNode(prefix, outputQuantityConfig, value);
    occurrence.set(LifecycleResource.HAS_QTY_VAL_RELATIONS, outputQuantity);
    return occurrence;
  }

  /**
   * Generates a scalar quantity node in JSON-LD format.
   * 
   * @param prefix               The prefix for an IRI.
   * @param scalarQuantityConfig The sclar quantity configuration in JSON
   *                             containing @id, @type, and unit keys.
   * @param value                The value of the quantity.
   */
  private ObjectNode genScalarQuantityNode(String prefix, ObjectNode scalarQuantityConfig, double value) {
    ObjectNode quantity = this.objectMapper.createObjectNode();
    quantity.put(ShaclResource.ID_KEY, prefix + "quantity/" + UUID.randomUUID());
    quantity.put(ShaclResource.TYPE_KEY, scalarQuantityConfig.get(ShaclResource.TYPE_KEY).asText());
    quantity.set("https://www.omg.org/spec/Commons/QuantitiesAndUnits/hasMeasurementUnit",
        this.objectMapper.createObjectNode()
            .put(ShaclResource.ID_KEY, scalarQuantityConfig.get(ShaclResource.UNIT_KEY).asText()));
    quantity.set("https://www.omg.org/spec/Commons/QuantitiesAndUnits/hasNumericValue",
        this.jsonLdService.genLiteral(String.valueOf(value), "http://www.w3.org/2001/XMLSchema#decimal"));
    return quantity;
  }
}
