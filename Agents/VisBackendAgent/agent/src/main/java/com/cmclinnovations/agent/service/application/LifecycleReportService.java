package com.cmclinnovations.agent.service.application;

import java.util.List;
import java.util.Map;
import java.util.Queue;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.model.response.ApiResponse;
import com.cmclinnovations.agent.model.type.CalculationType;
import com.cmclinnovations.agent.model.type.SparqlEndpointType;
import com.cmclinnovations.agent.service.GetService;
import com.cmclinnovations.agent.service.core.DateTimeService;
import com.cmclinnovations.agent.service.core.JsonLdService;
import com.cmclinnovations.agent.service.core.LoggingService;
import com.cmclinnovations.agent.template.LifecycleQueryFactory;
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
  private final GetService getService;
  private final JsonLdService jsonLdService;
  private final LoggingService loggingService;
  private final ObjectMapper objectMapper;
  private final LifecycleQueryFactory queryFactory;

  private static final String FLAT_FEE_LABEL = "Base Fee";
  private static final String UNIT_PRICE_LABEL = "unit price";
  private static final String RATE_LABEL = "rate";
  private static final String LOWER_BOUND_LABEL = "lowerBound";
  private static final String UPPER_BOUND_LABEL = "upperBound";

  private static final String PRICING_MODEL_PREFIX = "https://www.theworldavatar.io/kg/agreement/pricing/";
  private static final String MONETARY_PRICE_PREFIX = "https://www.theworldavatar.io/kg/agreement/money/";
  private static final String VARIABLE_FEE_PREFIX = "https://www.theworldavatar.io/kg/agreement/variable/money/";
  private static final String LIFECYCLE_REPORT_PREFIX = "https://www.theworldavatar.io/kg/lifecycle/report/";
  private static final String LIFECYCLE_RECORD_PREFIX = "https://www.theworldavatar.io/kg/lifecycle/record/";

  private static final Logger LOGGER = LogManager.getLogger(LifecycleReportService.class);

  /**
   * Constructs a new service to interact with JSON-LD objects with the following
   * dependencies.
   * 
   * @param calculationService A service to perform any calculations.
   * @param dateTimeService    A service to handle date and times.
   * @param getService         A service to retrieve information from the KG.
   * @param jsonLdService      A service to handle all JSON-LD related
   *                           transformation.
   * @param loggingService     A service to handle all reusable logging
   *                           statements.
   * @param objectMapper       The JSON object mapper.
   */
  public LifecycleReportService(CalculationService calculationService, DateTimeService dateTimeService,
      GetService getService, JsonLdService jsonLdService, LoggingService loggingService, ObjectMapper objectMapper) {
    this.calculationService = calculationService;
    this.dateTimeService = dateTimeService;
    this.getService = getService;
    this.jsonLdService = jsonLdService;
    this.loggingService = loggingService;
    this.objectMapper = objectMapper;
    this.queryFactory = new LifecycleQueryFactory();
  }

  /**
   * Generates a report instance.
   * 
   * @param contract The subject contract instance of interest to report on.
   */
  public ObjectNode genReportInstance(String contract) {
    ObjectNode report = this.jsonLdService.genInstance(LIFECYCLE_REPORT_PREFIX, LifecycleResource.LIFECYCLE_REPORT);
    ObjectNode contractNode = this.objectMapper.createObjectNode().put(ShaclResource.ID_KEY, contract);
    report.set(LifecycleResource.IS_ABOUT_RELATIONS, contractNode);
    return report;
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
    ObjectNode recordInstance = this.jsonLdService.genInstance(LIFECYCLE_RECORD_PREFIX,
        LifecycleResource.LIFECYCLE_RECORD);
    // Retrieve and associate output value in calculation
    recordInstance.set(LifecycleResource.RECORDS_RELATIONS,
        calculationInstance.get(LifecycleResource.HAS_QTY_VAL_RELATIONS));
    // Retrieve report instance and attach record to report
    String query = this.queryFactory.getReportQuery(params.get(LifecycleResource.STAGE_KEY).toString());
    String report = this.getService.getInstance(query).getFieldValue(LifecycleResource.IRI_KEY);
    ObjectNode reportsOnNode = this.objectMapper.createObjectNode()
        .set(LifecycleResource.REPORTS_ON_RELATIONS,
            this.objectMapper.createObjectNode().put(ShaclResource.ID_KEY, report));
    recordInstance.set(ShaclResource.REVERSE_KEY, reportsOnNode);
    // Parent node should be a reverse node
    parentNode.set(LifecycleResource.IS_ABOUT_RELATIONS, recordInstance);
  }

  /**
   * Generates a pricing model JSON node.
   * 
   * @param params Mappings of the parameters and their values.
   */
  public ObjectNode genPricingModel(Map<String, Object> params) {
    ArrayNode arguments = this.objectMapper.createArrayNode();
    ObjectNode pricingModel = this.jsonLdService.genInstance(PRICING_MODEL_PREFIX, LifecycleResource.PRICING_MODEL);
    ObjectNode flatFee = this.jsonLdService.genInstance(MONETARY_PRICE_PREFIX, LifecycleResource.MONETARY_PRICE,
        FLAT_FEE_LABEL);
    flatFee.put(LifecycleResource.HAS_AMOUNT_RELATIONS,
        Double.parseDouble(params.get(FLAT_FEE_LABEL.toLowerCase()).toString()));
    arguments.add(flatFee);
    if (params.containsKey(UNIT_PRICE_LABEL)) {
      List<Map<String, Object>> unitPrices = (List<Map<String, Object>>) params.get(UNIT_PRICE_LABEL);
      for (Map<String, Object> unitPrice : unitPrices) {
        ObjectNode currentNode = this.genUnitPrice(unitPrice);
        arguments.add(currentNode);
      }
    }
    pricingModel.set(LifecycleResource.HAS_ARGUMENT_RELATIONS, arguments);
    return pricingModel;
  }

  /**
   * Retrieves the pricing model set for the target contract if it exist.
   * 
   * @param contract The ID of the target contract.
   */
  public SparqlBinding getPricingModel(String contract) {
    LOGGER.debug("Checking for an existing pricing model...");
    String query = this.queryFactory.getPricingModelQuery(contract);
    try {
      // If there is a pricing model set, no exception is thrown
      return this.getService.getInstance(query);
    } catch (IllegalStateException | NullPointerException e) {
      // When unset, an error is returned but it should be ignored and return an empty
      // object
      LOGGER.error("Invalid pricing model state! Read error message for more details:", e);
      return new SparqlBinding(this.objectMapper.createObjectNode());
    }
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
    ObjectNode occurrence = this.jsonLdService.genInstance(prefix,
        "https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/Calculation");
    // Set event date time
    occurrence.set("https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/hasEventDate",
        this.jsonLdService.genLiteral(this.dateTimeService.getCurrentDateTime(),
            ShaclResource.XSD_DATE_TIME));

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
    ObjectNode expressionNode = this.jsonLdService.genInstance(prefix + "expression/",
        LifecycleResource.getExpressionClass(calculationType));
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
    ObjectNode quantity = this.jsonLdService.genInstance(prefix + "quantity/",
        scalarQuantityConfig.get(ShaclResource.TYPE_KEY).asText());
    quantity.set("https://www.omg.org/spec/Commons/QuantitiesAndUnits/hasMeasurementUnit",
        this.objectMapper.createObjectNode()
            .put(ShaclResource.ID_KEY, scalarQuantityConfig.get(ShaclResource.UNIT_KEY).asText()));
    quantity.set("https://www.omg.org/spec/Commons/QuantitiesAndUnits/hasNumericValue",
        this.jsonLdService.genLiteral(String.valueOf(value), ShaclResource.XSD_DECIMAL));
    return quantity;
  }

  /**
   * Generates a unit price instance based on the input parameters.
   *
   * @param params Mappings of the required parameters and their values.
   */
  private ObjectNode genUnitPrice(Map<String, Object> params) {
    ObjectNode unitPriceNode = this.jsonLdService.genInstance(VARIABLE_FEE_PREFIX,
        LifecycleResource.VARIABLE_FEE);
    unitPriceNode.set(LifecycleResource.HAS_AMOUNT_RELATIONS,
        this.jsonLdService.genLiteral(params.get(RATE_LABEL).toString(), ShaclResource.XSD_DECIMAL));
    unitPriceNode.set(LifecycleResource.HAS_LOWER_BOUND_RELATIONS,
        this.jsonLdService.genLiteral(params.get(LOWER_BOUND_LABEL).toString(), ShaclResource.XSD_DECIMAL));
    try {
      unitPriceNode.set(LifecycleResource.HAS_UPPER_BOUND_RELATIONS,
          this.jsonLdService.genLiteral(params.get(UPPER_BOUND_LABEL).toString(), ShaclResource.XSD_DECIMAL));
    } catch (NumberFormatException e) {
      LOGGER.warn("Detected empty value for the optional upper bound. Please notify the team if this is incorrect!");
    }
    return unitPriceNode;
  }
}
