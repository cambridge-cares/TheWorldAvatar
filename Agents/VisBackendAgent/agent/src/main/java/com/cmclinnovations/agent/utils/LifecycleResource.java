package com.cmclinnovations.agent.utils;

import java.time.LocalDate;

import com.cmclinnovations.agent.model.type.CalculationType;
import com.cmclinnovations.agent.model.type.LifecycleEventType;
import com.cmclinnovations.agent.service.core.FileService;

public class LifecycleResource {
  public static final String LIFECYCLE_RESOURCE = "lifecycle";
  public static final String SCHEDULE_RESOURCE = "schedule";
  public static final String OCCURRENCE_INSTANT_RESOURCE = "occurrence instant";
  public static final String PRICING_RESOURCE = "pricing";

  public static final String IRI_KEY = "iri";
  public static final String CONTRACT_KEY = "contract";
  public static final String ORDER_KEY = "order";
  public static final String CURRENT_DATE_KEY = "current date";
  public static final String DATE_KEY = "date";
  public static final String DATE_TIME_KEY = "dateTime";
  public static final String EVENT_KEY = "event";
  public static final String STAGE_KEY = "stage";
  public static final String STATUS_KEY = "status";
  public static final String REMARKS_KEY = "remarks";
  public static final String TIMESTAMP_KEY = "timestamp";
  public static final String SCHEDULE_DURATION_KEY = "duration";
  public static final String SCHEDULE_DAY_KEY = "scheduleday";
  public static final String SCHEDULE_START_DATE_KEY = "start date";
  public static final String SCHEDULE_END_DATE_KEY = "end date";
  public static final String SCHEDULE_START_TIME_KEY = "start time";
  public static final String SCHEDULE_END_TIME_KEY = "end time";
  public static final String SCHEDULE_RECURRENCE_KEY = "recurrence";
  public static final String SCHEDULE_TYPE_KEY = "schedule type";

  public static final String EXEMPLIFIES_RELATIONS = "https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/exemplifies";
  public static final String HAS_AMOUNT_RELATIONS = "https://spec.edmcouncil.org/fibo/ontology/FND/Accounting/CurrencyAmount/hasAmount";
  public static final String HAS_LOWER_BOUND_RELATIONS = "https://www.omg.org/spec/Commons/QuantitiesAndUnits/hasLowerBound";
  public static final String HAS_UPPER_BOUND_RELATIONS = "https://www.omg.org/spec/Commons/QuantitiesAndUnits/hasUpperBound";
  public static final String HAS_ARGUMENT_RELATIONS = "https://www.omg.org/spec/Commons/QuantitiesAndUnits/hasArgument";
  public static final String HAS_MINUEND_RELATIONS = "https://spec.edmcouncil.org/fibo/ontology/FND/Utilities/Analytics/hasMinuend";
  public static final String HAS_SUBTRAHEND_RELATIONS = "https://spec.edmcouncil.org/fibo/ontology/FND/Utilities/Analytics/hasSubtrahend";
  public static final String HAS_QTY_VAL_RELATIONS = "https://www.omg.org/spec/Commons/QuantitiesAndUnits/hasQuantityValue";
  public static final String IS_ABOUT_RELATIONS = "https://www.omg.org/spec/Commons/Documents/isAbout";
  public static final String REPORTS_ON_RELATIONS = "https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Reporting/reportsOn";
  public static final String RECORDS_RELATIONS = "https://www.omg.org/spec/Commons/Documents/records";
  public static final String SUCCEEDS_RELATIONS = "https://www.omg.org/spec/Commons/DatesAndTimes/succeeds";

  public static final String LIFECYCLE_STAGE_PREDICATE_PATH = "<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/hasLifecycle>/<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/hasStage>";
  public static final String LIFECYCLE_STAGE_EVENT_PREDICATE_PATH = "<https://www.omg.org/spec/Commons/Collections/comprises>";
  public static final String LIFECYCLE_EVENT_TYPE_PREDICATE_PATH = "<" + EXEMPLIFIES_RELATIONS + ">";
  public static final String LIFECYCLE_EVENT_PREDICATE_PATH = LIFECYCLE_STAGE_PREDICATE_PATH + "/"
      + LIFECYCLE_STAGE_EVENT_PREDICATE_PATH + "/" + LIFECYCLE_EVENT_TYPE_PREDICATE_PATH;
  public static final String EVENT_APPROVAL = "https://www.theworldavatar.com/kg/ontoservice/ContractApproval";
  public static final String EVENT_ORDER_RECEIVED = "https://www.theworldavatar.com/kg/ontoservice/OrderReceivedEvent";
  public static final String EVENT_DISPATCH = "https://www.theworldavatar.com/kg/ontoservice/ServiceDispatchEvent";
  public static final String EVENT_DELIVERY = "https://www.theworldavatar.com/kg/ontoservice/ServiceDeliveryEvent";
  public static final String EVENT_CANCELLATION = "https://www.theworldavatar.com/kg/ontoservice/TerminatedServiceEvent";
  public static final String EVENT_INCIDENT_REPORT = "https://www.theworldavatar.com/kg/ontoservice/IncidentReportEvent";
  public static final String EVENT_CONTRACT_COMPLETION = "https://www.theworldavatar.com/kg/ontoservice/ContractDischarge";
  public static final String EVENT_CONTRACT_RESCISSION = "https://www.theworldavatar.com/kg/ontoservice/ContractRescission";
  public static final String EVENT_CONTRACT_TERMINATION = "https://www.theworldavatar.com/kg/ontoservice/ContractTermination";
  public static final String LIFECYCLE_RECORD = "https://www.omg.org/spec/Commons/Documents/Record";
  public static final String LIFECYCLE_REPORT = "https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Reporting/Report";
  public static final String PAYMENT_OBLIGATION = "https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/PaymentsAndSchedules/PaymentObligation";
  public static final String PRICING_MODEL = "https://spec.edmcouncil.org/fibo/ontology/FBC/FinancialInstruments/InstrumentPricing/PricingModel";
  public static final String MONETARY_PRICE = "https://spec.edmcouncil.org/fibo/ontology/FND/Accounting/CurrencyAmount/MonetaryPrice";
  public static final String VARIABLE_FEE = "https://www.theworldavatar.com/kg/ontoservice/VariableFee";

  // Private constructor to prevent instantiation
  private LifecycleResource() {
    throw new UnsupportedOperationException("This class cannot be instantiated!");
  }

  /**
   * Check if the date input is either before and after the current date.
   * 
   * @param dateParam   The date parameter for checking.
   * @param checkBefore Indicator if the method should check if the date is before
   *                    the current date. Use false to check if date is after the
   *                    current date.
   */
  public static boolean checkDate(String dateParam, boolean checkBefore) {
    // Parse input date
    LocalDate inputDate = LocalDate.parse(dateParam);
    LocalDate currentDate = LocalDate.now();

    if (checkBefore) {
      return inputDate.isBefore(currentDate); // Check if the date is before today
    } else {
      return inputDate.isAfter(currentDate); // Check if the date is after today
    }
  }

  /**
   * Retrieve the stage class associated with the event type.
   * 
   * @param eventType The target event type.
   */
  public static String getStageClass(LifecycleEventType eventType) {
    switch (eventType) {
      case LifecycleEventType.APPROVED:
        return "https://www.theworldavatar.com/kg/ontoservice/CreationStage";
      case LifecycleEventType.SERVICE_ORDER_RECEIVED:
      case LifecycleEventType.SERVICE_ORDER_DISPATCHED:
      case LifecycleEventType.SERVICE_EXECUTION:
      case LifecycleEventType.SERVICE_CANCELLATION:
      case LifecycleEventType.SERVICE_INCIDENT_REPORT:
        return "https://www.theworldavatar.com/kg/ontoservice/ServiceExecutionStage";
      case LifecycleEventType.ARCHIVE_COMPLETION:
      case LifecycleEventType.ARCHIVE_RESCINDMENT:
      case LifecycleEventType.ARCHIVE_TERMINATION:
        return "https://www.theworldavatar.com/kg/ontoservice/ExpirationStage";
      default:
        throw new IllegalArgumentException("Invalid event type!");
    }
  }

  /**
   * Retrieve the event class associated with the event type.
   * 
   * @param eventType The target event type.
   */
  public static String getEventClass(LifecycleEventType eventType) {
    switch (eventType) {
      case LifecycleEventType.APPROVED:
        return EVENT_APPROVAL;
      case LifecycleEventType.SERVICE_ORDER_RECEIVED:
        return EVENT_ORDER_RECEIVED;
      case LifecycleEventType.SERVICE_ORDER_DISPATCHED:
        return EVENT_DISPATCH;
      case LifecycleEventType.SERVICE_EXECUTION:
        return EVENT_DELIVERY;
      case LifecycleEventType.SERVICE_CANCELLATION:
        return EVENT_CANCELLATION;
      case LifecycleEventType.SERVICE_INCIDENT_REPORT:
        return EVENT_INCIDENT_REPORT;
      case LifecycleEventType.ARCHIVE_COMPLETION:
        return EVENT_CONTRACT_COMPLETION;
      case LifecycleEventType.ARCHIVE_RESCINDMENT:
        return EVENT_CONTRACT_RESCISSION;
      case LifecycleEventType.ARCHIVE_TERMINATION:
        return EVENT_CONTRACT_TERMINATION;
      default:
        throw new IllegalArgumentException("Invalid event type!");
    }
  }

  /**
   * Retrieve the event identifier associated with the event type.
   * 
   * @param eventType The target event type.
   */
  public static String getEventIdentifier(LifecycleEventType eventType) {
    switch (eventType) {
      case LifecycleEventType.APPROVED:
        return "approve";
      case LifecycleEventType.SERVICE_ORDER_RECEIVED:
        return "order";
      case LifecycleEventType.SERVICE_ORDER_DISPATCHED:
        return "dispatch";
      case LifecycleEventType.SERVICE_EXECUTION:
        return "complete";
      case LifecycleEventType.SERVICE_CANCELLATION:
        return "cancel";
      case LifecycleEventType.SERVICE_INCIDENT_REPORT:
        return "report";
      case LifecycleEventType.ARCHIVE_COMPLETION:
        return "completed";
      case LifecycleEventType.ARCHIVE_RESCINDMENT:
        return "rescinded";
      case LifecycleEventType.ARCHIVE_TERMINATION:
        return "terminated";
      default:
        throw new IllegalArgumentException("Invalid event type!");
    }
  }

  /**
   * Retrieve the priority level of the specified event.
   * 
   * @param event Target event.
   */
  public static int getEventPriority(String event) {
    switch (event) {
      case EVENT_INCIDENT_REPORT:
        return 4;
      case EVENT_CANCELLATION:
        return 3;
      case EVENT_DELIVERY:
        return 2;
      case EVENT_DISPATCH:
        return 1;
      case EVENT_ORDER_RECEIVED:
        return 0;
      default:
        return -1;
    }
  }

  /**
   * Retrieve the expression class associated with the target calculation type.
   * 
   * @param calculationType The target calculation type.
   */
  public static String getExpressionClass(CalculationType calculationType) {
    switch (calculationType) {
      case CalculationType.DIFFERENCE:
        return "https://spec.edmcouncil.org/fibo/ontology/FND/Utilities/Analytics/Difference";
      case CalculationType.TOTAL:
        return "https://www.omg.org/spec/Commons/QuantitiesAndUnits/Total";
      default:
        throw new IllegalArgumentException("Invalid Calculation Type!");
    }
  }

  /**
   * Retrieve the lifecycle resource file path associated with the resource ID.
   * 
   * @param resourceID The identifier for the resource.
   */
  public static String getLifecycleResourceFilePath(String resourceID) {
    switch (resourceID) {
      case LifecycleResource.LIFECYCLE_RESOURCE:
        return FileService.LIFECYCLE_JSON_LD_RESOURCE;
      case LifecycleResource.OCCURRENCE_INSTANT_RESOURCE:
        return FileService.OCCURRENCE_INSTANT_JSON_LD_RESOURCE;
      case LifecycleResource.SCHEDULE_RESOURCE:
        return FileService.SCHEDULE_JSON_LD_RESOURCE;
      case LifecycleResource.PRICING_RESOURCE:
        return FileService.PRICING_JSON_LD_RESOURCE;
      default:
        return null;
    }
  }
}