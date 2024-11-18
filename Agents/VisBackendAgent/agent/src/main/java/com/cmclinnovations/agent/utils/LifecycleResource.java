package com.cmclinnovations.agent.utils;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import com.cmclinnovations.agent.model.type.LifecycleEventType;
import com.cmclinnovations.agent.service.FileService;

public class LifecycleResource {
  public static final String LIFECYCLE_RESOURCE = "lifecycle";
  public static final String SCHEDULE_RESOURCE = "schedule";
  public static final String OCCURRENCE_INSTANT_RESOURCE = "occurrence instant";

  public static final String IRI_KEY = "iri";
  public static final String CONTRACT_KEY = "contract";
  public static final String CURRENT_DATE_KEY = "current date";
  public static final String DATE_KEY = "date";
  public static final String EVENT_KEY = "event";
  public static final String STAGE_KEY = "stage";

  public static final String LIFECYCLE_EVENT_PREDICATE_PATH = "<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/hasLifecycle>/<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/hasStage>/<https://www.omg.org/spec/Commons/Collections/comprises>/^<https://www.omg.org/spec/Commons/Classifiers/classifies>";
  public static final String EVENT_APPROVAL = "https://www.theworldavatar.com/kg/ontoservice/ContractApproval";
  public static final String EVENT_DELIVERY = "https://www.theworldavatar.com/kg/ontoservice/ServiceDeliveryEvent";
  public static final String EVENT_CANCELLATION = "https://www.theworldavatar.com/kg/ontoservice/TerminatedServiceEvent";
  public static final String EVENT_MIS_REPORT = "https://www.theworldavatar.com/kg/ontoservice/MissedServiceEvent";
  public static final String EVENT_CONTRACT_COMPLETION = "https://www.theworldavatar.com/kg/ontoservice/ContractDischarge";
  public static final String EVENT_CONTRACT_RESCISSION = "https://www.theworldavatar.com/kg/ontoservice/ContractRescission";
  public static final String EVENT_CONTRACT_TERMINATION = "https://www.theworldavatar.com/kg/ontoservice/ContractTermination";

  // Private constructor to prevent instantiation
  private LifecycleResource() {
    throw new UnsupportedOperationException("This class cannot be instantiated!");
  }

  /**
   * Get current date in YYYY-MM-DD format.
   */
  public static String getCurrentDate() {
    // Define the date format
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    return LocalDate.now().format(formatter);
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
      case LifecycleEventType.SERVICE_EXECUTION:
      case LifecycleEventType.SERVICE_CANCELLATION:
      case LifecycleEventType.SERVICE_MISS_REPORT:
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
      case LifecycleEventType.SERVICE_EXECUTION:
        return EVENT_DELIVERY;
      case LifecycleEventType.SERVICE_CANCELLATION:
        return EVENT_CANCELLATION;
      case LifecycleEventType.SERVICE_MISS_REPORT:
        return EVENT_MIS_REPORT;
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
      case LifecycleEventType.SERVICE_EXECUTION:
        return "complete";
      case LifecycleEventType.SERVICE_CANCELLATION:
        return "cancel";
      case LifecycleEventType.SERVICE_MISS_REPORT:
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
      default:
        return null;
    }
  }

  /**
   * Appends FILTER EXISTS or NOT EXISTS statements for an archived contract.
   * 
   * @param query  Builder for the query template.
   * @param exists Indicate if using FILTER EXISTS or FILTER NOT EXISTS.
   */
  public static void appendArchivedFilterExists(StringBuilder query, boolean exists) {
    StringBuilder tempBuilder = new StringBuilder();
    tempBuilder.append("{");
    StringResource.appendTriple(tempBuilder, "?iri", LifecycleResource.LIFECYCLE_EVENT_PREDICATE_PATH,
        StringResource.parseIriForQuery(LifecycleResource.EVENT_CONTRACT_COMPLETION));
    tempBuilder.append(ShaclResource.UNION_OPERATOR);
    StringResource.appendTriple(tempBuilder, "?iri", LifecycleResource.LIFECYCLE_EVENT_PREDICATE_PATH,
        StringResource.parseIriForQuery(LifecycleResource.EVENT_CONTRACT_RESCISSION));
    tempBuilder.append(ShaclResource.UNION_OPERATOR);
    StringResource.appendTriple(tempBuilder, "?iri", LifecycleResource.LIFECYCLE_EVENT_PREDICATE_PATH,
        StringResource.parseIriForQuery(LifecycleResource.EVENT_CONTRACT_TERMINATION));
    tempBuilder.append("}");
    LifecycleResource.appendFilterExists(query, tempBuilder.toString(), exists);
  }

  /**
   * Appends FILTER EXISTS or NOT EXISTS statements for the specified object
   * instance.
   * 
   * @param query    Builder for the query template.
   * @param exists   Indicate if using FILTER EXISTS or FILTER NOT EXISTS.
   * @param instance Target IRI instance. Typically the object in a triple.
   */
  public static void appendFilterExists(StringBuilder query, boolean exists, String instance) {
    StringBuilder tempBuilder = new StringBuilder();
    StringResource.appendTriple(tempBuilder, "?iri", LifecycleResource.LIFECYCLE_EVENT_PREDICATE_PATH,
        StringResource.parseIriForQuery(instance));
    LifecycleResource.appendFilterExists(query, tempBuilder.toString(), exists);
  }

  /**
   * Appends FILTER EXISTS or NOT EXISTS statements for lifecycles.
   * 
   * @param query    Builder for the query template.
   * @param contents Contents for the clause.
   * @param exists   Indicate if using FILTER EXISTS or FILTER NOT EXISTS.
   */
  private static void appendFilterExists(StringBuilder query, String contents, boolean exists) {
    query.append("FILTER").append(ShaclResource.WHITE_SPACE);
    // Add NOT parameter if this filter should not exist
    if (!exists) {
      query.append("NOT").append(ShaclResource.WHITE_SPACE);
    }
    query.append("EXISTS{").append(contents).append("}");
  }
}