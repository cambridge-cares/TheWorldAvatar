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
  public static final String STATUS_KEY = "status";
  public static final String REMARKS_KEY = "remarks";
  public static final String SCHEDULE_START_DATE_KEY = "start date";
  public static final String SCHEDULE_END_DATE_KEY = "end date";
  public static final String SCHEDULE_START_TIME_KEY = "start time";
  public static final String SCHEDULE_END_TIME_KEY = "end time";
  public static final String SCHEDULE_TYPE_KEY = "schedule type";

  public static final String LIFECYCLE_STAGE_PREDICATE_PATH = "<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/hasLifecycle>/<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/hasStage>";
  public static final String LIFECYCLE_STAGE_EVENT_PREDICATE_PATH = "<https://www.omg.org/spec/Commons/Collections/comprises>";
  public static final String LIFECYCLE_EVENT_TYPE_PREDICATE_PATH = "<https://www.omg.org/spec/Commons/Classifiers/classifies>";
  public static final String LIFECYCLE_EVENT_PREDICATE_PATH = LIFECYCLE_STAGE_PREDICATE_PATH + "/"
      + LIFECYCLE_STAGE_EVENT_PREDICATE_PATH + "/^" + LIFECYCLE_EVENT_TYPE_PREDICATE_PATH;
  public static final String EVENT_APPROVAL = "https://www.theworldavatar.com/kg/ontoservice/ContractApproval";
  public static final String EVENT_DELIVERY = "https://www.theworldavatar.com/kg/ontoservice/ServiceDeliveryEvent";
  public static final String EVENT_CANCELLATION = "https://www.theworldavatar.com/kg/ontoservice/TerminatedServiceEvent";
  public static final String EVENT_INCIDENT_REPORT = "https://www.theworldavatar.com/kg/ontoservice/IncidentReportEvent";
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
    return LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE);
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
   * Generates a SPARQL query to get the current status of the contract.
   * 
   * @param contractId the target contract id.
   */
  public static String genServiceStatusQuery(String contractId) {
    return genPrefixes()
        + "SELECT DISTINCT ?iri ?status WHERE{"
        + "{SELECT DISTINCT ?iri (MAX(?priority_val) AS ?priority) WHERE{"
        + "?iri a fibo-fnd-pas-pas:ServiceAgreement;"
        + "fibo-fnd-arr-lif:hasLifecycle/fibo-fnd-arr-lif:hasStage/<https://www.omg.org/spec/Commons/Collections/comprises> ?event."
        + "?event_type <https://www.omg.org/spec/Commons/Classifiers/classifies> ?event."
        + "BIND(IF(?event_type=ontoservice:ContractDischarge||?event_type=ontoservice:ContractRescission||?event_type=ontoservice:ContractTermination,"
        + "2,IF(?event_type=ontoservice:ContractApproval,1,0)"
        + ") AS ?priority_val)"
        + "FILTER STRENDS(STR(?iri),\"" + contractId + "\")"
        + "}"
        + "GROUP BY ?iri}"
        + "BIND(IF(?priority=2,\"Archived\","
        + "IF(?priority=1,\"Active\",\"Pending\")"
        + ") AS ?status)"
        + "}";
  }

  /**
   * Generates a SPARQL query to get the schedule of the contract.
   * 
   * @param contractId the target contract id.
   */
  public static String genServiceScheduleQuery(String contractId) {
    return genPrefixes()
        + "SELECT DISTINCT * WHERE{"
        + genScheduleTemplateQuery()
        + "?iri a fibo-fnd-pas-pas:ServiceAgreement."
        // Nested query for all days
        + "{SELECT ?iri "
        + "(MAX(IF(?day=fibo-fnd-dt-fd:Monday,\"Monday\",\"\")) AS ?monday) "
        + "(MAX(IF(?day=fibo-fnd-dt-fd:Tuesday,\"Tuesday\",\"\")) AS ?tuesday) "
        + "(MAX(IF(?day=fibo-fnd-dt-fd:Wednesday,\"Wednesday\",\"\")) AS ?wednesday) "
        + "(MAX(IF(?day=fibo-fnd-dt-fd:Thursday,\"Thursday\",\"\")) AS ?thursday) "
        + "(MAX(IF(?day=fibo-fnd-dt-fd:Friday,\"Friday\",\"\")) AS ?friday) "
        + "(MAX(IF(?day=fibo-fnd-dt-fd:Saturday,\"Saturday\",\"\")) AS ?saturday) "
        + "(MAX(IF(?day=fibo-fnd-dt-fd:Sunday,\"Sunday\",\"\")) AS ?sunday) "
        + "WHERE{?iri " + LIFECYCLE_STAGE_PREDICATE_PATH
        + "/<https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/hasSchedule>/fibo-fnd-dt-fd:hasRecurrenceInterval ?day.}"
        + "GROUP BY ?iri}"
        // WARNING: FedX seems to execute filters at the end and will return inaccurate
        // values otherwise
        + "FILTER STRENDS(STR(?iri),\"" + contractId + "\")"
        + "}";
  }

  /**
   * Generates a SPARQL query to get today's day of week IRI from the cmns-dt
   * ontology.
   */
  public static String genTodayDayOfWeekQuery() {
    return "PREFIX cmns-dt: <https://www.omg.org/spec/Commons/DatesAndTimes/>"
        + "SELECT DISTINCT ?iri WHERE{"
        // Day of week individuals
        + "?iri a cmns-dt:TimeInterval;"
        + "a owl:NamedIndividual;"
        + "rdfs:label ?day_of_week."
        // Calculate the days passed since 2000-01-01, which was Saturday
        + "BIND(NOW()-\"2000-01-01\"^^xsd:date AS ?days_diff)"
        // Map the day of week to their modulus based on 2000-01-01, where 0 represents
        // Saturday and 6 represents Friday
        + "VALUES(?day ?day_of_week){(0 \"Saturday\")(1 \"Sunday\")(2 \"Monday\")(3 \"Tuesday\")(4 \"Wednesday\")(5 \"Thursday\")(6 \"Friday\")}"
        // Match the day of week to the computed modulus of the days passed since
        // 2000-01-01
        + "FILTER(?day=xsd:integer(floor(?days_diff-(7*floor(?days_diff/7)))))"
        + "}";
  }

  /**
   * Generates a query template for human readable schedule details.
   */
  public static String genReadableScheduleQuery() {
    return genScheduleTemplateQuery()
        + "BIND(IF(?recurrence=\"P1D\",\"Single Service\","
        + "IF(?recurrence=\"P2D\",\"Alternate Day Service\", "
        + "CONCAT(\"Regular Service\")"
        + ")" // Close IF statement
        + ") AS ?" + StringResource.parseQueryVariable(SCHEDULE_TYPE_KEY) + ")";
  }

  /**
   * Generates a SPARQL query for retrieving active services that are scheduled
   * for today.
   * 
   * @param dayOfWeekInstance the day of week instance to target.
   */
  public static String genActiveServiceQuery(String dayOfWeekInstance) {
    StringBuilder stageFilters = new StringBuilder();
    // A query line to ensure that the agreement must be approved before any
    // services are delivered
    StringResource.appendTriple(stageFilters, "?iri", LIFECYCLE_EVENT_PREDICATE_PATH,
        StringResource.parseIriForQuery(EVENT_APPROVAL));
    // Generate FILTER NOT EXISTS to ensure no occurrence exist yet for today
    String occurenceFilterQuery = "?stage <https://www.omg.org/spec/Commons/Collections/comprises>/fibo-fnd-dt-oc:hasEventDate ?today.";
    LifecycleResource.appendFilterExists(stageFilters, occurenceFilterQuery, false);
    return genPrefixes()
        + "SELECT DISTINCT ?iri WHERE{"
        + "?iri a fibo-fnd-pas-pas:ServiceAgreement;"
        + "fibo-fnd-arr-lif:hasLifecycle/fibo-fnd-arr-lif:hasStage ?stage."
        + "?stage fibo-fnd-rel-rel:exemplifies <"
        + LifecycleResource.getStageClass(LifecycleEventType.SERVICE_EXECUTION) + ">;"
        + "fibo-fnd-dt-fd:hasSchedule/cmns-dt:hasStartDate/cmns-dt:hasDateValue ?start_date;"
        + "fibo-fnd-dt-fd:hasSchedule/fibo-fnd-dt-fd:hasRecurrenceInterval/cmns-dt:hasDurationValue ?duration."
        + "OPTIONAL{?stage fibo-fnd-dt-fd:hasSchedule/fibo-fnd-dt-fd:hasRecurrenceInterval ?schedule_day.}"
        + "BIND(xsd:date(NOW()) AS ?today)"
        // Calculate the days passed since start date
        + "BIND(xsd:integer(?today-?start_date) AS ?days_from_start)"
        // Filters for services that have commenced
        + "FILTER(?today>=?start_date)"
        // Filters to match today with the scheduled requirements
        // day of week must match for weekly schedules
        + "FILTER((?schedule_day=" + StringResource.parseIriForQuery(dayOfWeekInstance) + "&& ?duration=\"P7D\")"
        + "||(?days_from_start-FLOOR(?days_from_start/2)*2=0 && ?duration=\"P2D\"))" // for alternate day schedules
        + stageFilters.toString()
        + "}";
  }

  /**
   * Generates a SPARQL query for retrieving the active service task for the
   * specified contract and date.
   * 
   * @param contract Target contract iri.
   * @param date     Target date in YYYY-MM-DD format.
   */
  public static String genActiveServiceTaskQuery(String contract, String date) {
    StringBuilder stageFilters = new StringBuilder();
    LifecycleResource.appendFilterExists(stageFilters, true, EVENT_APPROVAL);
    LifecycleResource.appendArchivedFilterExists(stageFilters, false);
    return genPrefixes()
        + "SELECT DISTINCT ?iri ?" + STATUS_KEY + "{"
        + StringResource.parseIriForQuery(contract) + " a fibo-fnd-pas-pas:ServiceAgreement;"
        + "fibo-fnd-arr-lif:hasLifecycle/fibo-fnd-arr-lif:hasStage ?stage."
        + "?stage fibo-fnd-rel-rel:exemplifies <"
        + LifecycleResource.getStageClass(LifecycleEventType.SERVICE_EXECUTION) + ">;"
        + "<https://www.omg.org/spec/Commons/Collections/comprises> ?iri."
        + "?iri fibo-fnd-dt-oc:hasEventDate \"" + date + "\"^^xsd:date."
        + StringResource.parseIriForQuery(EVENT_DELIVERY)
        + " <https://www.omg.org/spec/Commons/Classifiers/classifies> ?iri."
        + "}";
  }

  /**
   * Generates a SPARQL query for retrieving the service tasks for the specified
   * date.
   * 
   * @param date Target date in YYYY-MM-DD format.
   */
  public static String genServiceTasksQuery(String date) {
    StringBuilder stageFilters = new StringBuilder();
    LifecycleResource.appendFilterExists(stageFilters, true, EVENT_APPROVAL);
    LifecycleResource.appendArchivedFilterExists(stageFilters, false);
    return genPrefixes()
        + "SELECT DISTINCT ?iri ?client ?location ?event_date ?" + STATUS_KEY + "{"
        + "?iri a fibo-fnd-pas-pas:ServiceAgreement;"
        + "fibo-fnd-arr-rep:isRequestedBy/cmns-rlcmp:isPlayedBy/cmns-dsg:hasName/<https://spec.edmcouncil.org/fibo/ontology/BE/LegalEntities/LEIEntities/hasTransliteratedName> ?client;"
        + "fibo-fnd-rel-rel:governs/fibo-fnd-rel-rel:provides/fibo-fnd-rel-rel:involves/^ontobim:hasFacility/<https://www.theworldavatar.com/kg/ontoservice/hasServiceLocation>/geo:hasGeometry/geo:asWKT ?location;"
        + "fibo-fnd-arr-lif:hasLifecycle/fibo-fnd-arr-lif:hasStage ?stage."
        + "?stage fibo-fnd-rel-rel:exemplifies <"
        + LifecycleResource.getStageClass(LifecycleEventType.SERVICE_EXECUTION) + ">;"
        + "<https://www.omg.org/spec/Commons/Collections/comprises> ?event."
        + "?event fibo-fnd-dt-oc:hasEventDate ?event_date."
        + "?eventtype <https://www.omg.org/spec/Commons/Classifiers/classifies> ?event."
        + "BIND("
        + "IF(?eventtype=" + StringResource.parseIriForQuery(EVENT_DELIVERY)
        + ",\"In progress\","
        + "IF(?eventtype=" + StringResource.parseIriForQuery(EVENT_CANCELLATION)
        + ",\"Cancelled\","
        + "IF(?eventtype=" + StringResource.parseIriForQuery(EVENT_INCIDENT_REPORT)
        + ",\"Incomplete\""
        + ",\"Unknown\"))) AS ?" + STATUS_KEY + ")"
        + "FILTER(?event_date=\"" + date + "\"^^xsd:date)"
        + "}";
  }

  /**
   * Generates a query template containing prefixes query.
   */
  public static String genPrefixes() {
    return "PREFIX cmns-dt: <https://www.omg.org/spec/Commons/DatesAndTimes/>"
        + "PREFIX cmns-dsg: <https://www.omg.org/spec/Commons/Designators/>"
        + "PREFIX cmns-rlcmp: <https://www.omg.org/spec/Commons/RolesAndCompositions/>"
        + "PREFIX fibo-fnd-arr-id:<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/>"
        + "PREFIX fibo-fnd-plc-adr:<https://spec.edmcouncil.org/fibo/ontology/FND/Places/Addresses/>"
        + "PREFIX fibo-fnd-plc-loc:<https://spec.edmcouncil.org/fibo/ontology/FND/Places/Locations/>"
        + "PREFIX fibo-fnd-arr-lif: <https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/>"
        + "PREFIX fibo-fnd-arr-rep: <https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Reporting/>"
        + "PREFIX fibo-fnd-dt-fd: <https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/>"
        + "PREFIX fibo-fnd-dt-oc: <https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/>"
        + "PREFIX fibo-fnd-rel-rel: <https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/>"
        + "PREFIX fibo-fnd-pas-pas: <https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/ProductsAndServices/>"
        + "PREFIX geo: <http://www.opengis.net/ont/geosparql#>"
        + "PREFIX ontobim: <https://www.theworldavatar.com/kg/ontobim/>"
        + "PREFIX ontoservice: <https://www.theworldavatar.com/kg/ontoservice/>"
        + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>";
  }

  /**
   * Appends a query statement to retrieve the status of an archived contract.
   * 
   * @param query Builder for the query template.
   */
  public static void appendArchivedStateQuery(StringBuilder query) {
    String eventVar = ShaclResource.VARIABLE_MARK + EVENT_KEY;
    StringBuilder tempBuilder = new StringBuilder();
    StringResource.appendTriple(tempBuilder, ShaclResource.VARIABLE_MARK + IRI_KEY,
        LIFECYCLE_STAGE_PREDICATE_PATH + "/" + LIFECYCLE_STAGE_EVENT_PREDICATE_PATH + "/^"
            + LIFECYCLE_EVENT_TYPE_PREDICATE_PATH,
        ShaclResource.VARIABLE_MARK + EVENT_KEY);
    String statement = "BIND("
        + "IF(" + eventVar + "=" + StringResource.parseIriForQuery(EVENT_CONTRACT_COMPLETION)
        + ",\"Completed\","
        + "IF(" + eventVar + "=" + StringResource.parseIriForQuery(EVENT_CONTRACT_RESCISSION)
        + ",\"Rescinded\","
        + "IF(" + eventVar + "=" + StringResource.parseIriForQuery(EVENT_CONTRACT_TERMINATION)
        + ",\"Terminated\""
        + ",\"Unknown\"))) AS ?" + STATUS_KEY
        + ")"
        + "FILTER(?" + STATUS_KEY + "!=\"Unknown\")";
    query.append("{").append(tempBuilder).append("}")
        .append(statement);
  }

  /**
   * Appends FILTER EXISTS or NOT EXISTS statements for an archived contract.
   * 
   * @param query  Builder for the query template.
   * @param exists Indicate if using FILTER EXISTS or FILTER NOT EXISTS.
   */
  public static void appendArchivedFilterExists(StringBuilder query, boolean exists) {
    String stageVar = ShaclResource.VARIABLE_MARK + STAGE_KEY;
    StringBuilder tempBuilder = new StringBuilder();
    StringResource.appendTriple(tempBuilder, ShaclResource.VARIABLE_MARK + IRI_KEY,
        LIFECYCLE_STAGE_PREDICATE_PATH, stageVar);
    StringResource.appendTriple(tempBuilder, stageVar,
        "<https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/exemplifies>",
        StringResource.parseIriForQuery(LifecycleResource.getStageClass(LifecycleEventType.ARCHIVE_COMPLETION)));
    StringResource.appendTriple(tempBuilder, stageVar, LIFECYCLE_STAGE_EVENT_PREDICATE_PATH,
        ShaclResource.VARIABLE_MARK + EVENT_KEY);
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
    StringResource.appendTriple(tempBuilder, "?iri", LIFECYCLE_EVENT_PREDICATE_PATH,
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

  /**
   * Generate a template query for regular schedules.
   */
  private static String genScheduleTemplateQuery() {
    return "?iri " + LIFECYCLE_STAGE_PREDICATE_PATH
        + "/<https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/hasSchedule> ?schedule."
        + "?schedule <https://www.omg.org/spec/Commons/DatesAndTimes/hasStartDate>/<https://www.omg.org/spec/Commons/DatesAndTimes/hasDateValue> ?"
        + StringResource.parseQueryVariable(SCHEDULE_START_DATE_KEY) + ";"
        + "^<https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/hasSchedule>/<https://www.omg.org/spec/Commons/PartiesAndSituations/holdsDuring>/<https://www.omg.org/spec/Commons/DatesAndTimes/hasEndDate>/<https://www.omg.org/spec/Commons/DatesAndTimes/hasDateValue> ?"
        + StringResource.parseQueryVariable(SCHEDULE_END_DATE_KEY) + ";"
        + "<https://www.omg.org/spec/Commons/DatesAndTimes/hasTimePeriod>/<https://www.omg.org/spec/Commons/DatesAndTimes/hasStart>/<https://www.omg.org/spec/Commons/DatesAndTimes/hasTimeValue> ?"
        + StringResource.parseQueryVariable(SCHEDULE_START_TIME_KEY) + ";"
        + "<https://www.omg.org/spec/Commons/DatesAndTimes/hasTimePeriod>/<https://www.omg.org/spec/Commons/DatesAndTimes/hasEndTime>/<https://www.omg.org/spec/Commons/DatesAndTimes/hasTimeValue> ?"
        + StringResource.parseQueryVariable(SCHEDULE_END_TIME_KEY) + ";"
        + "<https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/hasRecurrenceInterval>/<https://www.omg.org/spec/Commons/DatesAndTimes/hasDurationValue> ?recurrence.";
  }
}